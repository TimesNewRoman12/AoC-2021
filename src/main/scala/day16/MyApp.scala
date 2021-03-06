package day16

import cats.effect.{IO, IOApp}
import day16.Solution.Packet._
import day16.Utils._
import helpers.Buffer

//https://adventofcode.com/2021/day/16
object Solution {

  def binaryToDecimal(value: String) = Integer.parseInt(value, 2)

  object Packet {
    val LITERAL: Int = 4
    val SUM: Int = 0
    val PRODUCT: Int = 1
    val MIN: Int = 2
    val MAX: Int = 3
    val GREATER_THAN: Int = 5
    val LESS_THAN: Int = 6
    val EQUAL: Int = 7

    def readLiteral(buffer: Buffer): IO[(Buffer, Literal)] = {
      def readContent(
          buffer: Buffer,
          contents: List[String] = List.empty
      ): IO[(Buffer, List[String])] = for {
        (buffer, keepReading) <- buffer.take(1)
        (buffer, content) <- buffer.take(4)
        (buffer, contents) <-
          if (keepReading == "1") readContent(buffer, contents :+ content)
          else IO.pure { (buffer, contents :+ content) }
      } yield (buffer, contents)

      for {
        (buffer, version) <- buffer.take(3)
        buffer <- buffer.consume(3) //id for literal is 4
        (buffer, contents) <- readContent(buffer)
      } yield (buffer, Literal(binaryToDecimal(version), contents))
    }

    def readOperator(buffer: Buffer): IO[(Buffer, Operator)] = {
      for {
        (buffer, version) <- buffer.take(3)
        (buffer, id) <- buffer.take(3)
        (buffer, lengthId) <- buffer.take(1)
        taken <-
          if (lengthId == "0") {
            for {
              (buffer, length) <- buffer.take(15)
            } yield readLength(buffer, binaryToDecimal(length))
          } else {
            for {
              (buffer, count) <- buffer.take(11)
            } yield readCount(buffer, binaryToDecimal(count))
          }
        (buffer, packets) <- taken
      } yield (
        buffer,
        Operator(binaryToDecimal(version), binaryToDecimal(id), packets)
      )
    }

    def readLength(
        buffer: Buffer,
        length: Int,
        packets: List[Packet] = List.empty
    ): IO[(Buffer, List[Packet])] = {
      require(length > 0)
      for {
        (buffer, content) <- buffer.take(length)
        (subBuffer, packet) <- reads(Buffer(content))
        // this sub-buffer is going to get depleted and is not used further
        (_, packets) <-
          if (subBuffer.isEmpty) IO { (buffer, packets :+ packet) }
          else readLength(subBuffer, subBuffer.length, packets :+ packet)

      } yield (buffer, packets)
    }

    def readCount(
        buffer: Buffer,
        count: Int,
        packets: List[Packet] = List.empty
    ): IO[(Buffer, List[Packet])] = {
      require(count > 0)
      for {
        (buffer, packet) <- reads(buffer)
        newCount = count - 1
        (buffer, packets) <-
          if (newCount == 0) IO { (buffer, packets :+ packet) }
          else readCount(buffer, newCount, packets :+ packet)
      } yield (buffer, packets)
    }

    def reads(buffer: Buffer): IO[(Buffer, Packet)] = {
      val id = binaryToDecimal(buffer.scan(3, 6))

      for {
        result <- id match {
          case LITERAL => readLiteral(buffer)
          case _       => readOperator(buffer)
        }
      } yield result
    }
  }

  def versionSum(packet: Packet): Int = {
    def loop(packet: Packet, sum: Int = 0): Int = {
      packet match {
        case o: Operator =>
          o.packets.foldLeft(sum) { case (acc, packet) =>
            loop(packet, acc)
          } + o.version
        case l: Literal =>
          sum + l.version
      }
    }

    loop(packet)
  }

  sealed trait Packet { self: Packet =>
    def version: Int
    def id: Int
    def value: Long
  }

  case class Literal(version: Int, content: List[String]) extends Packet {
    val id: Int = LITERAL
    def value: Long = BigInt(content.mkString, 2).longValue
    override def toString = value.toString
  }

  case class Operator(version: Int, id: Int, packets: List[Packet])
      extends Packet {
    def value: Long = id match {
      case SUM     => packets.map(_.value).sum
      case PRODUCT => packets.map(_.value).product
      case MIN     => packets.map(_.value).min
      case MAX     => packets.map(_.value).max
      // is there any wy of passing operator >/</== as a function
      case GREATER_THAN =>
        packets match {
          case first :: second :: Nil =>
            if (first.value > second.value) 1L else 0L
          case _ =>
            throw new Error("GREATER_THAN should have exactly two sub packets")
        }
      case LESS_THAN =>
        packets match {
          case first :: second :: Nil =>
            if (first.value < second.value) 1L else 0L
          case _ =>
            throw new Error("GREATER_THAN should have exactly two sub packets")
        }
      case EQUAL =>
        packets match {
          case first :: second :: Nil =>
            if (first.value == second.value) 1L else 0L
          case _ =>
            throw new Error("GREATER_THAN should have exactly two sub packets")
        }
    }
  }

  val program = for {
    input <- readHex("day16/input.txt")
    //input <- readHex("day16/input_example.txt")
    //input <- readHex("day16/test.txt")
    result <- reads(Buffer(input))

    (buffer, packet) = result

    _ <- printAny { packet }
    _ <- printAny { s"part1: ${versionSum(packet)}" }

    _ <- printAny { s"part2: ${packet.value}" }
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
