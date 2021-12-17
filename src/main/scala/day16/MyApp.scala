package day16

import cats.effect.{IO, IOApp}
import day16.Solution.Packet.{LITERAL, reads}
import day16.Utils._

import scala.annotation.tailrec

//https://adventofcode.com/2021/day/16
object Solution {

  def binaryToDecimal(value: String) = Integer.parseInt(value, 2)

  case class Buffer(value: String) {
    def isEmpty = value.isEmpty
    def length = value.length

    def consume(n: Int): IO[Buffer] = IO.pure { Buffer(value.drop(n)) }
    def take(n: Int): IO[(Buffer, String)] = IO.pure {
      (Buffer(value.drop(n)), value.take(n))
    }

    def scan(from: Int, until: Int): String = value.slice(from, until)
  }

  object Packet {
    val LITERAL: Int = 4

    def readLiteral(buffer: Buffer): IO[(Buffer, Literal)] = {
      def readContent(
          buffer: Buffer,
          contents: List[String] = List.empty
      ): IO[(Buffer, List[String])] = for {
        taken <- buffer.take(1)
        (buffer, keepReading) = taken
        taken <- buffer.take(4)
        (buffer, content) = taken
        taken <-
          if (keepReading == "1") readContent(buffer, contents :+ content)
          else IO.pure { (buffer, contents :+ content) }
        (buffer, contents) = taken
      } yield (buffer, contents)

      for {
        taken <- buffer.take(3)
        (buffer, version) = taken
        buffer <- buffer.consume(3) //id for literal is 4
        taken <- readContent(buffer)
        (buffer, contents) = taken
      } yield (buffer, Literal(binaryToDecimal(version), contents))
    }

    def readOperator(buffer: Buffer): IO[(Buffer, Operator)] = {
      for {
        taken <- buffer.take(3)
        (buffer, version) = taken
        taken <- buffer.take(3)
        (buffer, id) = taken
        taken <- buffer.take(1)
        (buffer, lengthId) = taken
        taken <-
          if (lengthId == "0") {
            for {
              taken <- buffer.take(15)
              (buffer, length) = taken
            } yield readLength(buffer, binaryToDecimal(length))
          } else {
            for {
              taken <- buffer.take(11)
              (buffer, count) = taken
            } yield readCount(buffer, binaryToDecimal(count))
          }
        taken <- taken
        (buffer, packets) = taken
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
        taken <- buffer.take(length)
        (buffer, content) = taken
        taken <- reads(Buffer(content))
        (subBuffer, packet) = taken
        taken <-
          if (subBuffer.isEmpty) IO { (buffer, packets :+ packet) }
          else readLength(subBuffer, subBuffer.length, packets :+ packet)
        (_, packets) =
          taken // this sub-buffer is going to get depleted and is not used further
      } yield (buffer, packets)
    }

    def readCount(
        buffer: Buffer,
        count: Int,
        packets: List[Packet] = List.empty
    ): IO[(Buffer, List[Packet])] = {
      require(count > 0)
      for {
        taken <- reads(buffer)
        (buffer, packet) = taken
        newCount = count - 1
        taken <-
          if (newCount == 0) IO { (buffer, packets :+ packet) }
          else readCount(buffer, newCount, packets :+ packet)
        (buffer, packets) = taken
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

  sealed trait Packet {
    def version: Int
    def id: Int
  }

  case class Literal(version: Int, content: List[String]) extends Packet {
    val id: Int = LITERAL
    def value: Long = BigInt(content.mkString, 2).longValue
    override def toString = value.toString
  }

  case object Operator {
    def dummy: Operator = Operator(0, 0, List.empty)
  }

  case class Operator(version: Int, id: Int, packets: List[Packet])
      extends Packet

  val program = for {
    input <- readHex("day16/input.txt")
    //input <- readHex("day16/input_example.txt")
    //input <- readHex("day16/test.txt")

    result <- reads(Buffer(input))

    (buffer, packet) = result

    _ <- printAny { packet }
    _ <- printAny { s"part1: ${versionSum(packet)}" }

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
