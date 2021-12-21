package day18

import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxOptionId
import day18.Solution.SnailfishNumber.fromBuffer
import day18.Utils._
import helpers.Buffer
import cats.implicits._

//https://adventofcode.com/2021/day/18
object Solution {

  object SnailfishNumber {
    def fromBuffer(buffer: Buffer): IO[(Buffer, SnailfishNumber)] =
      buffer.scan(1) match {
        case "[" => readPair(buffer)
        case _   => readNumber(buffer)
      }

    def readNumber(buffer: Buffer) =
      for {
        (buffer, value) <- buffer.take(1)
        buffer <- buffer.consume(1) //in [1,1] consumes ','
      } yield (buffer, RegularNumber(value.toInt))

    def readPair(buffer: Buffer): IO[(Buffer, Pair)] = for {
      buffer <- buffer.consume(1) //in [1,1] consumes '['
      (buffer, p1) <- fromBuffer(buffer)
      (buffer, p2) <- fromBuffer(buffer)
      buffer <- buffer.consume(1) //in [1,1] consumes ']'
    } yield (buffer, Pair(p1, p2))
  }

  def toBinaryString(value: Int) =
    String.format("%4s", value.toBinaryString).replace(' ', '0')

  def levels4 = List(
    "0000",
    "0001",
    "0010",
    "0011",
    "0100",
    "0101",
    "0110",
    "0111",
    "1000",
    "1001",
    "1010",
    "1011",
    "1100",
    "1101",
    "1110",
    "1111"
  )

  def allLevels: Seq[String] = List(
    "0",
    "00",
    "000",
    "0000",
    "0001",
    "001",
    "01",
    "010",
    "0010",
    "0011",
    "0100",
    "0101",
    "011",
    "0110",
    "0111",
    "1",
    "10",
    "100",
    "1000",
    "1001",
    "101",
    "11",
    "110",
    "1010",
    "1011",
    "1100",
    "1101",
    "111",
    "1110",
    "1111"
  )

  sealed trait SnailfishNumber {
    def reduce: SnailfishNumber
    def get(depth: String): Option[SnailfishNumber]
    def findFirstToExplode: Option[(String, Pair)]
    def magnitude: Int
  }

  case class RegularNumber(value: Int) extends SnailfishNumber {
    override def toString: String = value.toString
    def get(depth: String): Option[SnailfishNumber] = None
    def findFirstToExplode: Option[(String, Pair)] = None
    def reduce: RegularNumber = this
    def +(add: Int) = copy(value = value + add)
    def magnitude = value
  }
  case class Pair(p1: SnailfishNumber, p2: SnailfishNumber)
      extends SnailfishNumber {
    override def toString = s"[${p1.toString}, ${p2.toString}]"

    def magnitude: Int = p1.magnitude * 3 + p2.magnitude * 2

    def +(value: SnailfishNumber) = Pair(
      p1 = Pair(p1, p2),
      p2 = value
    )

    def replace(depth: String, sfNumber: SnailfishNumber) = {
      def loop(depth: List[Char], result: Pair): Pair = {
        depth match {
          case first :: Nil =>
            first match {
              case '0' => result.copy(p1 = sfNumber)
              case '1' => result.copy(p2 = sfNumber)
            }
          case first :: tail =>
            first match {
              case '0' =>
                result.p1 match {
                  case p: Pair => result.copy(p1 = loop(tail, p))
                  case _       => result
                }
              case '1' =>
                result.p2 match {
                  case p: Pair => result.copy(p2 = loop(tail, p))
                  case _       => result
                }
            }
          case Nil => result
        }
      }

      loop(depth.toList, this)
    }

    def addToFirstRegular(left: Boolean, value: Int, depth: String) = {
      def loop(depth: List[Char], result: Pair): Pair = {
        depth match {
          case first :: tail =>
            first match {
              case '0' =>
                result.p1 match {
                  case p: Pair          => result.copy(p1 = loop(tail, p))
                  case r: RegularNumber => result.copy(p1 = r + value)
                }
              case '1' =>
                result.p2 match {
                  case p: Pair          => result.copy(p2 = loop(tail, p))
                  case r: RegularNumber => result.copy(p2 = r + value)
                }
            }
          case Nil =>
            if (left)
              result.p2 match {
                case r: RegularNumber => result.copy(p2 = r + value)
                // dig until we find first regular number
                case p: Pair => result.copy(p2 = loop(List('0'), p))
              }
            else
              result.p1 match {
                case r: RegularNumber => result.copy(p1 = r + value)
                // dig until we find first regular number
                case p: Pair => result.copy(p1 = loop(List('1'), p))
              }
        }
      }

      loop(depth.toList, this)
    }

    def explode = (for {
      (depth, pair) <- findFirstToExplode
      (left, right) = (
        toBinaryString(Integer.parseInt(depth, 2) - 1),
        toBinaryString(Integer.parseInt(depth, 2) + 1)
      )
      replaced = replace(depth, RegularNumber(0))
      leftR = pair.p1 match {
        case RegularNumber(value) if depth != "0000" =>
          replaced.addToFirstRegular(true, value, left)
        case _ => replaced
      }
      rightR = pair.p2 match {
        case RegularNumber(value) if depth != "1111" =>
          leftR.addToFirstRegular(false, value, right)
        case _ => leftR
      }
    } yield rightR).getOrElse(this)

    def split = (for {
      (depth, regNumber) <- findFirstToSplit
    } yield replace(
      depth,
      Pair(
        RegularNumber(Math.floor(regNumber.value.toDouble / 2).toInt),
        RegularNumber(Math.ceil(regNumber.value.toDouble / 2).toInt)
      )
    )).getOrElse(this)

    def reduce: Pair = {
      def loop(pair: Pair): Pair = {
        val exploded = pair.explode
        if (exploded != pair) loop(exploded)
        else {
          val splitted = pair.split
          if (splitted != pair) loop(splitted)
          else pair
        }
      }

      loop(this)
    }

    def findFirstToExplode: Option[(String, Pair)] =
      levels4.map { depth => (depth, get(depth)) }.collectFirst {
        case (depth, Some(pair: Pair)) => (depth, pair)
      }

    def findFirstToSplit: Option[(String, RegularNumber)] =
      allLevels
        .map { depth => (depth, get(depth)) }
        .collectFirst {
          case (depth, Some(r: RegularNumber)) if r.value > 9 =>
            (depth, r)
        }

    def get(depth: String): Option[SnailfishNumber] =
      depth.foldLeft[Option[SnailfishNumber]](this.some) {
        case (Some(acc), depth) =>
          acc match {
            case d: Pair =>
              depth match {
                case '0' => d.p1.some
                case '1' => d.p2.some
              }
            case _: RegularNumber => None
          }
        case (None, _) => None
      }
  }

  def numbers(buffers: List[Buffer]) = (for {
    buffer <- buffers
  } yield fromBuffer(buffer).map { case (_, n: Pair) => n }).sequence

  def part1(numbers: List[Pair]): Int =
    numbers.reduce[Pair] { case (n1, n2) => (n1 + n2).reduce }.magnitude

  def part2(numbers: List[Pair]) = (for {
    n1 <- numbers
    i <- numbers.indices
    n2 = numbers(i)
    if n1 != n2
  } yield Math.max(
    (n1 + n2).reduce.magnitude,
    (n2 + n1).reduce.magnitude
  )).max

  val program = for {
    input <- readLines("day18/input.txt")
    //input <- readLines("day18/input_example.txt")
    buffers = input.map(Buffer)

    sfNumbers <- numbers(buffers)

    _ <- printAny { part1(sfNumbers) }
    _ <- printAny { part2(sfNumbers) }

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
