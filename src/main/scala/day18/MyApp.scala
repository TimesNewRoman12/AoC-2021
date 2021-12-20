package day18

import cats.effect.{IO, IOApp}
import cats.implicits.catsSyntaxOptionId
import day18.Solution.SnailfishNumber.fromBuffer
import day18.Utils._
import helpers.Buffer

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

  def fourthPairs = List(
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

  sealed trait SnailfishNumber {
    //def reduce: SnailfishNumber
    //def atDepth(n: Int): Option[SnailfishNumber]
    def get(depth: String): Option[SnailfishNumber]
    def findFirstToExplode: Option[(String, Pair)]
  }

  case class RegularNumber(value: Int) extends SnailfishNumber {
    override def toString: String = value.toString
    def get(depth: String): Option[SnailfishNumber] = None
    def findFirstToExplode: Option[(String, Pair)] = None
    //def atDepth(n: Int): Option[SnailfishNumber] = None
    //def reduce: RegularNumber = this
  }
  case class Pair(p1: SnailfishNumber, p2: SnailfishNumber)
      extends SnailfishNumber {
    override def toString = s"[${p1.toString}, ${p2.toString}]"

    def findFirstToExplode: Option[(String, Pair)] = {
      def firstRegularNumber(depth: String) = {
        val (_, found) = depth
          .foldLeft[(SnailfishNumber, Option[RegularNumber])]((this, None)) {
            case ((acc, None), depth) =>
              acc match {
                case d: Pair =>
                  depth match {
                    case '0' => (d.p1, None)
                    case '1' => (d.p2, None)
                  }
                case r: RegularNumber => (acc, r.some)
              }
            case ((acc, regularNumber), depth) => (acc, regularNumber)
          }
        found
      }

      val res = {
        fourthPairs.map { depth => (depth, get(depth)) }.collectFirst {
          case (depth, Some(pair: Pair)) => (depth, pair)
        }
      }
      val res2 = for {
        (depth, pair) <- res
        (left, right) = (
          toBinaryString(Integer.parseInt(depth, 2) - 1),
          toBinaryString(Integer.parseInt(depth, 2) + 1)
        )
        _ = println(s"left: $left, right: $right")
        /*
        replacement =
          if (depth.drop(3) == "0") Pair(RegularNumber(0), pair.p2)
          else Pair(pair.p1, RegularNumber(0))
        rr = firstRegularNumber(right)
        ll = firstRegularNumber(left)
        _ = println(rr)

         */
      } yield ()



      println(res2)

      res
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
    //def reduce = ???
  }

  val program = for {
    //input <- readLines("day18/input.txt")
    input <- readLines("day18/input_example.txt")
    buffers = input.map(Buffer)

    (buffer, number) <- fromBuffer(buffers.head)

    _ <- printAny { number }
    //_ <- printAny { number.get("0111") }
    // [[[[[9,8],1],2],3],4]
    //_ <- printAny { number.get("0000") }
    //_ <- printAny { number.get("0001") }
    _ <- printAny { number.findFirstToExplode }

    // [[3, [2, [[7, 3], 1]]], [6, [5, [4, [3, 2]]]]]
    //_ <- printAny { number.get("0110") }
    //_ <- printAny { number.get("0111") }
    //_ <- printAny { number.get("010") } // 0101
    // [[3, [2, [1, [7, 3]]]], [6, [5, [4, [3, 2]]]]]
    //_ <- printAny { number.get("0111") }
    //_ <- printAny { number.get("0110") }
    //_ <- printAny { number.get("1") } // 1000

    //_ <- printAny { number.get("1") }

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
