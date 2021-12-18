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

  sealed trait SnailfishNumber

  case class RegularNumber(value: Int) extends SnailfishNumber {
    override def toString: String = value.toString
  }
  case class Pair(p1: SnailfishNumber, p2: SnailfishNumber)
      extends SnailfishNumber {
    override def toString = s"[${p1.toString}, ${p2.toString}]"
  }

  val program = for {
    //input <- readLines("day18/input.txt")
    input <- readLines("day18/input_example.txt")
    buffers = input.map(Buffer)

    number <- fromBuffer(buffers.head)

    _ <- printAny { number }

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
