package day3

import cats.effect.{IO, IOApp}
import day3.Utils.{inputLines, printAny, toDecimal}

import scala.io.Source

//https://adventofcode.com/2021/day/3
object MyApp extends IOApp.Simple {
  val run = Solution1.program *> Solution2.program
}

object Utils {
  def inputLines: IO[List[String]] = IO {
    //val source = Source.fromResource("day3/input_example.txt")
    val source = Source.fromResource("day3/input.txt")
    source.getLines().toList
  }

  def toDecimal(s: String) = Integer.parseInt(s, 2)
  def printAny(any: Any) = IO { println(any) }
}

object Solution2 {
  def go(list: List[String]) = IO {

    def pp(charAt: Int, list: List[String], max: Boolean): Int = {
      val char = if (max) '1' else '0'
      val (l1, l2) = list.partition { e => e.charAt(charAt) == char }
      val resultList = if (max) {
        if (l1.size > l2.size) l1
        else if (l1.size < l2.size) l2
        else l1
      } else {
        if (l2.size < l1.size) l2
        else if (l1.size < l2.size) l1
        else l1
      }

      list match {
        case _ :: _ :: Nil => toDecimal(l1.head)
        case xs :: Nil     => toDecimal(xs)
        case _             => pp(charAt + 1, resultList, max)
      }
    }

    val (l1, l2) = list.partition { e => e.charAt(0) == '1' }

    val (oxygen, co2) =
      if (l1.size >= l2.size)
        (
          pp(1, l1, true),
          pp(1, l2, false)
        )
      else
        (
          pp(1, l2, true),
          pp(1, l1, false)
        )

    oxygen * co2
  }

  val program = for {
    inputLines <- inputLines
    result <- go(inputLines)
    _ <- printAny(s"2: $result")
  } yield ()
}

object Solution1 {

  val program = for {
    inputLines <- inputLines
    firstChars <- firstChars(inputLines)
    result <- result(firstChars)
    _ <- printAny(s"1: $result")
  } yield ()

  def firstChars(inputLines: List[String]) = IO.pure {
    val lineLength = inputLines.head.length
    val res = (for {
      x <- 0 until lineLength
      line <- inputLines
    } yield line.charAt(x)).toList

    res.sliding(lineLength, inputLines.length).toList
  }

  def result(firstChars: List[List[Char]]) = IO.pure {
    val filtered = for {
      line <- firstChars
    } yield
      if (line.count(_ == '0') > line.count(_ == '1')) ("0", "1")
      else ("1", "0")

    loop(filtered)
  }

  def loop(
      r: Seq[(String, String)],
      gamma: String = "",
      epsilon: String = ""
  ): Int =
    r match {
      case (g1, e1) :: tail => loop(tail, gamma + g1, epsilon + e1)
      case Nil              => toDecimal(gamma) * toDecimal(epsilon)
    }
}
