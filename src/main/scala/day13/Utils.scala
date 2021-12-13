package day13

import cats.effect.IO
import day13.Solution.{Axis, Coords, Folds}
import helpers.Matrix.{X, Y}

import scala.io.Source

object Utils {
  def parse(input: List[String]): IO[(Coords, Folds)] = IO.pure {
    def loop(
        lines: List[String],
        coords: List[(X, Y)] = List.empty,
        folds: List[(Axis, Int)] = List.empty
    ): (Coords, Folds) = {
      lines match {
        case head :: tail =>
          if (head.contains(',')) {
            val xy = head.split(',').map(_.toInt).toList
            xy match {
              case x :: y :: Nil => loop(tail, coords :+ (x, y))
              case _             => throw new Error("failed to parse coords")
            }
          } else if (head.contains("fold along ")) {
            val replaced = head.replace("fold along ", "")
            val fold = replaced.split('=').toList match {
              case head :: tail :: Nil => (head.charAt(0), tail.toInt)
              case _             => throw new Error("failed to parse fold instruction")
            }
            loop(tail, coords, folds :+ fold)
          } else
            loop(tail, coords, folds)
        case Nil => (coords, folds)
      }
    }
    loop(input)
  }

  def printMap(map: Map[(X, Y), Char]): IO[Unit] = IO {
    val minX = map.keys.map(_._1).min
    val minY = map.keys.map(_._2).min
    val maxX = map.keys.map(_._1).max
    val maxY = map.keys.map(_._2).max

    //println(s"left top: ($minX, $minY)")

   for {
      y <- minY to maxY
      x <- minX to maxX
    } {
      val value = map.getOrElse((x, y), '.')
      print(value)
      if (x == maxX) println
    }
  }

  def readNumbers(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList

    source.close()
    lines
  }

  def printAny(any: Any) = IO { println(any) }
}
