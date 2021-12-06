package day5

import cats.effect.IOApp
import day5.Utils._

object Solution {
  val RequiredCoverage = 2
  type Coverage = Int

  def points(lines: List[Line]): Map[Point, Coverage] =
    lines
      .flatMap(_.points)
      .groupBy(identity)
      .view
      .mapValues(_.length)
      .toMap

  val program = for {
    lines <- readNumbers("day5/input.txt")

    task1Lines = lines.filter(_.isStraight)
    task2Lines = lines.filter(line => line.isStraight || line.is45Diagonal)

    _ <- printAny(points(task1Lines).values.count(_ >= RequiredCoverage))
    _ <- printAny(points(task2Lines).values.count(_ >= RequiredCoverage))
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}

case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
  def points: List[Point] = {
    val byX: Int = if (x1 > x2) -1 else 1
    val byY: Int = if (y1 > y2) -1 else 1

    def xRange = x1 to x2 by byX
    def yRange = y1 to y2 by byY

    (for {
      (x, y) <- xRange.zipAll(yRange, xRange.head, yRange.head)
    } yield Point(x, y)).toList
  }

  val xShift = Math.abs(x1 - x2)
  val yShift = Math.abs(y1 - y2)

  def is45Diagonal = xShift == yShift && points.size == Math.min(xShift, yShift) + 1
  def isStraight = x1 == x2 || y1 == y2
}

case class Point(x: Int, y: Int)
