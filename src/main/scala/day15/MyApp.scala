package day15

import cats.effect.IOApp
import day15.Utils._
import helpers.Matrix
import helpers.Matrix.{X, Y}

import scala.annotation.tailrec

//https://adventofcode.com/2021/day/15
object Solution {

  case class Area(risk: Int, totalRisk: Int = Int.MaxValue) {
    override def toString = "|" + totalRisk.toString + "|"
    def setRisk(totalRisk: Int) = copy(totalRisk = totalRisk)
    def asEntry = copy(risk = 0, totalRisk = 0)
  }

  def part2(input: List[List[Int]]) = {
    def enlarge(input: List[List[Int]], times: Int) = (for {
      time <- 0 until times
      length <- 0 to input.size
      row <- input.lift(length)
    } yield {
      def wrap(value: Int): Int = if (value > 9) value % 9 else value
      def copy(list: List[Int]) = {
        for {
          time2 <- 0 until times
        } yield list.map(v => wrap(v + time + time2))
      }.flatten.toList
      copy(row)
    }).toList

    val enlarged = enlarge(input, 5)
    val matrix = Matrix(enlarged).map(Area(_))

    val weighted = calculateWeight(matrix)

    weighted.last.map(_.totalRisk)
  }

  //TODO: less brute force solution
  @tailrec
  def calculateWeight(matrix: Matrix[Area]): Matrix[Area] = {

    val matrixWithEntry = matrix.mapAt(0, 0, _.asEntry)
    val res = matrixWithEntry.xy.foldLeft(matrixWithEntry) { case (m, (x, y)) =>
      val withNeighbors =
        m.hvNeighboursMapped(x, y).foldLeft(m) { case (m, ((xx, yy), nArea)) =>
          val mainAreaRisk = m.get(x, y).map(_.totalRisk).getOrElse(-199)
          val totalRisk = mainAreaRisk + nArea.risk
          if (totalRisk < nArea.totalRisk) m.mapAt(xx, yy, _.setRisk(totalRisk))
          else m
        }

      withNeighbors
    }

    if (res == matrixWithEntry) res
    else calculateWeight(res)
  }

  val program = for {
    input <- readNumbers("day15/input.txt")
    //input <- readNumbers("day15/input_example.txt")
    //input <- readNumbers("day15/test.txt")

    matrix = Matrix(input).map(Area(_))
    part1 = calculateWeight(matrix).last.map(_.totalRisk)

    _ = println(part1.last)

    _ <- printAny{ part2(input) }
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
