package day9

import cats.effect.IOApp
import day9.Utils._
import helpers.Matrix

//https://adventofcode.com/2021/day/9
object Solution {

  object BasinSearcher {
    def check(x: Int, y: Int, searcher: BasinSearcher): BasinSearcher = {
      val value = searcher.matrix.get(x, y)
      val isBasin = value.exists(_ < 9)

      if (isBasin && !searcher.checked(x, y)) {
        searcher
          .matrix.hvNeighboursMapped(x, y)
          .foldLeft(searcher.pointChecked(x, y).increaseSize()) {
            case (searcher, ((xx, yy), value)) =>
              if (value < 9) check(xx, yy, searcher)
              else searcher.pointChecked(xx, yy)
          }
      } else searcher.pointChecked(x, y)
    }
  }

  def get(input: List[List[Int]], x: Int, y: Int): Option[Int] =
    input.lift(y).flatMap(_.lift(x))

  def getMapped(
      input: List[List[Int]],
      x: Int,
      y: Int
  ): Option[((Int, Int), Int)] =
    get(input, x, y).map((x, y) -> _)

  case class BasinSearcher(
      matrix: Matrix[Int],
      pointsChecked: Map[(Int, Int), Boolean] = Map.empty,
      sizes: Int = 0
  ) {
    def pointChecked(x: Int, y: Int) =
      copy(pointsChecked = pointsChecked + ((x, y) -> true))
    def checked(x: Int, y: Int) = pointsChecked.get((x, y)).contains(true)
    def increaseSize() = copy(sizes = sizes + 1)
  }

  def neighbourMap(matrix: Matrix[Int]): Seq[(Int, List[Int])] = {
    (for {
      col <- matrix.yIndices
      row <- matrix.xIndices
      digit <- matrix.get(row, col)
    } yield (digit, matrix.hvNeighbours(row, col))).toList
  }

  def part2(matrix: Matrix[Int]): Int = {
    val all = matrix.xy.foldLeft(List[BasinSearcher]()) { case (acc, (x, y)) =>
      val pointsChecked = acc.lastOption.map(_.pointsChecked).getOrElse(Map.empty)
      acc :+ BasinSearcher.check(x, y, BasinSearcher(matrix, pointsChecked))
    }

    all.map(_.sizes).sorted.takeRight(3).product
  }

  def part1(neighborMap: Seq[(Int, List[Int])]): Int = {
    neighborMap
      .filter { case (lowest, neighbors) =>
        neighbors.forall(_ > lowest)
      }
      .map { case (value, _) =>
        value + 1
      }
      .sum
  }

  val program = for {
    //input <- readNumbers("day9/input_example.txt")
    input <- readNumbers("day9/input.txt")

    matrix = Matrix[Int](input)

    _ <- printAny(part1(neighbourMap(matrix)))
    _ <- printAny(part2(matrix))
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
