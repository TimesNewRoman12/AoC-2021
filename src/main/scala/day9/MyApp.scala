package day9

import cats.effect.IOApp
import cats.implicits.catsSyntaxOptionId
import day9.Solution.{getMapped, neighbourMap}
import day9.Utils._

//https://adventofcode.com/2021/day/9
object Solution {

  object BasinSearcher {
    def check(x: Int, y: Int, searcher: BasinSearcher): BasinSearcher = {
      val value = get(searcher.input, x, y)
      val isBasin = value.exists(_ < 9)

      if (isBasin && !searcher.checked(x, y)) {
        searcher
          .takeNeighbours(x, y)
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
      input: List[List[Int]],
      pointsChecked: Map[(Int, Int), Boolean] = Map.empty,
      sizes: Int = 0
  ) {
    def pointChecked(x: Int, y: Int) =
      copy(pointsChecked = pointsChecked + ((x, y) -> true))
    def checked(x: Int, y: Int) = pointsChecked.get((x, y)).contains(true)
    def increaseSize() = copy(sizes = sizes + 1)

    def takeNeighbours(x: Int, y: Int): Seq[((Int, Int), Int)] = {
      List(
        getMapped(input, x, y - 1),
        getMapped(input, x - 1, y),
        getMapped(input, x + 1, y),
        getMapped(input, x, y + 1)
      ).flatten
    }
  }

  def neighbourMap(input: List[List[Int]]): Seq[(Int, List[Int])] = {
    def takeNeighbours(x: Int, y: Int) = {
      List(
        get(input, x, y - 1),
        get(input, x - 1, y),
        get(input, x + 1, y),
        get(input, x, y + 1)
      ).flatten
    }

    val height = input.size
    val width = input.head.size

    (for {
      col <- 0 until height
      row <- 0 until width
      digit <- get(input, row, col)
    } yield (digit, takeNeighbours(row, col))).toList
  }

  def part2(input: List[List[Int]]): Int = {
    val height = input.size
    val width = input.head.size

    val xy = for {
      col <- 0 until height
      row <- 0 until width
    } yield (row, col)

    val all = xy.foldLeft(List[BasinSearcher]()) { case (acc, (x, y)) =>
      val pointsChecked = acc.lastOption.map(_.pointsChecked).getOrElse(Map.empty)
      acc :+ BasinSearcher.check(x, y, BasinSearcher(input, pointsChecked))
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

    _ <- printAny(part1(neighbourMap(input)))
    _ <- printAny(part2(input))
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
