package day11

import cats.effect.IOApp
import day11.Utils._
import helpers.Matrix

//https://adventofcode.com/2021/day/11
object Solution {

  case class Cavern(matrix: Matrix[Dumbo], flashes: Int = 0) {
    def step: Cavern = {
      val step1 = matrix.map(_.increaseEnergy)

      def loop(matrix: Matrix[Dumbo]): Matrix[Dumbo] = {
        if (matrix.exists(_.shareLight)) {
          val step2 = matrix.xy.foldLeft(matrix) { case (matrix, (x, y)) =>
            if (
              matrix
                .get(x, y)
                .exists(dumbo => dumbo.flashed && dumbo.shareLight)
            )
              matrix
                .mapAt(x, y, _.sharedLight)
                .mapHvdNeighbours(x, y, _.neighbourFlashed)
            else
              matrix
          }
          loop(step2)
        } else matrix
      }
      val step2 = loop(step1)
      Cavern(step2.map(_.reset), flashes + step2.count(_.flashed))
    }
  }

  case class Dumbo(
      energy: Int,
      flashed: Boolean = false,
      shareLight: Boolean = false
  ) {
    def increaseEnergy: Dumbo =
      if (energy == 9) Dumbo(0, true, true)
      else Dumbo(energy + 1)

    def neighbourFlashed: Dumbo =
      if (flashed) this
      else increaseEnergy

    def sharedLight: Dumbo = copy(shareLight = false)
    def reset: Dumbo = Dumbo(energy)
    override def toString = energy.toString
  }

  def part1(matrix: Matrix[Dumbo]): Int =
    (1 to 100).foldLeft(Cavern(matrix))((cavern, _) => cavern.step).flashes

  def part2(matrix: Matrix[Dumbo]): Int = {
    def loop(cavern: Cavern, stepsMade: Int = 0): Int = {
      val step = cavern.step

      if (cavern.matrix.forall(_.energy == 0))
        stepsMade
      else
        loop(step, stepsMade + 1)
    }

    loop(Cavern(matrix))
  }

  val program = for {
    //input <- readNumbers("day11/input_example.txt")
    input <- readNumbers("day11/input.txt")
    matrix = Matrix(input).map(Dumbo.apply(_))
    _ = matrix.printOut

    _ = println(s"part1: ${part1(matrix)}")
    _ = println(s"part2: ${part2(matrix)}")
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
