package day7

import cats.effect.IOApp
import day7.Utils._

//https://adventofcode.com/2021/day/7
object Solution {
  def fuelConsumption1(start: Int, end: Int): Int = Math.abs(end - start)
  def fuelConsumption2(start: Int, end: Int): Int = (1 to Math.abs(end - start)).sum

  def lessFuel(input: List[Int], fuelConsumption: (Int, Int) => Int) = {
    def test(end: Int): Int = {
      val fuels = input.map(crab => fuelConsumption(crab, end))
      fuels.sum
    }

    //maybe implement improved algo like binary search instead of brute force?
    val r = for {
      i <- input.min to input.max
    } yield test(i)

    r.min
  }

  val program = for {
    input <- readNumbers("day7/input.txt")
    //input <- readNumbers("day7/input_example.txt")

    part1 = lessFuel(input, fuelConsumption1)
    part2 = lessFuel(input, fuelConsumption2)

    _ <- printAny(part1)
    _ <- printAny(part2)
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
