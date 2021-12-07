package day1

import cats.effect.IOApp
import day7.Utils._

//https://adventofcode.com/2021/day/1
object Solution {

  def part1(input: List[Int]) =
    (for {
      slide <- input.sliding(2)
    } yield slide match {
      case prev :: curr :: _ => if (curr > prev) 1 else 0
    }).sum

  def part2(input: List[Int]) = part1(input.sliding(3).map(_.sum).toList)


  val program = for {
    input <- readNumbers("day1/input.txt")
    //input <- readNumbers("day1/input_example.txt")

    _ <- printAny(part1(input))
    _ <- printAny(part2(input))
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
