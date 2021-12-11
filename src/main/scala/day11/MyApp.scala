package day11

import cats.effect.IOApp
import day10.Utils._

//https://adventofcode.com/2021/day/11
object Solution {

  val program = for {
    //input <- readNumbers("day11/input_example.txt")
    input <- readNumbers("day11/input.txt")

    _ <- printAny(input)
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
