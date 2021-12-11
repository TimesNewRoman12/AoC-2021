package day11

import cats.effect.IOApp
import day11.Utils._
import helpers.Matrix

//https://adventofcode.com/2021/day/11
object Solution {

  case class Dumbo(energy: Int) {
    override def toString = energy.toString
  }

  val program = for {
    //input <- readNumbers("day11/input_example.txt")
    input <- readNumbers("day11/input.txt")
    matrix = Matrix(input).to(Dumbo.apply)

    _ = matrix.printOut

   // _ = matrix.printOut
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
