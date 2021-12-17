package day17

import cats.effect.IOApp
import day17.Utils._

//https://adventofcode.com/2021/day/17
object Solution {

  case class Point(x: Int, y: Int)

  /*
  def yPos(velocity: Int, step: Int) = {
    def loop(currVelocity: Int, currY: Int, currStep: Int = 0): Unit = {

      if (currY == step)
    }
    velocity + (velocity - step) * step
  }

  def findY(yMin: Point) = {


  }

   */

  val program = for {
    //input <- readRanges("day17/input.txt")
    input <- readRanges("day17/input_example.txt")
    (xMin, yMin) = input

    _ <- printAny { (xMin, yMin) }
    //_ <- printAny { yPos(9, ) }

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
