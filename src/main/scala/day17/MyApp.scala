package day17

import cats.effect.IOApp
import cats.implicits.catsSyntaxOptionId
import day17.Solution.Trajectory
import day17.Utils._

//https://adventofcode.com/2021/day/17
object Solution {

  case class Range(min: Int, max: Int)
  case class Velocity(x: Int, y: Int)
  case class Coords(x: Int, y: Int)

  case class Trajectory(startVelocity: Velocity, path: List[Coords] = List.empty) {
    def addCoords(c: Coords): Trajectory = copy(path = path :+ c)
    def maxY: Int = if (path.nonEmpty) path.map(_.y).max else 0
  }

  def findY(yRange: Range) = {
    def loop(currVelocity: Int, trajectory: Trajectory, currY: Int = 0): Option[Trajectory] = {
      //println(currY)
      val newY = currY + currVelocity

      if (currY > yRange.max)
        loop(currVelocity - 1, trajectory.addCoords(Coords(0, newY)), newY)
      else if (currY <= yRange.max && currY >= yRange.min)
        trajectory.some
      else
        None
    }

    val trajectories = ( 1 to 1000).foldLeft(List.empty[Option[Trajectory]]) { case (acc, startingVelocity) =>
      val result = loop(startingVelocity, Trajectory(Velocity(0, startingVelocity)))
      acc :+ result
    }

    println(s"length: ${trajectories.flatten.length}")

    trajectories.flatten.map(_.maxY).max
  }


  def checkY(yVelocity: Int, steps: Int) = {
    def loop(currVelocity: Int, currY: Int = 0, currStep: Int = 0): Int = {

      println(currY)

      if (currStep == steps) currY
      else loop(currVelocity - 1, currY + currVelocity, currStep + 1)
    }

    loop(yVelocity)
  }

  def checkX(xVelocity: Int, steps: Int) = {
    def loop(currVelocity: Int, currX: Int = 0, currStep: Int = 0): Int = {

      println(currX)

      if (currStep == steps) currX
      else {
        val newVelocity = if (currVelocity > 0) currVelocity - 1
        else if (currVelocity < 0) currVelocity + 1
        else 0
        loop(newVelocity, currX + newVelocity, currStep + 1)
      }
    }

    loop(xVelocity)
  }



  val program = for {
    input <- readRanges("day17/input.txt")
    //input <- readRanges("day17/input_example.txt")
    (xRange, yRange) = input

    _ <- printAny { (xRange, yRange) }

    _ <- printAny { findY(yRange) }
    //_ <- printAny { checkY(9, 20) }
    //_ <- printAny { checkX(6, 20) }

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
