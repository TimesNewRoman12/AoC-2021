package day17

import cats.effect.IOApp
import cats.implicits.catsSyntaxOptionId
import day17.Utils._

//https://adventofcode.com/2021/day/17
object Solution {

  case class Range(min: Int, max: Int)
  case class Velocity(x: Int, y: Int)
  case class Coords(x: Int, y: Int)

  case class Trajectory(
      startVelocity: Velocity,
      path: List[Coords] = List.empty
  ) {
    def addCoords(c: Coords): Trajectory = copy(path = path :+ c)
    def maxY: Int = if (path.nonEmpty) path.map(_.y).max else 0
  }

  def part2(xRange: Range, yRange: Range) = {
    def loop(
        currVelocity: Velocity,
        trajectory: Trajectory,
        currPos: Coords = Coords(0, 0)
    ): Option[Trajectory] = {
      val newPos =
        Coords(currPos.x + currVelocity.x, currPos.y + currVelocity.y)
      val newVelocityY = currVelocity.y - 1
      val newVelocityX =
        if (currVelocity.x > 0) currVelocity.x - 1
        else if (currVelocity.x < 0) currVelocity.x + 1
        else 0

      val newVelocity = Velocity(newVelocityX, newVelocityY)

      if (currPos.y < yRange.min || currPos.x > xRange.max)
        None
      else if (
        currPos.y <= yRange.max && currPos.y >= yRange.min &&
        currPos.x >= xRange.min && currPos.x <= xRange.max
      )
        trajectory.some
      else
        loop(
          newVelocity,
          trajectory.addCoords(Coords(currPos.x, currPos.y)),
          newPos
        )
    }

    val velocities = for {
      x <- 1 to 100 // actual range 12 to 96
      y <- -200 to 200 // actual range -179 to 170
    } yield Velocity(x, y)

    val trajectories = velocities.foldLeft(List.empty[Trajectory]) {
      case (acc, startingVelocity) =>
        val trajectory = loop(startingVelocity, Trajectory(startingVelocity))
        trajectory.map(t => acc :+ t).getOrElse(acc)
    }

    trajectories.length
  }

  def part1(yRange: Range) = {
    def loop(
        currVelocity: Int,
        trajectory: Trajectory,
        currY: Int = 0
    ): Option[Trajectory] = {
      val newY = currY + currVelocity

      if (currY > yRange.max)
        loop(currVelocity - 1, trajectory.addCoords(Coords(0, newY)), newY)
      else if (currY <= yRange.max && currY >= yRange.min)
        trajectory.some
      else
        None
    }

    val trajectories = (1 to 200).foldLeft(List.empty[Option[Trajectory]]) {
      case (acc, startingVelocity) =>
        val result =
          loop(startingVelocity, Trajectory(Velocity(0, startingVelocity)))
        acc :+ result
    }

    println(s"length: ${trajectories.flatten.length}")
    trajectories.flatten.map(_.maxY).max
  }

  val program = for {
    input <- readRanges("day17/input.txt")
    //input <- readRanges("day17/input_example.txt")
    (xRange, yRange) = input

    _ <- printAny { (xRange, yRange) }

    _ <- printAny { part1(yRange) }
    _ <- printAny { part2(xRange, yRange) }

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
