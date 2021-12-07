package day2

import cats.effect.IOApp
import day2.Utils._

object Solution {
  case class Position(forward: Int, depth: Int) {
    def combine(p: Position): Position = Position(forward + p.forward, depth + p.depth)
    def multi: Int = forward * depth
  }

  case class Position2(forward: Int, depth: Int, aim: Int ) {
    def multi: Int = forward * depth
  }

  val ForwardR = "forward (\\d+)".r
  val DownR = "down (\\d+)".r
  val UpR = "up (\\d+)".r

  def toPosition(s: String): Position = s match {
    case ForwardR(distance) => Position(distance.toInt, 0)
    case DownR(distance)    => Position(0, distance.toInt)
    case UpR(distance)      => Position(0, -distance.toInt)
  }

  val program = for {
    commands <- getInput("day2/input.txt")
    //commands <- getInput("day2/input_example.txt")
    positions = commands.map(toPosition)

    position1 = positions.reduce(_ combine _)
    position2 = positions.foldLeft(Position2(0, 0, 0)) { case (acc, p) =>
      val forward = acc.forward + p.forward
      val aim = acc.aim + p.depth
      val depth = acc.depth + p.forward * acc.aim

      Position2(forward, depth, aim)
    }

    _ <- printAny(position1.multi)
    _ <- printAny(position2.multi)

    _ <- printAny(commands)
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
