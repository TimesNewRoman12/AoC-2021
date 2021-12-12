package day12

import cats.effect.IOApp
import day12.Solution.Connection.fromString
import day12.Utils._

//https://adventofcode.com/2021/day/12
object Solution {

  case object Connection {
    def fromString(s: String) = s.split('-').toList match {
      case head :: tail :: Nil => Connection(head, tail)
      case _ => throw new Error("Error parsing connection")
    }
  }

  def part1(middlePoints: List[String], allConnections: List[Connection]): Unit = {

    def connectionsTo(p: String): List[String] = {
      allConnections.filter(_.p1 == p).map(_.p2) :::
        allConnections.filter(_.p2 == p).map(_.p1)
    }

    println(connectionsTo("start"))


    def loop(path: List[String], exclusions: Map[List[String], List[String]] = Map.empty): List[String] = {

      val p1 = path.last
      val cons: List[String] = connectionsTo(p1).filterNot(_ == p1)

      val lowerCases = path.filter(_.forall(_.isLower))

      val excludeSmallAndVisited = cons.filterNot(p => lowerCases.contains(p))

      println(s"start: $p1")
      println(s"cons: $cons")
      //println(s"lowerCases: $lowerCases")
      println(s"excludeSmallAndVisited: $excludeSmallAndVisited")

      excludeSmallAndVisited match {
        case head :: tail =>
          println(s"path: ${path :+ head}")
          loop(path :+ head)
        case Nil => path
      }
    }

    println(loop(List("start")))
  }

  case class Connection(p1: String, p2: String)

  val program = for {
    input <- readNumbers("day12/input_example.txt")
    //input <- readNumbers("day12/input.txt")

    connections = input.map(fromString)


    points: List[String] = input.flatMap(_.split('-')).distinct

    middlePoints = points.filterNot(p => p == "start" || p == "end")

    _ = println(s"${input}")

    _ = println(s"${points}")
    _ = println(s"${middlePoints}")
    _ = println(connections)
    _ = println("----------")

    _ = part1(middlePoints, connections)

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}