package day12

import cats.effect.IOApp
import day12.Solution.Connection.fromString
import day12.Utils._

//https://adventofcode.com/2021/day/12
object Solution {

  case object Connection {
    def fromString(s: String) = s.split('-').toList match {
      case head :: tail :: Nil => Connection(head, tail)
      case _                   => throw new Error("Error parsing connection")
    }
  }

  def part1(
      middlePoints: List[String],
      allConnections: List[Connection]
  ): Unit = {

    def connectionsTo(p: String): List[String] = {
      allConnections.filter(_.p1 == p).map(_.p2) :::
        allConnections.filter(_.p2 == p).map(_.p1)
    }

    println(connectionsTo("start"))

    def loop(
        currentPath: List[String],
        paths: List[List[String]] = List.empty,
        exclusions: Map[List[String], List[String]] = Map.empty
    ): List[List[String]] = {

      val p1 = currentPath.last
      val cons: List[String] = connectionsTo(p1).filterNot(_ == p1)

      val lowerCases = currentPath.filter(_.forall(_.isLower))

      val exclusion = exclusions.getOrElse(currentPath, List.empty)

      val excludeSmallAndVisited = cons
        .filterNot(p => lowerCases.contains(p))
        .filterNot(p => exclusion.contains(p))

      //println(s"start: $p1")
      //println(s"cons: $cons")
      //println(s"exclusions: $exclusions")
      //println(s"excludeSmallAndVisited: $excludeSmallAndVisited")

      excludeSmallAndVisited match {
        case head :: _ =>
          //println(s"path: ${currentPath :+ head}")
          loop(currentPath :+ head, paths, exclusions)
        case Nil =>
          //all options checked
          if (currentPath == List("start")) paths
          else {
            val exclusionKey = currentPath.dropRight(1)
            val exclusionValue = exclusions.getOrElse(exclusionKey, List.empty)
            loop(
              List("start"),
              paths :+ currentPath,
              exclusions + (exclusionKey -> (exclusionValue :+ currentPath.last))
            )
          }
      }
    }

    val allPaths = loop(List("start"))

    val startEndPaths = allPaths
      .filter(_.last == "end")
      .distinct

    println(startEndPaths.length)
  }

  def part2(
      middlePoints: List[String],
      allConnections: List[Connection]
  ): Unit = {

    def connectionsTo(p: String): List[String] = {
      allConnections.filter(_.p1 == p).map(_.p2) :::
        allConnections.filter(_.p2 == p).map(_.p1)
    }

    println(connectionsTo("start"))

    def loop(
        currentPath: List[String],
        paths: List[List[String]] = List.empty,
        exclusions: Map[List[String], List[String]] = Map.empty
    ): List[List[String]] = {

      val p1 = currentPath.last
      val cons: List[String] = connectionsTo(p1).filterNot(_ == p1)

      val firstSmallCave =
        currentPath
        .filterNot(List("start", "end").contains(_))
        .find(cave => cave.forall(_.isLower))

      val firstSmallCaveVisitedTwice = (for {
        ff <- firstSmallCave
      }  yield currentPath.count(_ == ff) == 2).getOrElse(false)

      println(s"firstSmallCave: $firstSmallCave")

      val smallCaves = currentPath
        .filter(cave => cave.forall(_.isLower) && (firstSmallCaveVisitedTwice || !firstSmallCave.contains(cave)))

      println(s"smallCaves: $smallCaves")

      val exclusion = exclusions.getOrElse(currentPath, List.empty)

      val excludeSmallAndVisited = cons
        .filterNot(p => smallCaves.contains(p))
        .filterNot(p => exclusion.contains(p))

      println(s"start: $p1")
      println(s"cons: $cons")
      println(s"exclusions: $exclusions")
      println(s"excludeSmallAndVisited: $excludeSmallAndVisited")

      excludeSmallAndVisited match {
        case _ :: _ if currentPath.endsWith("end") =>
          val exclusionKey = currentPath.dropRight(1)
          val exclusionValue = exclusions.getOrElse(exclusionKey, List.empty)
          loop(
            List("start"),
            paths :+ currentPath,
            exclusions + (exclusionKey -> (exclusionValue :+ currentPath.last))
          )
        case head :: _ =>
          println(s"path: ${currentPath :+ head}")
          loop(currentPath :+ head, paths, exclusions)
        case Nil =>
          //all options checked
          if (currentPath == List("start")) paths
          else {
            val exclusionKey = currentPath.dropRight(1)
            val exclusionValue = exclusions.getOrElse(exclusionKey, List.empty)
            loop(
              List("start"),
              paths :+ currentPath,
              exclusions + (exclusionKey -> (exclusionValue :+ currentPath.last))
            )
          }
      }
    }

    val allPaths = loop(List("start"))

    val startEndPaths = allPaths
      .filter(_.last == "end")
      .distinct

    println(startEndPaths)

    println(startEndPaths.length)
  }

  case class Connection(p1: String, p2: String)

  val program = for {
    //input <- readNumbers("day12/input_example.txt")
    input <- readNumbers("day12/input.txt")

    connections = input.map(fromString)

    points: List[String] = input.flatMap(_.split('-')).distinct

    middlePoints = points.filterNot(p => p == "start" || p == "end")

    _ = println(s"${input}")

    _ = println(s"${points}")
    _ = println(s"${middlePoints}")
    _ = println(connections)
    _ = println("----------")

    _ = part1(middlePoints, connections)
    //_ = part2(middlePoints, connections)

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
