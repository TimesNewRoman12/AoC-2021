package day12

import cats.effect.{IO, IOApp}
import day12.Solution.Connection.fromString
import day12.Utils._

import java.time.Instant

//https://adventofcode.com/2021/day/12
object Solution {

  case object Connection {
    def fromString(s: String) = s.split('-').toList match {
      case head :: tail :: Nil => Connection(head, tail)
      case _                   => throw new Error("Error parsing connection")
    }
  }

  def part1(
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

      excludeSmallAndVisited match {
        case head :: _ =>
          //println(s"path: ${currentPath :+ head}")
          loop(currentPath :+ head, paths, exclusions)
        case Nil =>
          //all options checked
          if (currentPath == List("start")) paths
          else {
            if (currentPath.lastOption.contains("end")) {
              val exclusionKey = currentPath.dropRight(1)
              val exclusionValue =
                exclusions.getOrElse(exclusionKey, List.empty)
              loop(
                List("start"),
                paths :+ currentPath,
                exclusions + (exclusionKey -> (exclusionValue :+ currentPath.last))
              )
            } else {
              val exclusionKey = currentPath.dropRight(1)
              val exclusionValue =
                exclusions.getOrElse(exclusionKey, List.empty)
              loop(
                List("start"),
                paths,
                exclusions + (exclusionKey -> (exclusionValue :+ currentPath.last))
              )
            }
          }
      }
    }

    val allPaths = loop(List("start"))

    val startEndPaths = allPaths
      .filter(_.last == "end")
      .distinct

    println(startEndPaths.length)
  }

  //TODO: reduced part2 to 70 seconds
  def part2(
      allConnections: List[Connection]
  ): IO[Int] = IO.pure {

    def connectionsTo(p: String): List[String] = {
      allConnections.filter(_.p1 == p).map(_.p2) :::
        allConnections.filter(_.p2 == p).map(_.p1)
    }

    def loop(
        currentPath: List[String],
        paths: List[List[String]] = List.empty,
        exclusions: Map[List[String], List[String]] = Map.empty
    ): List[List[String]] = {

      val p1 = currentPath.last
      val cons: List[String] = connectionsTo(p1).filterNot(_ == p1)

      val smallCavesVisited =
        currentPath
          .filterNot(List("start", "end").contains(_))
          .filter(cave => cave.forall(_.isLower))

      val smallCaveVisitedTwice =
        smallCavesVisited.groupBy(identity).values.collectFirst {
          case x :: _ :: Nil => x
        }

      val smallCaves =
        if (smallCaveVisitedTwice.isDefined)
          currentPath
            .filter(cave => cave.forall(_.isLower))
        else
          currentPath
            .filter(List("start", "end").contains(_))

      val exclusion = exclusions.getOrElse(currentPath, List.empty)

      val excludeSmallAndVisited = cons
        .filterNot(p => smallCaves.contains(p))
        .filterNot(p => exclusion.contains(p))

      excludeSmallAndVisited match {
        case head :: _ =>
          loop(currentPath :+ head, paths, exclusions)
        case Nil =>
          //all options checked
          if (currentPath == List("start")) paths
          else {
            if (currentPath.lastOption.contains("end")) {
              val exclusionKey = currentPath.dropRight(1)
              val exclusionValue =
                exclusions.getOrElse(exclusionKey, List.empty)
              loop(
                List("start"),
                paths :+ currentPath,
                exclusions + (exclusionKey -> (exclusionValue :+ currentPath.last))
              )
            } else {
              val exclusionKey = currentPath.dropRight(1)
              val exclusionValue =
                exclusions.getOrElse(exclusionKey, List.empty)
              loop(
                List("start"),
                paths,
                exclusions + (exclusionKey -> (exclusionValue :+ currentPath.last))
              )
            }
          }
      }
    }

    val allPaths = loop(List("start"))

    val startEndPaths = allPaths
      .filter(_.last == "end")
      .distinct

    startEndPaths.length
  }

  case class Connection(p1: String, p2: String)

  val program = for {
    //input <- readNumbers("day12/input_example.txt")
    input <- readNumbers("day12/input.txt")

    connections = input.map(fromString)

    t1 = Instant.now().toEpochMilli
    //_ = part1(connections)
    res2 <- part2(connections)
    _ <- printAny(res2)
    _ <- printAny(s"time: ${Instant.now().toEpochMilli - t1}")

  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
