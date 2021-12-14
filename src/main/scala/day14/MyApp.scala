package day14

import cats.effect.{IO, IOApp}
import day14.Utils._

//https://adventofcode.com/2021/day/14
object Solution {

  // Map(PN -> B, OS -> K), etc
  def toPairsMap(input: List[String]): Map[String, String] = {
    input.foldLeft(Map.empty[String, String]) { case (acc, string) =>
      string.split(" -> ").toList match {
        case pair :: value :: Nil =>
          acc + (pair -> value)
        case _ => throw new Error("Error parsing pair")
      }
    }
  }

  def part2(polymer: String, pairs: Map[String, String], steps: Int): Long = {
    // polymer producing a pair of polymers i.e. BB -> Map(BN -> 1, NB -> 1)
    val producedPairs: Map[String, Map[String, Long]] =
      pairs.foldLeft(Map.empty[String, Map[String, Long]]) {
        case (acc, (pair, _)) =>
          val produced = part1(pair, pairs, 1)
          val producedPairs: Map[String, Long] =
            produced.sliding(2).foldLeft(Map.empty[String, Long]) {
              case (acc, pair) =>
                val count = acc.getOrElse(pair, 0L)
                acc + (pair -> (count + 1L))
            }
          acc + (pair -> producedPairs)
      }

    def once(polymerAsMap: Map[String, Long]): Map[String, Long] =
      polymerAsMap.foldLeft(Map.empty[String, Long]) {
        case (acc, (pair, count)) =>
          val produces: Map[String, Long] =
            producedPairs.getOrElse(pair, Map.empty)
          val updatedMap = produces.foldLeft(acc) { case (acc, (pair, _)) =>
            val alreadyProduced = acc.getOrElse(pair, 0L)
            acc + (pair -> (alreadyProduced + count))
          }
          updatedMap
      }

    def polymerToMap(acc: String): Map[String, Long] =
      acc.sliding(2).foldLeft(Map.empty[String, Long]) { case (acc, pair) =>
        val produced = acc.getOrElse(pair, 0L)
        acc + (pair -> (produced + 1L))
      }

    val polymerAsMap = polymerToMap(polymer)

    def loop(polymerAsMap: Map[String, Long], steps: Int): Map[String, Long] = {
      (1 to steps).foldLeft(polymerAsMap) { case (acc, _) =>
        once(acc)
      }
    }

    val result = loop(polymerAsMap, steps)
    val letters = pairs.values.toList.distinct

    def countMatches(string: String, subset: String) =
      string.length() - string.replaceAll(subset, "").length()

    // HashMap(BH -> 2, BB -> 3, CB -> 1, HC -> 0) for B will return (2 + 3*2 + 1 + 0) = 7
    val letterCounts: Seq[Long] = letters.map { letter =>
      result.collect { case (key, value) =>
        val repeats = countMatches(key, letter)
        value * repeats
      }.sum
    }

    // each pair shares a two letter combination with the other except a last one
    def removeSharedDuplicates(value: Long) =
      (value.toDouble / 2).toLong + value % 2

    removeSharedDuplicates(letterCounts.max) -
      removeSharedDuplicates(letterCounts.min)
  }

  def part1(polymer: String, pairs: Map[String, String], steps: Int): String = {
    def loop(acc: String): String =
      (1 to steps).foldLeft(acc) { case (acc, _) => once(acc) }

    def once(acc: String) =
      acc.sliding(2).foldLeft("") { case (acc, pair) =>
        val in = pairs.getOrElse(pair, "")
        acc + pair.patch(1, in, 1)
      } + polymer.last

    loop(polymer)
  }

  val program = for {
    input <- readNumbers("day14/input.txt")
    //input <- readNumbers("day14/input_example.txt")
    polymer = input.head
    pairs = toPairsMap(input.drop(2))

    part1String = part1(polymer, pairs, 10)
    part1Counts = part1String.groupBy(identity).view.mapValues(_.length).toMap
    part1Result = part1Counts.values.max - part1Counts.values.min
    _ <- printAny { part1Result }

    _ <- printAny { part2(polymer, pairs, 40) }
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}