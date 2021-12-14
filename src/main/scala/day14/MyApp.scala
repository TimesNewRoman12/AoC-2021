package day14

import cats.effect.{IO, IOApp}
import day14.Utils._

//https://adventofcode.com/2021/day/14
object Solution {

  def toPairsMap(input: List[String]): Map[String, String] = {
    input.foldLeft(Map.empty[String, String]) { case (acc, string) =>
      string.split(" -> ").toList match {
        case pair :: value :: Nil =>
          acc + (pair -> value)
        case _ => throw new Error("Error parsing pair")
      }
    }
  }

  def part2(polymer: String, pairs: Map[String, String]): Unit = {
    // polymer producing a pair of polymers i.e. BB -> Map(BN -> 1, NB -> 1)
    val producedPairs: Map[String, Map[String, Int]] =
      pairs.foldLeft(Map.empty[String, Map[String, Int]]) {
        case (acc, (pair, _)) =>
          val res = part1(pair, pairs, 1)
          val producedPairs: Map[String, Int] =
            res.sliding(2).foldLeft(Map.empty[String, Int]) {
              case (acc, pair) =>
                val count = acc.getOrElse(pair, 0)
                acc + (pair -> (count + 1))
            }
          println(s"$pair => $res")
          println(s"producedPairs => $producedPairs")
          acc + (pair -> producedPairs)
      }

    def once(acc: String) =
      acc.sliding(2).foldLeft(Map.empty[String, Int]) { case (acc, pair) =>
        val produces: Map[String, Int] = producedPairs.getOrElse(pair, Map.empty)
        val updatedMap = produces.foldLeft(acc) { case (acc, (pair, count)) =>
          val alreadyProduced = acc.getOrElse(pair, 0)
          acc + (pair -> (alreadyProduced + count))
        }

        updatedMap
      }

    val result: Map[String, Int] = once(polymer)

    /*
    def loop(acc: Map[String, Int], steps: Int) = {
      (1 to steps).foldLeft(acc) { case (acc, _) =>
        val res = once(acc)
        //println(s"qty: ${res.groupBy(identity).view.mapValues(_.length).toMap}")
        res
      }
    }

     */

    println(result)
  }

  def part1(polymer: String, pairs: Map[String, String], steps: Int): String = {
    def loop(acc: String) = {
      (1 to steps).foldLeft(acc) { case (acc, _) =>
        val res = once(acc)
        //println(s"qty: ${res.groupBy(identity).view.mapValues(_.length).toMap}")
        res
      }
    }

    def once(acc: String) =
      acc.sliding(2).foldLeft("") { case (acc, pair) =>
        val in = pairs.getOrElse(pair, "")
        acc + pair.patch(1, in, 1)
      } + polymer.last

    /*
    val result = loop(polymer)
    val countMap: Map[Char, Int] = result.groupBy(identity).view.mapValues(_.length).toMap

    println(result)
    println(result.length)

    countMap.values.max - countMap.values.min
     */
    loop(polymer)
  }

  val program = for {
    //input <- readNumbers("day14/input.txt")
    input <- readNumbers("day14/input_example.txt")
    //input <- readNumbers("day14/test.txt")
    polymer = input.head
    pairs = toPairsMap(input.drop(2))

    //_ <- printAny(part1(polymer, pairs, 5))
    _ = part2(polymer, pairs)
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
