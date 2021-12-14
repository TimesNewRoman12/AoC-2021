package day14

import cats.effect.{IO, IOApp}
import day14.Utils._

//https://adventofcode.com/2021/day/14
object Solution {

  def toPairsMap(input: List[String]): Map[String, String] = {
    input.foldLeft(Map.empty[String, String]){
      case (acc, string) =>
        string.split(" -> ").toList match {
          case pair :: value :: Nil =>
            acc + (pair -> value)
          case _ => throw new Error("Error parsing pair")
        }
    }
  }

  def part1(polymer: String, pairs: Map[String, String], steps: Int): Int = {
    def loop(acc: String) = {
      (1 to steps).foldLeft(acc) { case (acc, _) =>
        val res = once(acc)
        println(s"qty: ${res.groupBy(identity).view.mapValues(_.length).toMap}")
        res
      }
    }

    def once(acc: String) =
      acc.sliding(2).foldLeft(""){
      case (acc, pair) =>
        val in = pairs.getOrElse(pair, "")
        acc + pair.patch(1, in, 1)
    } + polymer.last

    val result = loop(polymer)
    val countMap: Map[Char, Int] = result.groupBy(identity).view.mapValues(_.length).toMap

    println(result)
    println(result.length)

    countMap.values.max - countMap.values.min
  }

  val program = for {
    //input <- readNumbers("day14/input.txt")
    input <- readNumbers("day14/input_example.txt")
    //input <- readNumbers("day14/test.txt")
    polymer = input.head
    pairs = toPairsMap(input.drop(2))

    _ <- printAny(part1(polymer, pairs, 5))
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
