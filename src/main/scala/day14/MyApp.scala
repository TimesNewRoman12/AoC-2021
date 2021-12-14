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

  def part2(polymer: String, pairs: Map[String, String], steps: Int): Unit = {
    // polymer producing a pair of polymers i.e. BB -> BNB -> Map(BN -> 1, NB -> 1)
    val producedPairs: Map[String, Map[String, Long]] =
      pairs.foldLeft(Map.empty[String, Map[String, Long]]) {
        case (acc, (pair, _)) =>
          val res = part1(pair, pairs, 1)
          val producedPairs: Map[String, Long] =
            res.sliding(2).foldLeft(Map.empty[String, Long]) {
              case (acc, pair) =>
                val count = acc.getOrElse(pair, 0L)
                acc + (pair -> (count + 1L))
            }
          //println(s"$pair => $res")
         // println(s"producedPairs => $producedPairs")
          acc + (pair -> producedPairs)
      }

    //println(s"produceMap => $producedPairs")

    def once(polymerAsMap: Map[String, Long]) =
      polymerAsMap.foldLeft(Map.empty[String, Long]) { case (acc, (pair, ccount)) =>

        val produces: Map[String, Long] = producedPairs.getOrElse(pair, Map.empty)
        //val withCount = produces.view.mapValues(_ * count).toMap
        val updatedMap = produces.foldLeft(acc) { case (acc, (pair, count)) =>
          val alreadyProduced = acc.getOrElse(pair, 0L)
          //println(s"inner pair => $pair")
          //println(alreadyProduced)
          //println(count)
          //println(ccount)
          acc + (pair -> (alreadyProduced + ccount))
        }

        //println(s"pair -> acc => $pair => $updatedMap")

        updatedMap
        /*
        val produces: Map[String, Int] = producedPairs.getOrElse(pair, Map.empty)
        val updatedMap = produces.foldLeft(acc) { case (acc, (pair, count)) =>
          val alreadyProduced = acc.getOrElse(pair, 0)
          acc + (pair -> (alreadyProduced + count))
        }

        updatedMap

         */
      }

    def polymerToMap(acc: String) =
      acc.sliding(2).foldLeft(Map.empty[String, Long]) { case (acc, pair) =>
        val produced =  acc.getOrElse(pair, 0L)
        acc + (pair -> (produced + 1L))
      }

    val polymerAsMap = polymerToMap(polymer)
    //println(polymerAsMap)

    def loop(polymerAsMap: Map[String, Long], steps: Int) = {
      (1 to steps).foldLeft(polymerAsMap) { case (acc, _) =>
        val res = once(acc)
        //println(s"qty: ${res.groupBy(identity).view.mapValues(_.length).toMap}")
        res
      }
    }

    val result = loop(polymerAsMap, steps)
    println(result)

    //val result: Map[String, Int] = once(polymer)
    //println(result)

    /*
    def loop(acc: Map[String, Int], steps: Int) = {
      (1 to steps).foldLeft(acc) { case (acc, _) =>
        val res = once(acc)
        //println(s"qty: ${res.groupBy(identity).view.mapValues(_.length).toMap}")
        res
      }
    }

     */


  }

  def part1(polymer: String, pairs: Map[String, String], steps: Int): String = {
    def loop(acc: String) = {
      (1 to steps).foldLeft(acc) { case (acc, _) =>
        val res = once(acc)
        println(res)
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


    //_ <- printAny("NBCCNBBBCBHCB")

   //_ <- printAny(part1(polymer, pairs, 5))
   _ = part2(polymer, pairs, 40)



    //_ <- printAny(part1(polymer, pairs, 4))
    //_ = part2(polymer, pairs, 4)


  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
