package day10

import cats.effect.IOApp
import day10.Utils._

//https://adventofcode.com/2021/day/10
object Solution {

  val closures = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val openings = closures.keys.toList
  val corruptedScores = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val incompleteScores = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)

  sealed trait Chunk
  case class Corrupted(char: Char, expected: List[Char]) extends Chunk {
    def toPoints: Int = corruptedScores(char)
  }
  case class Incomplete(expected: List[Char]) extends Chunk {
    def toPoints: Long =
      expected.foldLeft(0L)((score, char) => score * 5 + incompleteScores(char))
  }

  case object Valid extends Chunk

  case object Chunk {
    def unfinishedOrValid(expected: List[Char]): Chunk =
      if (expected.isEmpty) Valid
      else Incomplete(expected)
  }

  def loop(chunk: List[Char], expected: List[Char] = List.empty): Chunk =
    chunk match {
      case head :: tail =>
        if (openings.contains(head))
          loop(tail, closures(head) :: expected)
        else if (head == expected.head)
          loop(tail, expected.drop(1))
        else
          Corrupted(head, expected)
      case Nil =>
        Chunk.unfinishedOrValid(expected)
    }

  def parseChunks(input: List[List[Char]]): List[Chunk] =
    for {
      chunk <- input
    } yield loop(chunk)

  def part1(input: List[List[Char]]) = {
    val parsedChunks = parseChunks(input)

    parsedChunks
      .collect { case c: Corrupted => c }
      .map(_.toPoints)
      .sum
  }

  def part2(input: List[List[Char]]) = {
    val parsedChunks = parseChunks(input)

    val scores = parsedChunks
      .collect { case c: Incomplete => c }
      .map(_.toPoints)
      .sorted

    scores.drop(scores.length / 2).headOption
  }

  val program = for {
    //input <- readNumbers("day10/input_example.txt")
    input <- readNumbers("day10/input.txt")

    _ <- printAny(part1(input))
    _ <- printAny(part2(input))
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
