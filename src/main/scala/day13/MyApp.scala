package day13

import cats.effect.{IO, IOApp}
import day13.Utils._
import helpers.Matrix.{X, Y, _}
import helpers.Matrix

//https://adventofcode.com/2021/day/13
object Solution {

  type Axis = Char
  type Coords = List[(X, Y)]
  type Folds = List[(Axis, Int)]
  type Dots = Map[(X, Y), Char]

  def filledMap(coords: Coords): Dots = {
    val emptyMap: Dots = Map.empty[(X, Y), Char]
    coords.foldLeft(emptyMap) { case (acc, (x, y)) =>
      acc + ((x, y) -> '#')
    }
  }

  def solution(map: Dots, folds: Folds): Dots =
    folds.foldLeft(map) { case (acc, (axis, value)) =>
      fold(acc, axis, value)
    }

  def fold(map: Dots, axis: Axis, value: Int): Dots = {
    def deleteFoldingLine(map: Dots, x: X, y: Y): Dots = {
      if (axis == 'y' && y == value) map - ((x, y))
      else if (axis == 'x' && x == value) map - ((x, y))
      else map
    }

    map.foldLeft(map) { case (acc, ((x, y), char)) =>
      if (axis == 'x') {
        val dd = deleteFoldingLine(acc, x, y)
        if (x <= value) dd
        else dd + ((value * 2 - x, y) -> char) - ((x, y))
      } else if (axis == 'y') {
        val dd = deleteFoldingLine(acc, x, y)
        if (y <= value) dd
        else dd + ((x, value * 2 - y) -> char) - ((x, y))
      } else acc
    }
  }

  val program = for {
    //input <- readNumbers("day13/test.txt")
    //input <- readNumbers("day13/input_example.txt")
    input <- readNumbers("day13/input.txt")
    _ <- printAny(input)

    input <- parse(input)
    (coords, folds) = input

    map = filledMap(coords)

    part1 = solution(map, folds.take(1))
    _ <- printAny(part1.size)

    part2 = solution(map, folds)
    _ <- printMap(part2)
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
