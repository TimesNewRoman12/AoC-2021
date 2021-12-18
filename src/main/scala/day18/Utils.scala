package day18

import cats.effect.IO
import day17.Solution.Range

import scala.io.Source

object Utils {
  def readLines(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList

    lines
  }

  def printAny(any: Any) = IO { println(any) }
}