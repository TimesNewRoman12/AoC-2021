package day2

import cats.effect.IO

import scala.io.Source

object Utils {
  def getInput(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    source.close()
    lines
  }

  def printAny(any: Any) = IO { println(any) }
}