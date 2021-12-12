package day12

import cats.effect.IO

import scala.io.Source

object Utils {
  def readNumbers(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList

    source.close()
    lines
  }

  def printAny(any: Any) = IO { println(any) }
}
