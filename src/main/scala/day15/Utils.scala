package day15

import cats.effect.IO

import scala.io.Source

object Utils {
  def readNumbers(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList

    val entries = for {
      line <- lines
    } yield line.toList.map(_.toString.toInt)

    source.close()
    entries
  }

  def printAny(any: Any) = IO { println(any) }
}