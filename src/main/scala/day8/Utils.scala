package day8

import cats.effect.IO
import day8.Solution.{Entry, Unknown}

import scala.io.Source

object Utils {
  def readNumbers(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList

    val entries = for {
      line <- lines
    } yield line.split("\\s\\|\\s").toList match {
      case signals :: output :: Nil =>
        Entry(
          signals.split(' ').toList.map(v => Unknown(v.sorted)),
          output.split(' ').toList.map(v => Unknown(v.sorted))
        )
      case _ => throw new Error("failed to parse input")
    }

    source.close()
    entries
  }

  def printAny(any: Any) = IO { println(any) }
}
