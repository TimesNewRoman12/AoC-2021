package day16

import cats.effect.IO

import scala.io.Source

object Utils {
  def readHex(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList

    val entries = for {
      line <- lines
    } yield line.toList.mkString

    source.close()
    val zeroes = entries.head.head match {
      case '0' | '1' | '2' | '3' => "00"
      case '4' | '5' | '6' | '7' => "0"
      case _ => ""
    }

    zeroes + BigInt(entries.head, 16).toString(2)
  }

  def printAny(any: Any) = IO { println(any) }
}