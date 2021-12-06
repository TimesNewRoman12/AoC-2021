package day5

import cats.effect.IO
import scala.io.Source

object Utils {

  // "5,9" to (5, 9)
  def parseXY(s: String): (Int, Int) = {
    val pair = s.split(",").toList

    pair match {
      case x :: y :: Nil => (x.toInt, y.toInt)
      case _ => throw new Error("failed to read an input pair")
    }
  }

  def readNumbers(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList
    for {
      line <- lines
      pair = line.split(" -> ").toList
      ((x1, y1), (x2, y2)) = pair match {
        case xy1 :: xy2 :: Nil => (parseXY(xy1), parseXY(xy2))
        case _ => throw new Error("failed to read an input pair of pairs")
      }
    } yield Line(x1, y1, x2, y2)
  }


  def printAny(any: Any) = IO { println(any) }
}