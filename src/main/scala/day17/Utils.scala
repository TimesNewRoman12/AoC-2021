package day17

import cats.effect.IO
import day17.Solution.Range

import scala.io.Source

object Utils {
  def readRanges(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines = source.getLines().toList

    val range = lines.head.mkString

    val xIndex: Int = range.indexOf('x')

    val xMin = range.drop(xIndex+2).takeWhile(_ != '.')
    val xMax = range.drop(xIndex+xMin.length+4).takeWhile(_ != ',')

    val yIndex: Int = range.indexOf('y')

    val yMin = range.drop(yIndex+2).takeWhile(_ != '.')
    val yMax = range.drop(yIndex+yMin.length+4).takeWhile(_ != ',')


    (Range(xMin.toInt, xMax.toInt), Range(yMin.toInt, yMax.toInt))
  }

  def printAny(any: Any) = IO { println(any) }
}