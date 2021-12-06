package day4

import cats.effect.IO

import scala.io.Source

object Utils {
  def readNumbers(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    source.getLines().mkString(",").split(",").map(_.toInt).toList
  }

  def readBoards(fileName: String) = IO {
    val source = Source.fromResource(fileName)
    val lines: List[String] = source.getLines().toList

    def loop(lines: List[String], board: Board, boards: List[Board] = List.empty): List[Board] = {
      lines match {
        case head :: tail => head match {
          case head =>
            val rowNumbers: List[Int] = head.split("\\s+").filterNot(_.isEmpty).map(_.toInt).toList

            if (rowNumbers.nonEmpty)
              loop(tail, board.addRow(rowNumbers), boards)
            else
              loop(tail, Board.empty, boards :+ board)
        }
        case Nil => boards :+ board
      }
    }

    loop(lines, Board.empty, List.empty)
  }

  def printAny(any: Any) = IO { println(any) }
}