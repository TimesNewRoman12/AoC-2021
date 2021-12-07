package day4

import cats.effect.IOApp
import day4.MyApp.{GridSize, Row}
import day4.Utils._

//https://adventofcode.com/2021/day/4
object MyApp extends IOApp.Simple {
  type Row = List[Int]
  val GridSize = 5

  val run = Solution.program
}

case object Board {
  val empty = Board(List.empty, List.empty)
}

case class Board(rows: List[Row], drawnNumbers: List[Int]) {
  def addRow(row: Row): Board = copy(rows = rows :+ row)
  def isWin: Boolean =
    rows.exists(_.intersect(drawnNumbers).size == GridSize) ||
      columns.exists(_.intersect(drawnNumbers).size == GridSize)
  def unmarkedSum: Int = rows.flatten.diff(drawnNumbers).sum

  def columns: List[List[Int]] = (for {
    index <- (0 until GridSize).toList
    row <- rows
  } yield row(index)).sliding(GridSize, GridSize).toList
}

sealed trait GameResolver {
  def winCondition(game: Game): Boolean
  def winningBoard(game: Game): Option[Board]
}

case object FirstBoardWins extends GameResolver {
  def winCondition(game: Game): Boolean = game.winningBoards.nonEmpty
  def winningBoard(game: Game): Option[Board] = game.winningBoards.headOption
}

case object LastBoardWins extends GameResolver {
  def winCondition(game: Game): Boolean = game.boards.isEmpty
  def winningBoard(game: Game): Option[Board] = game.winningBoards.lastOption
}

case class Game(
    boards: List[Board],
    drawnNumbers: List[Int] = List.empty,
    winningBoards: List[Board] = List.empty
)(implicit val gameResolver: GameResolver) {

  def applyDrawnNumbers(drawnNumbers: List[Int]): Game = {
    val boardsWithNumbers = for {
      board <- boards
    } yield board.copy(drawnNumbers = drawnNumbers)

    copy(boards = boardsWithNumbers, drawnNumbers = drawnNumbers)
  }

  def playUntilFinished(drawnNumbers: List[Int]): Game = {
    def roundLoop(game: Game, turn: Int): Game = {
      val numbers = drawnNumbers.take(turn)
      val gameWithNumbers = game.applyDrawnNumbers(numbers)
      val (winningBoards, remainingBoards) =
        gameWithNumbers.boards.partition(_.isWin)

      val updatedGame = gameWithNumbers.copy(
        boards = remainingBoards,
        winningBoards = winningBoards
      )

      if (gameResolver.winCondition(updatedGame)) updatedGame
      else roundLoop(updatedGame, turn + 1)
    }
    roundLoop(this, 1)
  }

  def winningScore: Option[Int] = for {
    board <- gameResolver.winningBoard(this)
    lastNumber <- drawnNumbers.lastOption
  } yield board.unmarkedSum * lastNumber
}

object Solution {
  val program = for {
    //numbers <- readNumbers("day4/example_numbers.txt")
    numbers <- readNumbers("day4/numbers.txt")
    //boards <- readBoards("day4/example_boards.txt")
    boards <- readBoards("day4/boards.txt")
    firstBoardWins = Game(boards)(FirstBoardWins).playUntilFinished(numbers)
    lastBoardWins = Game(boards)(LastBoardWins).playUntilFinished(numbers)
    _ <- printAny(firstBoardWins.winningScore)
    _ <- printAny(lastBoardWins.winningScore)
  } yield ()
}
