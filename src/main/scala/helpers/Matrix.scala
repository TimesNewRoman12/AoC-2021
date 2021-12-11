package helpers

import helpers.Matrix.{X, Y}

import scala.annotation.tailrec

case object Matrix {
  type X = Int
  type Y = Int
}

case class Matrix[T](input: List[List[T]]) {

  def printOut: Unit = input.foreach(row => println(row.mkString))

  def map[TT](apply: T => TT): Matrix[TT] =
    Matrix(
      for {
        row <- input
      } yield row.map(apply)
    )

  def count(f: T => Boolean): Int = (for {
    row <- input
  } yield row.count(f)).sum

  def mapAt(x: X, y: Y, apply: T => T): Matrix[T] = {
    val updated = for {
      row <- input.lift(y)
      el <- get(x, y)
      f = apply(el)
    } yield input.updated(y, row.updated(x, f))

    updated.map(Matrix(_)).getOrElse(this)
  }

  def exists(f: T => Boolean): Boolean = {
    @tailrec def loopRow(rows: List[T]): Boolean =
      rows match {
        case head :: tail =>
          if (f(head)) true
          else loopRow(tail)
        case Nil => false
      }

    @tailrec def loop(columns: List[List[T]]): Boolean =
      columns match {
        case head :: tail =>
          if (loopRow(head)) true
          else loop(tail)
        case Nil => false
      }

    loop(input)
  }

  def forall(f: T => Boolean): Boolean = !exists(!f(_))

  def mapHvdNeighbours(x: X, y: Y, apply: T => T): Matrix[T] = {
    val neighboursCoords = hvdNeighboursMapped(x, y) map { case (xy, _) => xy }

    neighboursCoords.foldLeft(this) { case (matrix, (xx, yy)) =>
      matrix.mapAt(xx, yy, apply)
    }
  }

  def get(x: X, y: Y): Option[T] = input.lift(y).flatMap(_.lift(x))

  def height: Int = input.size
  def width: Int = input.headOption.fold(0)(_.size)

  def yIndices: Seq[Int] = 0 until height
  def xIndices: Seq[Int] = 0 until width

  def xy: Seq[(X, Y)] = for {
    col <- yIndices
    row <- xIndices
  } yield (row, col)

  def getMapped(x: X, y: Y): Option[((X, Y), T)] =
    get(x, y).map((x, y) -> _)

  def hvNeighboursMapped(x: X, y: Y): List[((X, Y), T)] = {
    List(
      getMapped(x, y - 1),
      getMapped(x - 1, y),
      getMapped(x + 1, y),
      getMapped(x, y + 1)
    ).flatten
  }

  def dNeighboursMapped(x: X, y: Y): List[((X, Y), T)] =
    List(
      getMapped(x - 1, y - 1),
      getMapped(x + 1, y - 1),
      getMapped(x - 1, y + 1),
      getMapped(x + 1, y + 1)
    ).flatten

  def hvdNeighboursMapped(x: X, y: Y): List[((X, Y), T)] =
    hvNeighboursMapped(x, y) ::: dNeighboursMapped(x, y)

  def dNeighbours(x: X, y: Y): List[T] =
    List(
      get(x - 1, y - 1),
      get(x + 1, y - 1),
      get(x - 1, y + 1),
      get(x + 1, y + 1)
    ).flatten

  def hvNeighbours(x: X, y: Y): List[T] = {
    List(
      get(x, y - 1),
      get(x - 1, y),
      get(x + 1, y),
      get(x, y + 1)
    ).flatten
  }

  def hvdNeighbours(x: X, y: Y): List[T] = {
    hvNeighbours(x, y) ::: dNeighbours(x, y)
  }
}
