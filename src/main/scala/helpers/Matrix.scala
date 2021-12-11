package helpers

import helpers.Matrix.{X, Y}

case object Matrix {
  type X = Int
  type Y = Int
}

case class Matrix[T](input: List[List[T]]) {
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

  def dNeighbours(x: X, y: Y): List[T] =
    List(
      get(x - 1, y - 1),
      get(x + 1, y - 1),
      get(x - 1, y + 1),
      get(x + 1, y + 1)
    ).flatten

  def hvNeighbors(x: X, y: Y): List[T] = {
    List(
      get(x, y - 1),
      get(x - 1, y),
      get(x + 1, y),
      get(x, y + 1)
    ).flatten
  }

  def hvdNeighbors(x: X, y: Y): List[T] = {
    hvNeighbors(x, y) ::: dNeighbours(x, y)
  }
}
