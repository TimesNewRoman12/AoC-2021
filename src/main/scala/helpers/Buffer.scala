package helpers

import cats.effect.IO

case class Buffer(value: String) {
  def isEmpty = value.isEmpty
  def length = value.length

  def consume(n: Int): IO[Buffer] = IO.pure { Buffer(value.drop(n)) }
  def take(n: Int): IO[(Buffer, String)] = IO.pure {
    (Buffer(value.drop(n)), value.take(n))
  }

  def scan(from: Int, until: Int): String = value.slice(from, until)
  def scan(n: Int): String = value.take(n)
}