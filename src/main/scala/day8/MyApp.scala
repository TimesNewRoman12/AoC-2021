package day8

import cats.effect.IOApp
import cats.implicits.catsSyntaxOptionId
import day8.Utils._

//https://adventofcode.com/2021/day/8
object Solution {
  type Decoder = DecoderState => Option[DecoderState]
  type FindFilter = (Digit, Digit) => Boolean

  object DecoderState {
    def apply(entry: Entry): DecoderState =
      DecoderState(entry.output, entry.signals, List.empty)
  }

  case class DecoderState(
    toDecode: List[Unknown],
    unknown: List[Unknown],
    known: List[Known]
  ) {
    def decode(decoders: List[Decoder]): Option[DecoderState] =
      decoders.foldLeft(this.some)((acc, decoder) => acc.flatMap(decoder))

    def find(
      value: Int,
      requires: Int,
      segmentFilter: Unknown => Boolean,
      findFilter: FindFilter,
    ): Option[DecoderState] =
      for {
        use <- use(requires)
        matched <- unknown
          .filter(segmentFilter)
          .find(f => findFilter(f, use))
      } yield found(matched, value)

    def use(value: Int): Option[Known] = known.find(_.value == value)

    def found(matched: Unknown, value: Int): DecoderState = DecoderState(
      toDecode,
      unknown.filterNot(v => v.encoded == matched.encoded),
      known :+ matched.to(value)
    )

    def last(value: Int, segments: Int): Option[DecoderState] = for {
      use <- unknown.find(filterBySegments(_, segments))
    } yield found(use, value)

    def result: Seq[Known] = for {
      out <- toDecode
      decoded <- known.find(_ == out)
    } yield decoded
  }

  def decode1: List[Decoder] = List(find1, find4, find7, find8)
  def decode2: List[Decoder] = decode1 ::: List(find9, find0, find6, find2, find3, find5)

  def filterBySegments(v: Unknown, segments: Int): Boolean = v.segments == segments
  val segments5: Unknown => Boolean = _.segments == 5
  val segments6: Unknown => Boolean = _.segments == 6

  val contains: FindFilter = (used, found) => used.contains(found)
  val foundIsNotInUsed: FindFilter = (used, found) => !found.contains(used)

  /*
  1 | the only among 2 segments
  4 | the only among 4 segments
  7 | the only among 3 segments
  8 | the only among 7 segments
   */
  val find1: Decoder = state => state.last(value = 1, segments = 2)
  val find4: Decoder = state => state.last(value = 4, segments = 4)
  val find7: Decoder = state => state.last(value = 7, segments = 3)
  val find8: Decoder = state => state.last(value = 8, segments = 7)

  /*
  9 | 6 segments, containing 4
  0 | 6 segments, containing 7
  6 | remaining among 6 segments
   */
  val find9: Decoder = state => state.find(value = 9, requires = 4, segments6, contains)
  val find0: Decoder = state => state.find(value = 0, requires = 7, segments6, contains)
  val find6: Decoder = state => state.last(value = 6, segments = 6)

  /*
  2 | 5 segments is not contained in 9
  3 | 5 segments contains 1
  5 | remaining among 5 segments
   */
  val find2: Decoder = state => state.find(value = 2, requires = 9, segments5, foundIsNotInUsed)
  val find3: Decoder = state => state.find(value = 3, requires = 1, segments5, contains)
  val find5: Decoder = state => state.last(value = 5, segments = 5)

  sealed trait Digit {
    def encoded: String
    def segments: Int = encoded.length
    def contains(d: Digit): Boolean =
      d.encoded.toList.forall(c => encoded.contains(c))
    def ==(known: Digit): Boolean = encoded == known.encoded
  }

  case class Unknown(encoded: String) extends Digit {
    def to(value: Int): Known = Known(encoded, value)
  }

  case class Known(encoded: String, value: Int) extends Digit

  case class Entry(signals: List[Unknown], output: List[Unknown])

  def part1(entries: List[Entry]): Int = (for {
    entry <- entries
    decoded <- DecoderState(entry).decode(decode1)
  } yield decoded.result).flatten.size

  def part2(entries: List[Entry]): Int = (for {
    entry <- entries
    decoded <- DecoderState(entry).decode(decode2)
    int = decoded.result.map(_.value.toString).reduce(_ + _).toInt
  } yield int).sum

  val program = for {
    entries <- readNumbers("day8/input.txt")

    _ <- printAny(part1(entries))
    _ <- printAny(part2(entries))
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
