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

    def findAs(
      requires: Int,
      as: Int,
      segmentFilter: Unknown => Boolean,
      findFilter: FindFilter,
    ): Option[DecoderState] =
      for {
        use <- use(requires)
        matched <- unknown
          .filter(segmentFilter)
          .find(f => findFilter(f, use))
      } yield found(matched, as)

    def use(value: Int): Option[Known] = known.find(_.value == value)

    def found(matched: Unknown, value: Int): DecoderState = DecoderState(
      toDecode,
      unknown.filterNot(v => v.encoded == matched.encoded),
      known :+ matched.to(value)
    )

    def lastAs(segments: Int, as: Int): Option[DecoderState] = for {
      use <- unknown.find(filterBySegments(_, segments))
    } yield found(use, as)

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
  val find1: Decoder = state => state.lastAs(2, 1)
  val find4: Decoder = state => state.lastAs(4, 4)
  val find7: Decoder = state => state.lastAs(3, 7)
  val find8: Decoder = state => state.lastAs(7, 8)

  /*
  9 | 6 segments, containing 4
  0 | 6 segments, containing 7
  6 | remaining among 6 segments
   */
  val find9: Decoder = state => state.findAs(4, 9, segments6, contains)
  val find0: Decoder = state => state.findAs(7, 0, segments6, contains)
  val find6: Decoder = state => state.lastAs(6, 6)

  /*
  2 | 5 segments is not contained in 9
  3 | 5 segments contains 1
  5 | remaining among 5 segments
   */
  val find2: Decoder = state => state.findAs(9, 2, segments5, foundIsNotInUsed)
  val find3: Decoder = state => state.findAs(1, 3, segments5, contains)
  val find5: Decoder = state => state.lastAs(5, 5)

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
