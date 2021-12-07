package day6

import cats.effect.{IO, IOApp}
import day6.Utils._

object Solution {
  type FishCount = List[(Int, Long)]

  val NewBorn = 8
  val Reset = 6
  val Mommy = 0

  def dayGone(swarm: FishCount): IO[FishCount] = IO.pure {
    def getOlder(age: Int) = if (age == Mommy) Reset else age - 1
    def mommies: Long = swarm.collectFirst { case (age, qty) if age == Mommy => qty } getOrElse 0L
    val swarmed = swarm.map { case (age, qty) => getOlder(age) -> qty } :+ (NewBorn, mommies)

    swarmed.foldLeft(Map[Int, Long]()) { case (acc, (age, qty)) =>
      val addedQty: Long = acc.getOrElse(age, 0)
      acc + (age -> (addedQty + qty))
    }.toList
  }

  def daysGone(swarm: FishCount, days: Int): IO[FishCount] =
    for {
      swarm <-
        if (days > 0) {
          for {
            swarm <- dayGone(swarm)
            swarm <- daysGone(swarm, days - 1)
          } yield swarm
        } else {
          IO.pure(swarm)
        }
    } yield swarm

  val program = for {
    initialSwarm <- readNumbers("day6/input.txt")
    //initialSwarm <- readNumbers("day6/input_example.txt")

    groupedSwarm = initialSwarm.groupBy(identity)
      .view
      .mapValues[Long](_.length)
      .toList

    swarm1 <- daysGone(groupedSwarm, 80)
    swarm2 <- daysGone(groupedSwarm, days = 256)

    _ <- printAny( swarm1.toMap.values.sum )
    _ <- printAny( swarm2.toMap.values.sum )
  } yield ()
}

object MyApp extends IOApp.Simple {
  val run = Solution.program
}
