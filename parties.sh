#!/usr/bin/env scala

import scala.math.Ordering.Implicits._

case class PowerProfile(
    stOffense: Double,
    mtOffense: Double,
    defense: Double,
    healing: Double,
    life: Boolean,
    dungeonExit: Boolean
  )

sealed trait Member {
  def shortName: String
  def earlyGame: PowerProfile // Approx. lvl 1-10, up through Marsh Cave/Astos
  def midGame: PowerProfile // Approx. level 11-17, up through Ice Cave/RoC
  def lateGame: PowerProfile // Approx. level 18+
}
case object Fighter extends Member {
  val shortName = "Fi"
  val earlyGame = PowerProfile(1.0, 0.0, 1.0, 0.0, false, false)
  val midGame = PowerProfile(0.9, 0.0, 1.0, 0.0, false, false)
  val lateGame = PowerProfile(0.75, 0.0, 1.0, 0.05, false, false)
}
case object Thief extends Member {
  val shortName = "Th"
  val earlyGame = PowerProfile(0.4, 0.0, 0.3, 0.0, false, false)
  val midGame = PowerProfile(0.4, 0.0, 0.3, 0.0, false, false)
  val lateGame = PowerProfile(0.5, 0.05, 0.5, 0.0, false, false)
}
case object BlackBelt extends Member {
  val shortName = "Bb"
  val earlyGame = PowerProfile(0.4, 0.0, 0.3, 0.0, false, false)
  val midGame = PowerProfile(1.0, 0.0, 0.3, 0.0, false, false)
  val lateGame = PowerProfile(1.0, 0.0, 0.4, 0.0, false, false)
}
case object RedMage extends Member {
  val shortName = "Rm"
  val earlyGame = PowerProfile(0.75, 0.7, 0.75, 0.7, false, false)
  val midGame = PowerProfile(0.5, 0.6, 0.5, 0.5, false, false)
  val lateGame = PowerProfile(0.4, 0.4, 0.5, 0.3, true, true)
}
case object WhiteMage extends Member {
  val shortName = "Wm"
  val earlyGame = PowerProfile(0.1, 0.0, 0.1, 1.0, false, false)
  val midGame = PowerProfile(0.1, 0.0, 0.1, 1.0, true, false)
  val lateGame = PowerProfile(0.1, 0.1, 0.1, 1.0, true, true)
}
case object BlackMage extends Member {
  val shortName = "Bm"
  val earlyGame = PowerProfile(0.1, 1.0, 0.1, 0.0, false, false)
  val midGame = PowerProfile(0.1, 1.0, 0.1, 0.0, false, false)
  val lateGame = PowerProfile(0.1, 1.0, 0.1, 0.0, false, true)
}

case class Party(index: Int, members: Seq[Member]) {

  def score(): Double = {
    earlyGameScore
  }

  def dotProduct(as: Seq[Double], bs: Seq[Double]): Double =
    (for ((a, b) <- as zip bs) yield a * b) sum

  private val StOffenseWeights = Seq(1.0, 0.8, 0.6, 0.6)
  private val MtOffenseWeights = Seq(1.0, 0.6, 0.4, 0.4)
  private val DefenseWeights = Seq(1.0, 0.5, 0.25, 0.25)
  private val HealingWeights = Seq(1.0, 0.8, 0.5, 0.5)
  private val LifeWeights = Seq(1.0, 0.2, 0.1, 0.05)
  private val DungeonExitWeights = Seq(1.0, 0.1, 0.05, 0.05)
  private val EarlyGameWeights = Seq(1.0, 1.0, 1.0, 0.5, 0.0, 0.0)

  def earlyGameScore(): Double = {
    val earlyMembers = members.map(_.earlyGame)

    // Compute single target offense.
    val stOffenses = earlyMembers.map(_.stOffense).sorted
    val totalStOffense = dotProduct(stOffenses, StOffenseWeights)

    // Compute multi-target offense.
    val mtOffenses = earlyMembers.map(_.mtOffense).sorted
    val totalMtOffense = dotProduct(mtOffenses, MtOffenseWeights)

    // Compute defense.
    val defenses = earlyMembers.map(_.defense).sorted
    val totalDefense = dotProduct(defenses, DefenseWeights)

    // Compute healing.
    val healings = earlyMembers.map(_.healing).sorted
    val totalHealing = dotProduct(healings, HealingWeights)

    // Compute total score.
    dotProduct(Seq(totalStOffense, totalMtOffense, totalDefense, totalHealing, 0.0, 0.0),
               EarlyGameWeights)
  }

  override def toString(): String = members.map(_.shortName).mkString
  def toCsvLine(): String = Seq(index, toString(), score()).mkString(",")

}

object FinalFantasyParties extends App {

  val Classes = Array(Fighter, Thief, BlackBelt, RedMage, WhiteMage, BlackMage)

  val zeroToFive = 0 to 5
  val allCombinations = for (
    p1 <- zeroToFive;
    p2 <- zeroToFive;
    p3 <- zeroToFive;
    p4 <- zeroToFive
  ) yield Seq(p1, p2, p3, p4).sorted
  val allParties = allCombinations.toSet.toSeq.sorted.zipWithIndex.map { case (p, i) =>
    Party(i, p.map(Classes(_)))
  }
  println(s"allParties.size = ${allParties.size}")
  allParties.sortBy(_.score).reverse.foreach { p =>
    println(p.toCsvLine)
  }
}

FinalFantasyParties.main(args)
