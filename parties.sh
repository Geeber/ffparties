#!/usr/bin/env scala

import scala.math.Ordering.Implicits._

sealed trait Member {
  def shortName: String
}
case object Fighter extends Member {
  val shortName = "Fi"
}
case object Thief extends Member {
  val shortName = "Th"
}
case object BlackBelt extends Member {
  val shortName = "Bb"
}
case object RedMage extends Member {
  val shortName = "Rm"
}
case object WhiteMage extends Member {
  val shortName = "Wm"
}
case object BlackMage extends Member {
  val shortName = "Bm"
}

case class Party(index: Int, members: Seq[Member]) {

  def score(): Int = {
    (if (hasFighter) 20 else 0) +
    (if (hasWhiteMagic) 10 else 0) +
    (if (hasBlackMagic) 12 else 0) +
    (if (hasStrongMelee) 10 else 0) +
    variety
  }

  override def toString(): String = members.map(_.shortName).mkString
  def toCsvLine(): String = Seq(index, toString(), score()).mkString(",")

  private def hasFighter: Boolean = members.contains(Fighter)
  private def hasWhiteMagic: Boolean =
    members.contains(WhiteMage) || members.contains(RedMage)
  private def hasBlackMagic: Boolean =
    members.contains(BlackMage) || members.contains(RedMage)
  private def hasStrongMelee: Boolean =
    members.contains(Fighter) || members.contains(BlackBelt)
  private def variety: Int = members.toSet.size

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
  allParties.sortBy(_.score).foreach { p =>
    println(p.toCsvLine)
  }
}

FinalFantasyParties.main(args)
