package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day7 {
  val input = Source.fromFile("data2023/7").getLines()

  class Ranker {
    def rank(card: String) = {
      val groups = fixGroups(card.groupBy(identity))
      val size = groups.size
      if (size == 1)
        7
      else if (size == 2)
        if (Set(1, 4).contains(groups.values.head.length)) 6 else 5
      else if (size == 3)
        if (groups.values.map(_.length).max == 3) 4 else 3
      else if (size == 4)
        2
      else
        1
    }

    def fixGroups(groups: Map[Char, String]) = groups

    val cardRanks = "23456789TJQKA".zipWithIndex.toMap

    def compareCardRanks(card1: String, card2: String) =
      card1.map(cardRanks).zip(card2.map(cardRanks))
        .map { case (c1, c2) => if (c1 > c2) 1 else if (c2 > c1) -1 else 0 }
        .dropWhile(_ == 0)
        .head

    def sort(card1: String, card2: String) = {
      val rank1 = rank(card1)
      val rank2 = rank(card2)
      rank1 < rank2 || rank1 == rank2 && compareCardRanks(card1, card2) < 0
    }

    def calculateScore(cardBids: List[Array[String]]) =
      cardBids.sortWith { case (p1, p2) => sort(p1(0), p2(0)) }
        .zipWithIndex
        .map { case (cb, r) => cb(1).toInt * (r + 1) }
        .sum
  }

  val cardBids = input.toList.map(_.split(" "))

  val part1 = new Ranker().calculateScore(cardBids)

  class Ranker2 extends Ranker {
    override val cardRanks = "J23456789TQKA".zipWithIndex.toMap

    override def fixGroups(groups: Map[Char, String]): Map[Char, String] = {
      if (groups.contains('J')) {
        val (js, other) = groups.toList.partition(p => p._1 == 'J')
        if (other.isEmpty)
          Map('J' -> "JJJJJ")
        else {
          val ordered = other.toList.sortBy(-_._2.length)
          (ordered.headOption
            .map(p => (p._1, p._2 + p._1.toString * js.head._2.length)).get ::
            ordered.tail).toMap
        }
      } else
        groups
    }
  }

  val part2 = new Ranker2().calculateScore(cardBids)
}

object Day7Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day7.part1)
    println(LocalDateTime.now())
  }
}
