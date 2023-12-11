package Aoc2023
import java.time.LocalDateTime
import scala.io.Source
import scala.collection.mutable
import scala.math.pow

object Day4 {
  val input = Source.fromFile("data2023/4").getLines()

  def toNumbers(s: String) =
    s.split(" +").map(_.toInt).toSet

  val cards = input.map(row => row
    .split(": +")(1)
    .split(" +\\| +")
    .map(toNumbers)
  ).toList

  val part1 = cards.map { case Array(l1, l2) =>
    val hits = l1.intersect(l2).size
    if (hits > 0) pow(2, hits - 1).toInt else 0
  }.sum

  val state = mutable.Map[Int, Int]().withDefaultValue(1)
  val part2 = {
    cards.zipWithIndex.map { case (Array(l1, l2), ix) =>
      (1 to l1.intersect(l2).size).foreach(n => state(ix + n) += state(ix))
      state(ix)
    }.sum
  }
}

object Day4Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day4.part1)
    println(LocalDateTime.now())
  }
}
