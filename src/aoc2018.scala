import java.lang.Math._

import scala.Stream._
import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator._

object aoc2018 {
  object day1 {
    val input = Source.fromFile("data2018/1").getLines()
    val numbers = input.map(_.toInt).toList

    val part1 = numbers.sum

    val part2 =
      Stream.continually(numbers).flatten
      .scanLeft(0)(_ + _)
      .scanLeft((None: Option[Int], Set[Int]())) {
        case ((r: Option[Int], s: Set[Int]), n: Int) =>
          if (s.contains(n)) (Some(n), s) else (None, s + n)
      }
      .find(_._1.isDefined).get._1
  }

  def main(args: Array[String]) {
    println(day1.part1)
    println(day1.part2)
  }
}
