import aoc2016.day2.dirs

import scala.collection.mutable
import scala.io.Source
import Stream._

import scala.math.pow

object aoc2017 {
  object day1 {
    val input = Source.fromFile("data2017/1").mkString
    val numbers = input.map(_.toInt - '0')

    val part1 = (numbers :+ numbers(0)).sliding(2).map(p => if (p(0) == p(1)) p(0) else 0).sum

    val part2 = (numbers ++ numbers.slice(0, numbers.size/2)).sliding(numbers.size/2+1)
      .map(p => if (p(0) == p.last) p(0) else 0).sum
  }

  object day2 {
    val input = Source.fromFile("data2017/2").getLines()
    val numberRows = input.map(_.split("\\t").map(Integer.parseInt)).toList
    val part1 = numberRows.map(row => row.max - row.min).sum

    def check: Array[Int] => Int = {case Array(a, b) => if (b % a == 0) b / a else 0}
    val part2 = numberRows.map(_.sorted.combinations(2).map(p => check(p)).sum).sum
  }

  object day3 {
    val input = 277678

    def distance(n: Int, layer: Int = 1, acc: Int = 1): Int =
      if (acc >= n) Math.abs(((acc-n)%(layer*4)-layer+1)%layer) + layer - 1 else distance(n, layer+1, acc + layer*8)

    val part1 = distance(input)

    case class Pos(x: Int, y: Int) {
      def +(other: Pos) = Pos(x+other.x, y+other.y)
      def length = Math.abs(x) + Math.abs(y)
      def neighbors =
        Seq(Pos(x+1, y-1), Pos(x+1, y), Pos(x+1, y+1), Pos(x, y+1), Pos(x, y-1), Pos(x-1, y-1), Pos(x-1, y), Pos(x-1, y+1))
    }

    def move(dir: Pos, n: Int, start: Pos): Stream[Pos] =
      (1 to n).scanLeft(start) { case (a, _) => a + dir }.toStream.tail

    def loop(n: Int = 1, start: Pos = Pos(0, 0)): Stream[Pos] = {
      val out   = move(Pos(1, 0),  1,   start)
      val up    = move(Pos(0, -1), 2*n-1, out.last)
      val left  = move(Pos(-1, 0), 2*n, up.last)
      val down  = move(Pos(0, 1),  2*n, left.last)
      val right = move(Pos(1, 0),  2*n, down.last)
      lazy val next = loop(n + 1, right.last)
      out #::: up #::: left #::: down #::: right #::: next
    }

    val positions = Pos(0, 0) #:: loop()

    val part1x = positions(input-1).length

    val sumMaps = positions.tail.scanLeft(Map(Pos(0, 0) -> 1)) {
      case (m, p) => m + (p -> p.neighbors.map(m.getOrElse(_, 0)).sum)
    }

    val part2 = sumMaps.map(_.values.max).find(_ > input).get
  }

  object day4 {
    val input = Source.fromFile("data2017/4").getLines().toList

    val part1 = input.count(_.split(" ").groupBy(identity).forall(_._2.length == 1))

    val part2 = input.count(_.split(" ").map(_.sorted).groupBy(identity).forall(_._2.length == 1))

  }

  def main(args: Array[String]) {
    println(day4.part1)
    println(day4.part2)
  }
}
