import aoc2016.day2.dirs

import scala.collection.mutable
import scala.io.Source
import Stream._
import scala.annotation.tailrec
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
    val input = Source.fromFile("data2017/4").getLines()

    val phrases = input.map(_.split(" ")).toList

    val part1 = phrases.count(_.groupBy(identity).forall(_._2.length == 1))

    val part2 = phrases.count(_.map(_.sorted).groupBy(identity).forall(_._2.length == 1))
  }

  object day5 {
    val input = Source.fromFile("data2017/5").getLines().map(Integer.parseInt).toArray

    def step1(offset: Int, input: Array[Int], steps: Int = 0): Int = {
      val newOffset = offset + input(offset)
      input(offset) += 1
      if (newOffset < 0 || newOffset >= input.length) steps+1 else step1(newOffset, input, steps+1)
    }

    def step2(offset: Int, input: Array[Int], steps: Int = 0): Int = {
      val newOffset = offset + input(offset)
      input(offset) += (if (input(offset) >= 3) -1 else 1)
      if (newOffset < 0 || newOffset >= input.length) steps+1 else step2(newOffset, input, steps+1)
    }

    val part1 = step1(0, input)
    val part2 = step2(0, input)
  }

  object day6 {
    val input = Source.fromFile("data2017/6").getLines().toList.head.split("\\t").map(Integer.parseInt)

    def distribute1(banks: Array[Int], steps: Int = 0, seen: Set[Seq[Int]] = Set()): Int = {
      val (max, ix) = banks.zipWithIndex.foldLeft((-1, -1)) {
        case (best@(bestCount, bestIx), bank@(bankCount, bankIx)) => if (bankCount > bestCount) bank else best
      }
      val newBanks = banks.clone()
      newBanks(ix) -= max
      (ix+1 until ix+max+1).foreach{ i => newBanks(i % newBanks.length) += 1 }
      if (seen.contains(newBanks))
        steps+1
      else
        distribute1(newBanks, steps+1, seen + newBanks)
    }

    @tailrec
    def distribute2(banks: Array[Int], steps: Int = 0, seen: Map[Seq[Int], Int] = Map()): Int = {
      val (max, ix) = banks.zipWithIndex.foldLeft((-1, -1)) {
        case (best@(bestCount, bestIx), bank@(bankCount, bankIx)) => if (bankCount > bestCount) bank else best
      }
      val newBanks = banks.clone()
      newBanks(ix) -= max
      (ix+1 until ix+max+1).foreach{ i => newBanks(i % newBanks.length) += 1 }
      val seenBefore = seen.get(newBanks)
      if (seenBefore.nonEmpty)
        steps - seenBefore.get
      else
        distribute2(newBanks, steps+1, seen + (newBanks.toSeq -> steps))

      // Would like to replace last four lines above with the below line, but it's not tail recursive...
      // seen.get(newBanks).map(steps - _).getOrElse(distribute2(newBanks, steps+1, seen + (newBanks.toSeq -> steps)))
    }

    //val part1 = distribute1(input)
    val part2 = distribute2(input)

  }

  object day7 {
    val input = Source.fromFile("data2017/7").getLines().toList

    val p = """(\w+) \((\d+)\)""".r

    def parse(line: String) = {
      val parts = line.split(" -> ")
      parts.head match {
        case p(name, weight) => Disc(name, weight.toInt, if (parts.length > 1) parts(1).split(", ").toList else Nil)
      }
    }

    case class Disc(name: String, weight: Int, above: Iterable[String])

    val discs = input.map(parse)

    val part1 = (discs.map(_.name).toSet -- discs.flatMap(_.above).toSet).head

    val discMap = discs.map(disc => disc.name -> disc).toMap

    def aboveWeights(disc: Disc) = disc.above.map(discMap).map(weight).sum
    def weight(disc: Disc): Int = disc.weight + aboveWeights(disc)

    def findFaulty(disc: Disc, diff: Int): Option[Int] = {
      checkBalance(disc).orElse(Some(disc.weight + diff))
    }

    def checkBalance(disc: Disc): Option[Int] = {
      val discsAbove = disc.above.map(discMap)
      val weights = discsAbove.map(weight)
      if (weights.nonEmpty && weights.min != weights.max) {
        val groups = discsAbove.zip(weights).groupBy(_._2).values
        val bad = groups.filter(_.size == 1).head.head
        val oneGood = groups.filter(_.size > 1).head.head
        findFaulty(bad._1, oneGood._2 - bad._2)
      } else
        None
    }

    val part2 = discs.flatMap(checkBalance).head
  }

  def main(args: Array[String]) {
    println(day7.part2)
   // println(day4.part2)
  }
}
