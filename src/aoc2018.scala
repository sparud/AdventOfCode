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

  object day2 {
    val input = Source.fromFile("data2018/2").getLines().toList

    val counts = input.map(_.foldLeft(Map[Char, Int]().withDefaultValue(0)){ case (a, b) => a + (b -> (a(b)+1))}.values)

    val part1 = counts.map(c => Seq(c.exists(2 ==), c.exists(3 ==))).transpose.map(_.count(identity)).product

    def common(s1: String, s2: String) = {
      val equalParts = s1.zip(s2).filter{ case (c1, c2) => c1 == c2}
      if (equalParts.size == s1.length-1) Some(equalParts.map(_._1).mkString) else None
    }

    def triangulate[A](values: List[A]): Stream[(A, A)] = values match {
      case Nil => Stream.empty
      case v :: vs => vs.map((v, _)).toStream ++ triangulate(vs)
    }

    val part2 = triangulate(input).flatMap(p => common(p._1, p._2)).head
  }

  object day3 {
    val input = Source.fromFile("data2018/3").getLines().toList

    val pattern = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

    val claims = input flatMap {
      case pattern(id, x0, y0, w, h) =>
        (x0.toInt until x0.toInt+w.toInt).flatMap(x => (y0.toInt until y0.toInt+h.toInt).map(y => (id, (x, y))))
    }

    val fabric = claims.foldLeft(Map[(Int, Int), Int]().withDefaultValue(0)){
      case (map, (_, p)) => map + (p -> (map(p)+1))
    }

    val part1 = fabric.values.count(_ > 1)

    val fabricIds = claims.foldLeft(Map[(Int, Int), List[String]]().withDefaultValue(Nil)){
      case (map, (id, p)) => map + (p -> (id :: map(p)))
    }

    val fabricBuddies = fabricIds.values.foldLeft(Map[String, Set[String]]().withDefaultValue(Set.empty)) {
      case (map, ids) => map ++ ids.map(id => id -> (map(id) ++ ids))
    }

    val part2 = fabricBuddies.values.find(_.size == 1).get.head
  }

  object day4 {
    val input = Source.fromFile("data2018/4").getLines().toList

    val p = """\[[^ ]+ \d+:(\d+)\] ([^ ]+) ([^ ]+).*""".r
    val sleep = input.sorted.foldLeft((None: Option[Int], None: Option[Int], Map[Int, Map[Int, Int]]())) {
      case ((who, start, state), p(time, a, b)) => a match {
        case "Guard" => (Some(b.substring(1).toInt), None, state)
        case "falls" => (who, Some(time.toInt), state)
        case "wakes" =>
          val whoMap = state.getOrElse(who.get, Map[Int, Int]().withDefaultValue(0))
          (who, start, state + (who.get ->  (whoMap ++ (start.get until time.toInt).map(m => m -> (whoMap(m)+1)))))
      }
    }._3

    val (sleeper, sleepTimes) = sleep.maxBy(_._2.values.sum)
    val part1 = sleeper * sleepTimes.maxBy(_._2)._1

    val (sleeper2, sleepTimes2) = sleep.maxBy(_._2.toList.maxBy(_._2)._2)
    val part2 = sleeper2 * sleepTimes2.maxBy(_._2)._1
  }

  object day5 {
    val input = Source.fromFile("data2018/5").mkString

    def react(s: String) = s.foldLeft(List[Char]()) {
      case (Nil, c) => List(c)
      case (c1 :: rest, c2) if (c1 ^ c2) == 32 => rest
      case (rest, c) => c :: rest
    }.length

    val part1 = react(input)

    val part2 = input
      .map(_.toUpper)
      .toSet
      .map((c: Char) => react(input.replace(c+"", "").replace(c.toLower+"", "")))
      .min
  }

  object day6 {
    val input = Source.fromFile("data2018/6").getLines.toList

    case class Coord(x: Int, y: Int) {
      def distance(otherX: Int, otherY: Int) = Math.abs(otherX - x) + Math.abs(otherY-y)
    }
    case object Coord {
      def apply(s: String): Coord = s.split(", ") match {
        case Array(x, y) => Coord(x.toInt, y.toInt)
      }
    }

    val coordinates = input.map(Coord.apply)
    val topLeft = Coord(coordinates.map(_.x).min, coordinates.map(_.y).min)
    val bottomRight = Coord(coordinates.map(_.x).max, coordinates.map(_.y).max)

    val grid = (topLeft.x to bottomRight.x).map(x =>
      (topLeft.y to bottomRight.y).map { y =>
        val (dist, which) = coordinates.map(_.distance(x, y)).zipWithIndex.groupBy(_._1).toList.minBy(_._1)
        if (which.size > 1) -1 else which.head._2
      })

    val infinite = (grid.head ++ grid.last ++ grid.map(_.head) ++ grid.map(_.last)).toSet

    val part1 = grid
      .flatten
      .groupBy(identity)
      .mapValues(_.size)
      .toSeq
      .filter(p => !infinite.contains(p._1))
      .maxBy(_._2)
      ._2


    val grid2 = (topLeft.x to bottomRight.x).flatMap(x =>
      (topLeft.y to bottomRight.y).map { y =>
        coordinates.map(_.distance(x, y)).sum < 10000
      }).count(identity)

    val part2 = grid2
  }

  object day7 {
    val input = Source.fromFile("data2018/7").getLines.toList
    val pattern = """Step (.) must be finished before step (.) can begin.""".r

    val rules = input map {
      case pattern(from, to) => (from, to)
    }

    val dependencies = rules.groupBy(_._2).mapValues(_.map(_._1).mkString).toList.map(_.swap)

    val startState = dependencies ++ (dependencies.flatMap(_._1).toSet -- dependencies.map(_._2(0))).map("" -> _.toString)

    val part1 = startState.foldLeft((List[String](), startState.sorted)) {
      case ((builder, ("", step)::rest), _) =>
        (step :: builder, rest.map(p => (p._1.replace(step, ""), p._2)).sorted)
    }._1.reverse.mkString

    val NWORKERS = 5
    val EXTRA = 60

    def iterate(state: List[(String, String)], time: Int = 0, workers: List[(String, Int)] = Nil): Int = {
      if (state.isEmpty && workers.isEmpty)
        time
      else {
        val (ready, rest) = state.partition(_._1.isEmpty)
        val newWorkers = workers ++ ready.take(NWORKERS - workers.size).map(_._2).map(s => (s, s(0).toInt - 64 + EXTRA))
        val sortedWorkers = newWorkers.groupBy(_._2).mapValues(_.map(_._1).mkString).toList.sorted
        val (delta, steps, workingWorkers) = sortedWorkers.headOption.map {
          case (delta, steps) => (delta, steps.mkString, sortedWorkers.tail.flatMap(p => p._2.map(c => (c.toString, p._1))))
        }.getOrElse((0, "", Nil))
        iterate(
          ready.drop(NWORKERS - workers.size) ++ rest.map(p => ((p._1.toSet -- steps).mkString, p._2)),
          time + delta,
          workingWorkers.map(p => (p._1, p._2-delta)))
      }
    }

    val part2 = iterate(startState)
  }

  object day8 {
    val input = Source.fromFile("data2018/8").mkString.split(" ").map(_.toInt)

    case class Node(children: Seq[Node], metadata: Seq[Int]) {
      def sumMetadata(): Int = metadata.sum + children.map(_.sumMetadata()).sum
      def value(): Int = if (children.isEmpty)
          sumMetadata()
        else
          metadata.map(ix => if (ix > 0 && ix <= children.size) children(ix-1).value() else 0).sum
    }

    object Node {
      def apply(numbers: Iterator[Int]): Node = {
        val nChildren = numbers.next()
        val nMetadata = numbers.next()
        Node((1 to nChildren).map(_ => Node.apply(numbers)), (1 to nMetadata).map(_ => numbers.next()))
      }
    }

    val part1 = Node.apply(input.toIterator).sumMetadata()
    val part2 = Node.apply(input.toIterator).value()
  }

  object day9 {
    case class Entry[A](value: A, var prev: Entry[A] = null, var next: Entry[A] = null) {
      if (prev == null)
        prev = this
      if (next == null)
        next = this

      def insert(value: A): Entry[A] = {
        val entry = Entry(value, prev, this)
        prev.next = entry
        this.prev = entry
        entry
      }
      def remove(): (A, Entry[A]) = {
        prev.next = next
        next.prev = prev
        (value, next)
      }
      def moveCounterClockwise(steps: Int): Entry[A] = (1 to steps).foldLeft(this){ case (entry, _) => entry.prev }
      def moveClockwise(steps: Int): Entry[A] = (1 to steps).foldLeft(this){ case (entry, _) => entry.next }
    }

    def play(nPlayers: Int, endMarble: Int) = {
      val players = new Array[Long](nPlayers)

      def iterate(round: Int = 1, circle: Entry[Int] = Entry(0)): Long = {
        if (round > endMarble) {
          players.max
        } else if (round % 23 == 0) {
          val entry = circle.moveCounterClockwise(7)
          val (removed, nextEntry) = entry.remove()
          players(round % nPlayers) += removed + round
          iterate(round + 1, nextEntry)
        } else {
          iterate(round + 1, circle.moveClockwise(2).insert(round))
        }
      }

      iterate()
    }

    val part1 = play(410, 72059)

    val part2 = play(410, 72059*100)
  }

  object day10 {
    val input = Source.fromFile("data2018/10").getLines.toList

    val pattern = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r

    case class Point(x: Int, y: Int, dx: Int, dy: Int) {
      def step() = Point(x+dx, y+dy, dx, dy)
    }

    val startPoints = input.map {
      case pattern(x, y, dx, dy) => Point(x.toInt, y.toInt, dx.toInt, dy.toInt)
    }

    def minimizeHeight(points: List[Point], time: Int = 0, oldHeight: Int = Int.MaxValue): (Int, List[Point]) = {
      val next = points.map(_.step())
      val height = next.maxBy(_.y).y - next.minBy(_.y).y
      if (height > oldHeight)
        (time, points)
      else
        minimizeHeight(next, time+1, height)
    }

    val (time, points) = minimizeHeight(startPoints)

    val minY = points.minBy(_.y).y
    val maxY = points.maxBy(_.y).y
    val minX = points.minBy(_.x).x
    val maxX = points.maxBy(_.x).x
    val height = maxY-minY
    val width = maxX-minX

    val dots = points.map(p => (p.x-minX, p.y-minY)).toSet
    (0 to height).foreach{y =>
      (0 to width).foreach(x => print(if (dots.contains((x, y))) "#" else "."))
      println()
    }

   val part2 = time
  }

  def main(args: Array[String]) {
    println(day10.part2)
  }
}
