import java.time.LocalDateTime
import scala.::
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.math.{BigInt, exp, pow}

object aoc2023 {
  object day1 {
    val input = Source.fromFile("data2023/1").getLines().toList

    def calibrate(s: String) = {
      val digits = s.filter(_.isDigit)
      ("" + digits(0) + digits(digits.length - 1)).toInt
    }

    val part1 = input.map(calibrate).sum

    val patterns = List(
      "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
    ).zipWithIndex.tail

   def transform(s: String): String =
     if (s == "")
       ""
     else if (s(0).isDigit)
       s(0) + transform(s.substring(1))
     else {
       val rest = transform(s.substring(1))
       patterns
         .find(p => s.startsWith(p._1))
         .map(_._2 + rest)
         .getOrElse(rest)
     }

    def calibrate2(s: String) = calibrate(transform(s))

    val part2 = input.map(calibrate2).sum
  }

  object day2 {
    case class CubeSet(red: Int = 0, green: Int = 0, blue: Int = 0) {
      def +(c: CubeSet): CubeSet = CubeSet(red+c.red, green+c.green, blue+c.blue)
      def -(c: CubeSet): CubeSet = CubeSet(red-c.red, green-c.green, blue-c.blue)
      def power: Int = red*green*blue
      def ok: Boolean = red >= 0 && green >= 0 && blue >= 0
    }
    object CubeSet {
      val zero = CubeSet(0, 0, 0)
      def parseCubes(s: String): CubeSet =
        s.split(", ").map { n_color =>
          val Array(n, color) = n_color.split(" ")
          color match {
            case "red" => CubeSet(red=n.toInt)
            case "green" => CubeSet(green=n.toInt)
            case "blue" => CubeSet(blue=n.toInt)
          }
        }.foldLeft(zero)(_ + _)
    }

    case class Game(nr: Int, boxes: List[CubeSet]) {
      def gamePower = CubeSet(
        boxes.map(_.red).max,
        boxes.map(_.green).max,
        boxes.map(_.blue).max
      ).power
    }

    val input = Source.fromFile("data2023/2").getLines()

    val input2 =
      """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin.split("\n")


    println(input)

    val games = input.map(line => {
      val Array(gamePart, rest) = line.split(": ")
      Game(gamePart.split(" ")(1).toInt, rest.split("; ")
        .map(CubeSet.parseCubes)
        .toList)
    }).toList

    val allCubes = CubeSet(12, 13, 14)
    val part1 = games.filter(_.boxes.forall(box => (allCubes - box).ok)).map(_.nr).sum

    val part2 = games.map(_.gamePower).sum
  }

  object day6 {
    val input = Source.fromFile("data2023/6").getLines()
    def parse() = input.next().split("[\t ]+").tail.map(_.toInt)
    val times = parse()
    val distances = parse()
    val tasks = times.zip(distances)

    def countOk(time: Int, distance: Int): Unit = {
      println(s"Time: $time, distance: $distance")
    }

    def getDistances(time: Int) = (1 until time).map(lt => (time-lt)*lt)

    val part1 = tasks.map { case (t, d) =>
      getDistances(t).count(_ > d)
    }.product

    @inline
    def getDistance(totalTime: Int, chargeTime: Int): BigInt =
      BigInt(1)*(totalTime-chargeTime)*chargeTime

    def find(totalTime: Int, bestDistance: BigInt) = {

      def divide_and_conquer(condition: BigInt => Boolean, start: Int, stop: Int): Int = {
        if (stop - start < 2)
          stop
        else {
          val middle = (start + stop) / 2
          val distance = getDistance(totalTime, middle)
          if (condition(distance))
              divide_and_conquer(condition, middle, stop)
          else
              divide_and_conquer(condition, start, middle)
        }
      }
      divide_and_conquer(_ > bestDistance, totalTime/2, totalTime) - divide_and_conquer(_ <= bestDistance, 1, totalTime/2)
    }

    val part2time = (times.map(_.toString)).mkString("").toInt
    val part2distance = BigInt(distances.map(_.toString).mkString(""))

    val part2 = find(part2time, part2distance)

  }

  object day7 {
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

    val input2 = """32T3K 765
                  |T55J5 684
                  |KK677 28
                  |KTJJT 220
                  |QQQJA 483
                  |""".stripMargin.split("\n")
    val cardBids = input.toList.map(_.split(" "))

    val part1 = new Ranker().calculateScore(cardBids)

    class Ranker2 extends Ranker {
      override val cardRanks = "J23456789TQKA".zipWithIndex.toMap

      override def fixGroups(groups: Map[Char, String]): Map[Char, String] = {
        if (groups.contains('J')) {
          val (js, other) = groups.toList.partition(p => p._1 == 'J')
          if (other.isEmpty)
            Map('J'-> "JJJJJ")
          else {
            val ordered = other.toList.sortBy(-_._2.length)
            (ordered.headOption
              .map(p => (p._1, p._2 + p._1.toString*js.head._2.length)).get ::
              ordered.tail).toMap
          }
        } else
          groups
      }
    }

    val part2 = new Ranker2().calculateScore(cardBids)
  }

  object day3 {
    val input = Source.fromFile("data2023/3").getLines()
    val inner = input.toArray
    val board = "." * inner(0).length +: inner.map(row => "." + row + ".") :+ "." * inner(0).length

    val numbers = board.zipWithIndex.flatMap{ case (row, i) =>
      row.zipWithIndex.foldLeft((List(): List[List[((Int, Int), Int)]], "")){ case ((state, sofar), (col, j)) =>
        if (col.isDigit)
          (state, sofar+col)
        else if (sofar.nonEmpty)
          (state :+ (j-sofar.length until j).toList.map(c => ((i, c), sofar.toInt)), "")
        else
          (state, sofar)
      }._1
    }.zipWithIndex
      .flatMap{ case (p, ix) => p.map(v => (v._1, (v._2, ix)))}
      .toMap

    println(numbers)
    val part1 = board.zipWithIndex.map({ case (row, i) =>
      row.zipWithIndex.map{ case (col, j) =>
        if (col != '.' && !col.isDigit)
          List(
            (i - 1, j - 1), (i - 1, j),    (i - 1, j + 1),
            (i,     j - 1), (i,     j + 1),
            (i + 1, j - 1), (i + 1, j),    (i + 1, j + 1)
          ).flatMap(coord => numbers.get(coord)).groupBy(_._2).map(_._2.head._1).sum
        else
          0
      }.sum
    }).sum

    val part2 = board.zipWithIndex.map({ case (row, i) =>
      row.zipWithIndex.map { case (col, j) =>
        if (col == '*') {
          val numGroups = List(
            (i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
            (i, j - 1), (i, j + 1),
            (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)
          ).flatMap(coord => numbers.get(coord)).groupBy(_._2)
          if (numGroups.size == 2)
            numGroups.values.map(_.head._1).product
          else
            0
        } else
          0
      }.sum
    }).sum
  }

  object day4 {
    val input = Source.fromFile("data2023/4").getLines()

    def toNumbers(s: String) =
      s.split(" +").map(_.toInt).toSet

    val cards = input.map(row => row
      .split(": +")(1)
      .split(" +\\| +")
      .map(toNumbers)
    ).toList

    val part1 = cards.map{ case Array(l1, l2) =>
      val hits = l1.intersect(l2).size
      if (hits > 0) pow(2, hits-1).toInt else 0
    }.sum

    val state = mutable.Map[Int, Int]().withDefaultValue(1)
    val part2 = {
      cards.zipWithIndex.map { case (Array(l1, l2), ix) =>
        (1 to l1.intersect(l2).size).foreach(n => state(ix+n) += state(ix))
        state(ix)
      }.sum
    }
  }

  object day5 {
    val input = Source.fromFile("data2023/5").mkString.split("\n\n")

    abstract class Mapper {
      def apply(in: BigInt): BigInt
    }

    object IdentityMapper extends Mapper {
      override def apply(in: BigInt): BigInt = in
    }

    case class RangeMapper(dst: BigInt, src: BigInt, len: BigInt, parent: Mapper) extends Mapper {
      override def apply(in: BigInt): BigInt =
        if (in >= src && in < src+len) dst+(in-src) else parent(in)
    }

    val seeds = input
      .head
      .split("\n")
      .head
      .split(": ")(1)
      .split(" ").map(BigInt(_)).toList

    val maps = input.tail.map { section =>
      val _ :: conversions = section.split("\n").toList
      conversions.reverse.foldLeft(IdentityMapper: Mapper) { case (state, line) =>
        val numbers = line.split(" ").map(BigInt(_))
        RangeMapper(numbers(0), numbers(1), numbers(2), state)
      }
    }

    def seedLocation(seed: BigInt) = maps.foldLeft(seed){ (current, m) => m(current)}

    val part1 = seeds.map(seedLocation).min

    val part2 = seeds.grouped(2).flatMap{ case (start :: len :: _) => start to start + len }.map(seedLocation).min
  }

  object day8 {
    val input = Source.fromFile("data2023/8").getLines()

    val instructions = input.next()
    input.next()
    val states = input.map { row =>
      val Array(name, rest) = row.split(" = \\(")
      val Array(left, right) = rest.split(", |\\)")
      (name, Array(left, right))
    }.toMap

    def runInstructions(steps: Int, start: String, path: String): Either[(String, Int), Int] = {
      path.foldLeft(Left((start, steps)): Either[(String, Int), Int]){ case (state, instr) =>
        if (state.isLeft) {
          val next = states(state.left.get._1)(if (instr == 'L') 0 else 1)
          if (next == "ZZZ")
            Right(state.left.get._2)
          else
            Left(next, state.left.get._2+1)
        } else
          state
      }
    }

    def search(steps: Int, start: String): Int = {
      runInstructions(steps, start, instructions) match {
        case Left((pos, n)) => search(n, pos)
        case Right(n) => n+1
      }
    }

    val part1 = search(0, "AAA")

    def runInstructions2(steps: Int, start: String, path: String): Either[(String, Int), Int] = {
      path.foldLeft(Left((start, steps)): Either[(String, Int), Int]) { case (state, instr) =>
        if (state.isLeft) {
          val next = states(state.left.get._1)(if (instr == 'L') 0 else 1)
          if (next.endsWith("Z"))
            Right(state.left.get._2)
          else
            Left(next, state.left.get._2 + 1)
        } else
          state
      }
    }

    def search2(steps: Int, start: String): Int = {
      runInstructions2(steps, start, instructions) match {
        case Left((pos, n)) => search2(n, pos)
        case Right(n) => n + 1
      }
    }

    def lcm(a: BigInt, b: BigInt): BigInt = (a * b).abs / a.gcd(b)

    def lcmOfList(numbers: List[BigInt]): BigInt = numbers.foldLeft(BigInt(1))(lcm)

    val part2 = lcmOfList(states.keys.filter(_.endsWith("A")).map(start => search2(0, start)).map(BigInt(_)).toList)
  }

  object day10 {
    val input = Source.fromFile("data2023/10").getLines()
    val grid = input.map(_.toArray).toArray

    val width = grid(0).length
    val height = grid.length

    case class Pos(row: Int, col: Int) {
      def -(other: Pos) = Pos(row-other.row, col-other.col)
      def +(other: Pos) = Pos(row+other.row, col+other.col)
    }

    val steps = Map(
      (Pos(0, 1), '-') -> Pos(0, 1),
      (Pos(0, -1), '-') -> Pos(0, -1),
      (Pos(0, 1), '7') -> Pos(1, 0),
      (Pos(-1, 0), '7') -> Pos(0, -1),
      (Pos(1, 0), '|') -> Pos(1, 0),
      (Pos(-1, 0), '|') -> Pos(-1, 0),
      (Pos(1, 0), 'J') -> Pos(0, -1),
      (Pos(0, 1), 'J') -> Pos(-1, 0),
      (Pos(1, 0), 'L') -> Pos(0, 1),
      (Pos(0, -1), 'L') -> Pos(-1, 0),
      (Pos(-1, 0), 'F') -> Pos(0, 1),
      (Pos(0, -1), 'F') -> Pos(1, 0)
    )

    var startPos = Pos(0, 0)

    for (row <- grid.indices)
      for (col <- grid(0).indices)
        if (grid(row)(col) == 'S') {
          startPos = Pos(row, col)
        }

    def first(delta: Pos, pos: Pos) = {
      steps.get((delta, grid(pos.row+delta.row)(pos.col+delta.col)))
    }

    @tailrec
    def walk(oldPos: Pos, pos: Pos, acc: List[Pos] = Nil): List[Pos] = {
      val newPos = pos + steps((pos-oldPos, grid(pos.row)(pos.col)))
      if (newPos == startPos)
        newPos :: acc
      else
        walk(pos, newPos, newPos :: (if (acc.nonEmpty) acc else List(pos)))
    }

    val path = List(Pos(0, -1), Pos(-1, 0), Pos(0, 1), Pos(1, 0))
      .find(delta => first(delta, startPos).isDefined)
      .map(delta => walk(startPos, startPos + delta))
      .get


    val part1 = (path.length + 1) / 2

    var looped = 0

    for (row <- (0 until height)) {
      var inside = false
      var col = 0
      while (col < width) {
        var ch = grid(row)(col)
        if (path.contains(Pos(row, col))) {
          if (ch == '|') {
            inside = !inside
            col += 1
          } else if (ch == 'L' || ch == 'F') {
            var ix = col+1
            while (grid(row)(ix) == '-' || grid(row)(ix) == 'S')
              ix += 1
            if (ch == 'F' && grid(row)(ix) == 'J' || ch == 'L' && grid(row)(ix) == '7')
              inside = !inside
            col = ix + 1
          } else {
            col += 1
          }
        } else {
          if (inside) {
            looped += 1
          }
          col += 1
        }
      }
    }

    val part2 = looped
  }

  object day11 {
    val input = Source.fromFile("data2023/11").mkString.split("\n")

    val startGalaxies = input.zipWithIndex.flatMap { case (row, rowNo) =>
      row.zipWithIndex.flatMap{ case (col, colNo) =>
        if (col == '#') Some((BigInt(rowNo), BigInt(colNo))) else None}
    }

    val emptyRows = (0 until input.length).
      filter(r => !startGalaxies.map(_._1).contains(r)).sorted.map(BigInt(_))
    val emptyCols = (0 until input.head.length).
      filter(c => !startGalaxies.map(_._2).contains(c)).sorted.map(BigInt(_))

    def expandBy(axis: Seq[BigInt], empty: Seq[BigInt], factor: Int) =
      axis.foldLeft((empty, BigInt(0), Map(): Map[BigInt, BigInt])) { case ((empty, offset, m), v) =>
        val (less, more) = empty.span(_ < v)
        val newOffset = offset+less.size*factor
        (more, newOffset, m + (v->(v+newOffset)))
      }._3

    val rowMap = expandBy(startGalaxies.map(_._1).sorted, emptyRows, 1)
    val colMap = expandBy(startGalaxies.map(_._2).sorted, emptyCols, 1)

    val galaxies = startGalaxies.map{case (row, col) => (rowMap(row), colMap(col))}

    def distance(g1: (BigInt, BigInt), g2: (BigInt, BigInt)): BigInt =
      (g2._1 - g1._1).abs + (g2._2 - g1._2).abs

    val part1 = galaxies.combinations(2).map{ case Array(g1, g2) => distance(g1, g2)}.sum

    val rowMap2 = expandBy(startGalaxies.map(_._1).sorted, emptyRows, 999999)
    val colMap2 = expandBy(startGalaxies.map(_._2).sorted, emptyCols, 999999)

    val galaxies2 = startGalaxies.map { case (row, col) => (rowMap2(row), colMap2(col)) }

    val part2 = galaxies2.combinations(2).map{ case Array(g1, g2) => distance(g1, g2)}.sum
  }

  object day9 {
    val input = Source.fromFile("data2023/9").mkString.split("\n")
      .map(_.split(" ").map(_.toInt).toList)
    val input2 =
      """0 3 6 9 12 15
        |1 3 6 10 15 21
        |10 13 16 21 30 45
        |""".stripMargin
        .split("\n")
        .map(_.split(" ").map(_.toInt).toList)

    def getDiffLists(numbers: List[Int]): List[List[Int]] =
      if (numbers.forall(_ == 0)) Nil else numbers :: getDiffLists(numbers.sliding(2).map { case a :: b :: Nil => b - a }.toList)

    val part1 = input.map(getDiffLists(_).map(_.last).sum).sum

    def prev(numbers: List[Int]) =
      numbers.foldRight(0) { case (n, acc) => n - acc }

    val part2 = input.map(getDiffLists(_).map(_.head)).toList.map(prev).sum

  }

  def main(args: Array[String])  {
    println(LocalDateTime.now())
    println(day9.part2)
    println(LocalDateTime.now())
  }
}