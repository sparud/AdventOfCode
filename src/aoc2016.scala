import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter._

import scala.io.Source

object aoc2016 {
  object day1 {
    val turns = Map('L' -> -1, 'R' -> 1)
    val input = Source.fromFile("data2016/1").mkString.split(", ")
      .map(move => (turns(move(0)), move.substring(1).toInt)).toList
    val xdirs = Map(0 -> 0, 1 -> 1, 2 -> 0, 3 -> -1)
    val ydirs = Map(0 -> 1, 1 -> 0, 2 -> -1, 3 -> 0)
    val positions = input.scanLeft((0, List((0, 0)))){ case ((dir, coords), (turn, blocks)) =>
      val newdir = (dir + turn + 4) % 4
      val (x, y) = coords.last
      (newdir, (1 to blocks).map(steps => (x + xdirs(newdir)*steps, y + ydirs(newdir)*steps)).toList)
    }.flatMap(_._2)

    val (endX, endY) = positions.last
    val part1 = Math.abs(endX) + Math.abs(endY)

    val (endX2, endY2) = positions.foldLeft((Set[(Int, Int)](), List[(Int, Int)]())) {
      case ((seen, doubles), p) => (seen + p, if (seen.contains(p)) p :: doubles else doubles)
    }._2.last
    val part2 = Math.abs(endX2) + Math.abs(endY2)
  }

  object day2 {
    case class Pos(x: Int, y: Int) {
      def +(other: Pos) = Pos(x + other.x, y + other.y)
    }

    val dirs = Map('U' -> Pos(0, -1), 'R' -> Pos(1, 0), 'D' -> Pos(0, 1), 'L' -> Pos(-1, 0))
    val input = Source.fromFile("data2016/2").getLines.map(_.map(dirs)).toSeq

    def move(f: Pos => Boolean)(p1: Pos, p2: Pos) = if (f(p1+p2)) p1+p2 else p1

    def follow(f: Pos => Boolean)(start: Pos, steps: Seq[Pos]) = (start /: steps)(move(f))

    def code(f: Pos => Boolean, start: Pos, moves: Seq[Seq[Pos]]) = moves.scanLeft(start)(follow(f)).toList.tail

    def solve(keyboard: String, moves: Seq[Seq[Pos]]) = {
      val layout = keyboard.split(",").map(_.zipWithIndex).zipWithIndex
        .flatMap{ case (cols, y) => cols.flatMap { case (c, x) => if (c == ' ') None else Some(Pos(x, y) -> c) }}
        .toMap
      code(layout.contains, layout.map(_.swap).apply('5'), moves).map(layout).mkString
    }

    val part1 = solve("123,456,789", input)
    val part2 = solve("  1  , 234 ,56789, ABC ,  D  ", input)
  }

  object day3 {
    val input = Source.fromFile("data2016/3").getLines.toList.map(_.split(" +").tail.map(_.toInt).toList)

    def triangular(l: Seq[Int]) = l.sorted.take(2).sum > l.max

    val part1 = input.count(triangular)

    val part2 = input.grouped(3).flatMap(_.transpose).count(triangular)
  }

  object day4 {
    val p = """([a-z-]+)([0-9]+)\[([a-z]+)\]""".r
    def parse(s: String) = s match {
      case p(a, b, c) => (a.filter(_ != '-'), b.toInt, c)
    }

    val input = Source.fromFile("data2016/4").getLines.map(parse).toList

    def calcChecksum(s: String) =
      s.groupBy(identity).mapValues(-_.length).toList.map(_.swap).sorted.map(_._2).mkString.take(5)

    def ok(tuple: (String, Int, String)) = if (calcChecksum(tuple._1) == tuple._3) Some(tuple._2) else None

    val part1 = input.flatMap(ok).sum

    val transform = ('z' -> 'a' :: (('a' to 'y') map (c => c -> (c.toByte+1).toChar)).toList).toMap

    def repeat[A](n: Int, a: A, f: A => A) = (a /: (1 to n)) { case (b, _) => f(b) }

    def decode(tuple: (String, Int, String)) = repeat(tuple._2 % 26, tuple._1,  {s: String => s.map(transform)})

    val part2 = input.map(tuple => (tuple._2, decode(tuple))).find(_._2 == "northpoleobjectstorage").map(_._1).get
  }

  object day5 {
    val doorId = "reyedfim"

    def md5(s: String) = printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes))

    val part1 = Stream.from(0).map(n => md5(doorId + n)).filter(_.startsWith("00000")).take(8).map(_(5)).mkString

    val part2 = Stream.from(0).map(n => md5(doorId + n))
      .filter(h => h.startsWith("00000") && h(5) >= '0' && h(5) <= '7').scanLeft(List[(Char, Char)]()){
      case (state, hash) => if (state.map(_._1).contains(hash(5))) state else (hash(5), hash(6)) :: state
    }.find(_.size == 8).get.sorted.map(_._2).mkString
  }


  object day6 {
    val part1 = Source.fromFile("data2016/6")
      .getLines.toSeq.transpose.map(_.groupBy(identity).mapValues(_.size).map(_.swap).toSeq.sorted.last._2).mkString

    val part2 = Source.fromFile("data2016/6")
      .getLines.toSeq.transpose.map(_.groupBy(identity).mapValues(_.size).map(_.swap).toSeq.sorted.head._2).mkString

  }

  object day7 {
    def parse(s: String) = s.split("\\[|\\]").toList

    val input = Source.fromFile("data2016/7").getLines.map(parse).toList.map(_.map(_.toList))

    def everyother[T](isLeft: Boolean)(s: List[T]): (List[T], List[T]) = s match {
      case Nil => (List[T](), List[T]())
      case c :: rest =>
        val (left, right) = everyother(!isLeft)(rest)
        if (isLeft) (c :: left, right) else (left, c:: right)
    }

    def abba(s: List[Char]): Boolean = s match {
      case a :: b :: c :: d :: rest => a == d && b == c && a != b || abba(b :: c :: d :: rest)
      case _ => false
    }

    val part1 = input.map(everyother(isLeft=true)).count{ case (musts, mustnots) => musts.exists(abba) && !mustnots.exists(abba) }

    def aba(s: List[Char]) = s.sliding(3).flatMap { case l@(a :: b :: c :: Nil) => if (a == c && a != b) Some(l) else None }
    def aba2bab(s: String) = "" + s(1) + s(0) + s(1)
    def abasSet(parts: List[List[Char]]) = parts.flatMap(aba).map(_.mkString).toSet

    val part2 = input.map(everyother(isLeft=true)).count{ case (outer, inner) =>
      abasSet(outer).intersect(abasSet(inner).map(aba2bab)).nonEmpty
    }

  }

  object day8 {
    val input = Source.fromFile("data2016/8").getLines.toList
    val ROWS = 6
    val COLUMNS = 50

    val state = Array.fill(ROWS, COLUMNS)(false)

    def rotate[T](list: List[T]) = list.last :: list.slice(0, list.size-1)
    def repeat[T](n: Int, f: T => T, start: T) = (start /: (1 to n)){case p  => f(p._1)}

    input.foreach(_.split(" ") match {
      case Array("rotate", "column", xeq, "by", nval) =>
        val col = xeq.split("=").last.toInt
        val n = nval.toInt
        repeat(n, rotate[Boolean], (0 until ROWS).map(row => state(row)(col)).toList).zip(0 until ROWS).foreach { case (v, row) =>
          state(row)(col) = v
        }
      case Array("rotate", "row", yeq, "by", nval) =>
        val row = yeq.split("=").last.toInt
        val n = nval.toInt
        repeat(n, rotate[Boolean], state(row).toList).zip(0 until COLUMNS).foreach { case (v, col) =>
          state(row)(col) = v
        }
      case Array("rect", colsbyrows) =>
        val Array(cols, rows) = colsbyrows.split("x")
        (0 until rows.toInt).foreach(row => (0 until cols.toInt).foreach(col => state(row)(col) = true))
    })

    val part1 = state.map(_.count(identity)).sum

    // Part 2: look at the result of this:
    println(state.map(_.map(v => Map(false -> '.', true -> '#')(v)).mkString).mkString("\n"))
  }


  def main(args: Array[String]) {
    println(day8.part1)
    //println(day8.part2)
  }
}