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

    def decode(tuple: (String, Int, String)) = repeat(tuple._2, tuple._1,  {s: String => s.map(transform)})

    val part2 = input.map(tuple => (tuple._2, decode(tuple))).find(_._2 == "northpoleobjectstorage").map(_._1).get
  }

  def main(args: Array[String]) {
    println(day4.part1)
    println(day4.part2)
  }
}