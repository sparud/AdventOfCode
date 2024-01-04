package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day21 {
  case class Pos(row: Int, col: Int) {
    def +(other: Pos) = Pos(row + other.row, col + other.col)
    def *(factor: Int) = Pos(row * factor, col * factor)
    def go(dir: Dir, steps: Int) = (1 to steps).map(copy() + dir * _).toList
  }

  class Dir(row: Int, col: Int, val isHorizontal: Boolean) extends Pos(row, col) {
    def turnLeft: Dir = new Dir(-col, row, !isHorizontal)
    def turnRight: Dir = new Dir(col, -row, !isHorizontal)
    val isVertical = !isHorizontal
  }

  object Right extends Dir(0, 1, true)
  object Left extends Dir(0, -1, true)
  object Up extends Dir(-1, 0, false)
  object Down extends Dir(1, 0, false)
  val allDirs = List(Right, Left, Up, Down)

  val input0 =
    """........
      |...#.##.
      |.##S.##.
      |..#.....""".stripMargin.split("\n")

  val input2 = """...........
                |.....###.#.
                |.###.##..#.
                |..#.#...#..
                |....#.#....
                |.##..S####.
                |.##..#...#.
                |.......##..
                |.##.#.####.
                |.##..##.##.
                |...........
                |""".stripMargin.split("\n")

  val input = Source.fromFile("data2023/21").mkString.split("\n")

  val height = input.size
  val width = input.head.length

  val walledIn = "#" * (width+2) :: input.map("#" + _ + "#").toList ++ List("#" * (width+2))

  val rocks = walledIn.zipWithIndex.flatMap{case (row, rowNo) =>
    row.zipWithIndex.filter(_._1 == '#').map{case (_, colNo) =>
      Pos(rowNo, colNo)}}.toSet

  val start = walledIn.zipWithIndex.flatMap { case (row, rowNo) =>
    row.zipWithIndex.find(_._1 == 'S').map { case (_, colNo) =>
      Pos(rowNo, colNo)
    }
  }.head

  @tailrec
  def repeat[A](n: Int, v: A)(f: A => A): A =
    if (n == 0) v else repeat(n-1, f(v))(f)

  def steps(n: Int) =
    repeat(n, Set(start))(_.flatMap(pos => allDirs.map(pos + _).filter(!rocks(_))))

  lazy val part1 = steps(6).size

  val start2 = start + Left + Up

  lazy val grid = input.zipWithIndex.flatMap{ case (row, rowNo) =>
    row.zipWithIndex.map{case (col, colNo) => (Pos(rowNo, colNo), col)}}.toMap

  def check(p1: Pos, p2: Pos) =
    if (grid.getOrElse(p1, '#') == '#' || grid.getOrElse(p2, '#') == '#') None else Some((p1, p2, 1))

  lazy val edges = input.zipWithIndex.flatMap { case (row, rowNo) =>
    row.zipWithIndex.flatMap { case (col, colNo) =>
      val pos = Pos(rowNo, colNo)
      List(check(pos, pos + Right), check(pos, pos + Down)).flatten
    }
  }.toSet

  val distances = DSPA.run(edges, start2, false).mapValues(_._1).filter(_._2 < 1000)

  val evenCorners = distances.values.count(d => d % 2 == 0 && d > 65)
  val oddCorners = distances.values.count(d => d % 2 == 1 && d > 65)

  val evenFull = distances.values.count(_ % 2 == 0)
  val oddFull = distances.values.count(_ % 2 == 1)

  val n = BigInt((26501365 - input.length/2) / input.length)

  lazy val part2 = (n + 1) * (n + 1) * oddFull + (n * n) * evenFull - (n + 1) * oddCorners + n * evenCorners
}

object Day21Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day21.part2)
    println(LocalDateTime.now())
  }
}
