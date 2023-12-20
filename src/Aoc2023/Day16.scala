package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day16 {
  val input = Source.fromFile("data2023/16").mkString.split("\n")

  case class Pos(row: Int, col: Int) {
    def +(other: Pos) = Pos(row + other.row, col + other.col)
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

  def moves: (Dir, Char) => List[Dir] = {
    case (dir, '|') if dir.isHorizontal => List(dir.turnLeft, dir.turnRight)
    case (dir, '-') if dir.isVertical   => List(dir.turnLeft, dir.turnRight)
    case (dir, '/')  => List(if (dir.isHorizontal) dir.turnLeft else dir.turnRight)
    case (dir, '\\') => List(if (dir.isHorizontal) dir.turnRight else dir.turnLeft)
    case (dir, _)    => List(dir)
  }

  def moves2: (Dir, Char) => List[Dir] = {
    case (Right | Left, '|')         => List(Up, Down)
    case (Right, '\\') | (Left, '/') => List(Down)
    case (Right, '/') | (Left, '\\') => List(Up)
    case (Up | Down, '-')            => List(Left, Right)
    case (Up, '\\') | (Down, '/')    => List(Left)
    case (Up, '/') | (Down, '\\')    => List(Right)
    case (r, _)                      => List(r)
  }

  val rows = input.length
  val cols = input.head.length

  def inside(pos: Pos) = pos.row >= 0 && pos.row < rows && pos.col >=0 && pos.col < cols

  val grid = input.toList.zipWithIndex.flatMap { case (row, rowNo) =>
    row.zipWithIndex.flatMap{ case (c, colNo) =>
      if (c == '.') None else Some((Pos(rowNo, colNo),  c))
    }
  }.toMap.withDefaultValue('.')

  def run(dir: Dir, pos: Pos, visited: mutable.HashSet[(Pos, Pos)]): Unit = {
      if (inside(pos) && !visited((dir, pos))) {
        visited.add((dir, pos))
        moves(dir, grid(pos)).foreach(newDir => run(newDir, pos + newDir, visited))
      }
  }

  def startAndRun(dir: Dir, pos: Pos): Int = {
    val visited = new mutable.HashSet[(Pos, Pos)]()
    run(dir, pos, visited)
    visited.map(_._2).size
  }

  lazy val part1 = startAndRun(Right, Pos(0, 0))

  lazy val part2 = math.max(
    (0 until rows).map(rowNo => math.max(
      startAndRun(Right, Pos(rowNo, 0)), startAndRun(Left, Pos(rowNo, cols-1))
    )).max,
    (0 until cols).map(colNo => math.max(
      startAndRun(Down, Pos(0, colNo)), startAndRun(Up, Pos(rows-1, colNo))
    )).max
  )


}

object Day16Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day16.part2)
    println(LocalDateTime.now())
  }
}
