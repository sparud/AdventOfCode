package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day18 {
  case class Pos(row: Int, col: Int) {
    def +(other: Pos) = Pos(row + other.row, col + other.col)
    def *(factor: Int) = Pos(row * factor, col * factor)
    def go(dir: Dir, steps: Int) = (1 to steps).map(copy() + dir*_).toList
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

  case class Instruction(dir: Dir, len: Int, color: String) {
    def forReal =
      Instruction(
        Map(0 -> Right, 1 -> Down, 2 -> Left, 3 -> Up)(Integer.parseInt(color.substring(5))),
        Integer.parseInt(color.substring(0, 5), 16),
        color
      )
  }

  object Instruction {
    def apply(row: String): Instruction = row.split(" ") match {
      case Array(d, l, c) => Instruction(d match {
        case "R" => Right
        case "D" => Down
        case "L" => Left
        case "U" => Up
      }, l.toInt, c.split("\\(#|\\)")(1))
    }
  }

  val input = Source.fromFile("data2023/18").mkString.
    split("\n").toList.map(Instruction(_))

  def count(instructions: List[Instruction]) = {
    // Follow instructions and find polynom coordinates
    val coords = instructions.foldLeft(List(Pos(0, 0))) { case (acc, instr) =>
      acc.head + instr.dir * instr.len :: acc
    }.reverse

    // https://www.wikihow.com/Calculate-the-Area-of-a-Polygon
    // https://en.wikipedia.org/wiki/Shoelace_formula
    val area = coords.sliding(2).map(p =>
      BigInt(p.head.col) * p.last.row - BigInt(p.last.col) * p.head.row
    ).sum / 2

    val exterior = instructions.map(_.len).sum

    //Pick's theorem
    val interior = area - exterior/2 + 1

    interior + exterior
  }

  lazy val part1 = count(input)

  lazy val part2 = count(input.map(_.forReal))
}

object Day18Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day18.part2)
    println(LocalDateTime.now())
  }
}
