package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day10 {
  val input = Source.fromFile("data2023/10").getLines()
  val grid = input.map(_.toArray).toArray

  val width = grid(0).length
  val height = grid.length

  case class Pos(row: Int, col: Int) {
    def -(other: Pos) = Pos(row - other.row, col - other.col)

    def +(other: Pos) = Pos(row + other.row, col + other.col)
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
    steps.get((delta, grid(pos.row + delta.row)(pos.col + delta.col)))
  }

  @tailrec
  def walk(oldPos: Pos, pos: Pos, acc: List[Pos] = Nil): List[Pos] = {
    val newPos = pos + steps((pos - oldPos, grid(pos.row)(pos.col)))
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
          var ix = col + 1
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

object Day10Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day10.part1)
    println(LocalDateTime.now())
  }
}
