package Aoc2023
import scala.io.Source

import java.time.LocalDateTime

object Day2 {
  case class CubeSet(red: Int = 0, green: Int = 0, blue: Int = 0) {
    def +(c: CubeSet): CubeSet = CubeSet(red + c.red, green + c.green, blue + c.blue)
    def -(c: CubeSet): CubeSet = CubeSet(red - c.red, green - c.green, blue - c.blue)
    def power: Int = red * green * blue
    def ok: Boolean = red >= 0 && green >= 0 && blue >= 0
  }

  object CubeSet {
    val zero = CubeSet(0, 0, 0)

    def parseCubes(s: String): CubeSet =
      s.split(", ").map { n_color =>
        val Array(n, color) = n_color.split(" ")
        color match {
          case "red" => CubeSet(red = n.toInt)
          case "green" => CubeSet(green = n.toInt)
          case "blue" => CubeSet(blue = n.toInt)
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

object Day2Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day2.part2)
    println(LocalDateTime.now())
  }
}
