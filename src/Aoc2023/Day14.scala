package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day14 {
  val input = Source.fromFile("data2023/14").mkString.split("\n").toList.transpose

  type Column = List[Char]
  type Grid = List[Column]

  @tailrec
  def stabilize[A](zero: A)(f: A=>A): A = {
    val next = f(zero)
    if (next == zero) zero else stabilize(next)(f)
  }

  def tiltOne: (List[Char]) => List[Char] = {
    case ('.' :: 'O' :: rest) => 'O' :: tiltOne('.' :: rest)
    case (c :: c2 :: rest)    => c :: tiltOne(c2 :: rest)
    case (c :: Nil)           => List(c)
  }

  def tilt(grid: Grid): Grid = grid.map(stabilize(_)(tiltOne))

  def printGrid(grid: Grid) = {
    grid.foreach(row =>
      println(row.mkString)
    )
    println()
  }

  def rotate(grid: Grid): Grid = grid.transpose.reverse

  def load(grid: Grid): Int =
    grid.map(_.reverse.zipWithIndex.collect{case ('O', i) => i+1}.sum).sum

  val part1 = load(tilt(input))

  def loop(grid: Grid): Grid =
    rotate(tilt(rotate(tilt(rotate(tilt(rotate(tilt(grid))))))))

  val part2 = (1 to 500).scanLeft((input, load(input))) { case ((grid, l), _) =>
    val nextGrid = loop(grid)
    (nextGrid, load(nextGrid))
  }.map(_._2).zipWithIndex.groupBy(_._1).mapValues(_.map(_._2)).filter(_._2.length > 1)
  .map{ case (v, ixes) =>
    // Identify the looping loads, and then select the one which hits 1B
    println(s"$v: ${ixes.drop(5).head} ${ixes.toList.sliding(2).map{case vs => vs(1)-vs(0)}.mkString(", ")}")
  }

}

object Day14Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day14.part2)
    println(LocalDateTime.now())
  }
}
