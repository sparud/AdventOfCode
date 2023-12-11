package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day3 {
  val input = Source.fromFile("data2023/3").getLines()
  val inner = input.toArray
  val board = "." * inner(0).length +: inner.map(row => "." + row + ".") :+ "." * inner(0).length

  val numbers = board.zipWithIndex.flatMap { case (row, i) =>
      row.zipWithIndex.foldLeft((List(): List[List[((Int, Int), Int)]], "")) { case ((state, sofar), (col, j)) =>
        if (col.isDigit)
          (state, sofar + col)
        else if (sofar.nonEmpty)
          (state :+ (j - sofar.length until j).toList.map(c => ((i, c), sofar.toInt)), "")
        else
          (state, sofar)
      }._1
    }.zipWithIndex
    .flatMap { case (p, ix) => p.map(v => (v._1, (v._2, ix))) }
    .toMap

  println(numbers)
  val part1 = board.zipWithIndex.map({ case (row, i) =>
    row.zipWithIndex.map { case (col, j) =>
      if (col != '.' && !col.isDigit)
        List(
          (i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
          (i, j - 1), (i, j + 1),
          (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)
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

object Day3Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day3.part1)
    println(LocalDateTime.now())
  }
}
