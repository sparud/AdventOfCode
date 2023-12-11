package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day11 {
  val input = Source.fromFile("data2023/11").mkString.split("\n")

  val startGalaxies = input.zipWithIndex.flatMap { case (row, rowNo) =>
    row.zipWithIndex.flatMap { case (col, colNo) =>
      if (col == '#') Some((BigInt(rowNo), BigInt(colNo))) else None
    }
  }

  val emptyRows = (0 until input.length).
    filter(r => !startGalaxies.map(_._1).contains(r)).sorted.map(BigInt(_))
  val emptyCols = (0 until input.head.length).
    filter(c => !startGalaxies.map(_._2).contains(c)).sorted.map(BigInt(_))

  def expandBy(axis: Seq[BigInt], empty: Seq[BigInt], factor: Int) =
    axis.foldLeft((empty, BigInt(0), Map(): Map[BigInt, BigInt])) { case ((empty, offset, m), v) =>
      val (less, more) = empty.span(_ < v)
      val newOffset = offset + less.size * factor
      (more, newOffset, m + (v -> (v + newOffset)))
    }._3

  val rowMap = expandBy(startGalaxies.map(_._1).sorted, emptyRows, 1)
  val colMap = expandBy(startGalaxies.map(_._2).sorted, emptyCols, 1)

  val galaxies = startGalaxies.map { case (row, col) => (rowMap(row), colMap(col)) }

  def distance(g1: (BigInt, BigInt), g2: (BigInt, BigInt)): BigInt =
    (g2._1 - g1._1).abs + (g2._2 - g1._2).abs

  val part1 = galaxies.combinations(2).map { case Array(g1, g2) => distance(g1, g2) }.sum

  val rowMap2 = expandBy(startGalaxies.map(_._1).sorted, emptyRows, 999999)
  val colMap2 = expandBy(startGalaxies.map(_._2).sorted, emptyCols, 999999)

  val galaxies2 = startGalaxies.map { case (row, col) => (rowMap2(row), colMap2(col)) }

  val part2 = galaxies2.combinations(2).map { case Array(g1, g2) => distance(g1, g2) }.sum
}

object Day11Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day11.part1)
    println(LocalDateTime.now())
  }
}
