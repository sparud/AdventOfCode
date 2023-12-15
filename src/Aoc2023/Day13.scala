package Aoc2023

import java.time.LocalDateTime
import scala.io.Source

object Day13 {
  val input = Source.fromFile("data2023/13").mkString.split("\n\n").toList
    .map(_.split("\n").toList)

  def find[A](l: List[A]): List[Int] = {
    def g[A](v: List[A], acc: List[A]): List[Int] = {
      if (v.isEmpty)
        Nil
      else {
        val len = math.min(v.length, acc.length)
        val rest =  g(v.tail, v.head :: acc)
        if (v.take(len) == acc.take(len)) acc.length :: rest else rest
      }
    }

    g(l.tail, List(l.head))
  }

  lazy val part1 = input.map(scan).map(_.head).sum

  def getCols(rows: List[String]) = rows.transpose.map(_.mkString)

  def scan(rows: List[String]) = find(rows).map(_ * 100) ++ find(getCols(rows))

  def smudge(s: String, ix: Int) = {
    val sb = new StringBuilder(s)
    sb.setCharAt(ix, Map('#' -> '.', '.' -> '#')(s(ix)))
    sb.toString
  }

  def swap(rows: List[String], r: Int, c:Int) =
    rows.zipWithIndex.map{case (row, i) => if (i == r) smudge(row, c) else row}

  def generateSmudges(rows: List[String]) = {
    val res = scan(rows)
    val scans = (0 until rows.length).flatMap(r =>
      (0 until rows(0).length).map(c => scan(swap(rows, r, c))
    ))
    (scans.flatten.toSet -- res).head
  }

  lazy val part2 = input.map(generateSmudges).sum
}

object Day13Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day13.part1)
    println(LocalDateTime.now())
  }
}
