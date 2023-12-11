package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

object Day9 {
  val input = Source.fromFile("data2023/9").mkString.split("\n")
    .map(_.split(" ").map(_.toInt).toList)

  def getDiffLists(numbers: List[Int]): List[List[Int]] =
    if (numbers.forall(_ == 0))
      Nil
    else
      numbers :: getDiffLists(numbers.sliding(2).map { case a :: b :: Nil => b - a }.toList)

  val part1 = input.map(getDiffLists(_).map(_.last).sum).sum

  def prev(numbers: List[Int]) =
    numbers.foldRight(0) { case (n, acc) => n - acc }

  val part2 = input.map(getDiffLists(_).map(_.head)).toList.map(prev).sum

}

object Day9Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day9.part1)
    println(LocalDateTime.now())
  }
}
