package Aoc2023
import scala.io.Source
import java.time.LocalDateTime

object Day1 {
  val input = Source.fromFile("data2023/1").getLines().toList

  def calibrate(s: String) = {
    val digits = s.filter(_.isDigit)
    ("" + digits(0) + digits(digits.length - 1)).toInt
  }

  val part1 = input.map(calibrate).sum

  val patterns = List(
    "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"
  ).zipWithIndex.tail

  def transform(s: String): String =
    if (s == "")
      ""
    else if (s(0).isDigit)
      s(0) + transform(s.substring(1))
    else {
      val rest = transform(s.substring(1))
      patterns
        .find(p => s.startsWith(p._1))
        .map(_._2 + rest)
        .getOrElse(rest)
    }

  def calibrate2(s: String) = calibrate(transform(s))

  val part2 = input.map(calibrate2).sum
}

object Day1Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day1.part2)
    println(LocalDateTime.now())
  }
}
