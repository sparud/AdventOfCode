import scala.Stream._
import scala.io.Source

object aoc2021 {
  object day1 {
    val input = Source.fromFile("data2021/1").getLines()
    val numbers = input.map(_.toInt).toList

    val part1 = numbers.zip(numbers.tail)

    val part2 =
      Stream.continually(numbers).flatten
        .scanLeft(0)(_ + _)
        .scanLeft((None: Option[Int], Set[Int]())) {
          case ((r: Option[Int], s: Set[Int]), n: Int) =>
            if (s.contains(n)) (Some(n), s) else (None, s + n)
        }
        .find(_._1.isDefined).get._1
  }

  def main(args: Array[String]) {
    print(day1.part1)
  }
}