import java.lang.Math._

import scala.Stream._
import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator._

object aoc2018 {
  object day1 {
    val input = Source.fromFile("data2018/1").getLines()
    val numbers = input.map(_.toInt).toList

    val part1 = numbers.sum

    val part2 =
      Stream.continually(numbers).flatten
      .scanLeft(0)(_ + _)
      .scanLeft((None: Option[Int], Set[Int]())) {
        case ((r: Option[Int], s: Set[Int]), n: Int) =>
          if (s.contains(n)) (Some(n), s) else (None, s + n)
      }
      .find(_._1.isDefined).get._1
  }

  object day2 {
    val input = Source.fromFile("data2018/2").getLines().toList

    val counts = input.map(_.foldLeft(Map[Char, Int]().withDefaultValue(0)){ case (a, b) => a + (b -> (a(b)+1))}.values)

    val part1 = counts.map(c => Seq(c.exists(2 ==), c.exists(3 ==))).transpose.map(_.count(identity)).product

    def common(s1: String, s2: String) = {
      def iterate(ix: Int, errors: Int, soFar: List[Char]): Option[String] =
        if (errors > 1)
          None
        else if (ix == s1.length)
          Some(soFar.reverse.mkString)
        else if (s1(ix) == s2(ix))
          iterate(ix+1, errors, s1(ix) :: soFar)
        else
          iterate(ix+1, errors+1, soFar)
      iterate(ix=0, errors=0, soFar=Nil)
    }

    def triangulate[A](values: List[A]): Stream[(A, A)] = values match {
      case Nil => Stream.empty
      case v :: vs => vs.map((v, _)).toStream ++ triangulate(vs)
    }

    val part2 = triangulate(input).flatMap(p => common(p._1, p._2)).head
  }

  def main(args: Array[String]) {
    println(day2.part1)
    println(day2.part2)
  }
}
