package Aoc2023
import java.time.LocalDateTime
import scala.io.Source
import scala.collection.mutable


object Day12 {
  val input = Source.fromFile("data2023/12").mkString.split("\n").
    map(_.split(" ") match {
      case Array (records, spans) => (records, spans.split(",").map(_.toInt).toList)
    })

  // Simplistic brute force solution, will probably not work in part 2
  def combinations(records: List[Char]): List[List[Char]] = records match {
    case '?' :: rest => combinations(rest).flatMap(r => List('.', '#').map(_ :: r))
    case any :: rest => combinations(rest).map(any :: _)
    case Nil => List(List())
  }

  val part1 = input.map { case (records, spans) => combinations(records.toList)
    .count(_.dropWhile(_ == '.').mkString.split("[.]+").map(_.length).toList == spans)
  }.sum

  def infoRepeated(n: Int) = input.map{ case (records, spans) =>
    (List.fill(n)(records).mkString("?"), List.fill(n)(spans).flatten)
  }

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
    override def apply(key: I) = self.synchronized(getOrElseUpdate(key, f(key)))
  }

  val findSpans: ((List[Char], Int, Boolean)) => List[List[Char]] = memoize {
    case (Nil, 0, _) => List(Nil)
    case (Nil, n, _) => Nil
    case ('.' :: rest, 0, _) => List('.'::rest)
    case ('?' :: rest, 0, _) => List('.'::rest)
    case ('#' :: rest, 0, _) => Nil
    case ('.' :: rest, n, _) => Nil
    case ('?' :: rest, n, false) => findSpans(rest, n, false) ++ findSpans(rest, n-1, true)
    case ('?' :: rest, n, true) => findSpans(rest, n-1, true)
    case ('#' :: rest, n, _) => findSpans(rest, n-1, true)
  }

  val count: ((List[Char], List[Int])) => BigInt = memoize {
    case (records: List[Char], spans: List[Int]) =>
      if (records.isEmpty)
        if (spans.isEmpty) 1 else 0
      else if (spans.isEmpty)
          if (records.contains('#')) 0 else 1
      else if (records.length < spans.sum + spans.length - 1)
        0
      else {
        val (dots, other) = records.span(_ == '.')
        val (material, other2) = other.span("?#".contains(_))
        var total: BigInt = 0
        if (material.forall(_ == '?'))
          total += count(other2, spans)
        if (material.length >= spans.head)
          total += findSpans(material, spans.head, false).map(rest => count(rest ++ other2, spans.tail)).sum
        total
      }
  }

  val part2 = infoRepeated(5).map { case (records, spans) =>
    count(records.toList, spans)
  }.toList.sum

}

object Day12Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day12.part2)
    println(LocalDateTime.now())
  }
}
