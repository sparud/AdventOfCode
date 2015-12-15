import java.security.MessageDigest
import org.json4s.JsonAST.{JString, JInt, JObject, JArray}
import org.json4s.native.JsonMethods.parse

import scala.io.Source
import javax.xml.bind.DatatypeConverter.printHexBinary

object puzzle {
  object day1 {
    val input = Source.fromFile("data/1.data").mkString

    val dir = Map('(' -> 1, ')' -> -1)

    def A = input.map(dir).sum
    def B = input.map(dir).scanLeft(0)(_ + _).indexOf(-1)
  }

  object day2 {

    val input = Source.fromFile("data/2.data").getLines().toList

    def A = input.map(_.split("x").map(_.toInt))
        .map { case Array(a, b, c) => Array(a*b, a*c, b*c) }
        .map(sides => 2 * sides.sum + sides.min)
        .sum
    def B = input.map(_.split("x").map(_.toInt).sorted).map { case Array(a, b, c) => 2*(a+b) + a*b*c }.sum
  }

  object day3 {
    val s = Source.fromFile("data/3.data").mkString

    def positions(s: String) = s.scanLeft((0, 0)) {
      case ((x, y), '^') => (x, y + 1)
      case ((x, y), '>') => (x + 1, y)
      case ((x, y), '<') => (x - 1, y)
      case ((x, y), 'v') => (x, y - 1)
    }

    def A = positions(s).distinct.size
    def B = s.grouped(2).toList.transpose.map(_.mkString).flatMap(positions).distinct.size
  }

  object day4 {
    val secretKey = "iwrupvqb"

    def md5(s: String) = printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes))

    def withLeading(leading: String) =
       Stream.from(1).map(n => (md5(s"$secretKey$n"), n)).filter(_._1.startsWith(leading)).head._2

    def A = withLeading("00000")
    def B = withLeading("000000")
  }

  object day5 {
    val input = Source.fromFile("data/5.data").getLines().toList

    val vowels = "aeiou".toSet
    val forbidden = List("ab", "cd", "pq", "xy")

    def niceA(s: String) =
      s.count(vowels) >= 3 && s.sliding(2).exists(p => p(0) == p(1)) && !forbidden.exists(s.contains)

    def niceB(s: String) =
      """(..).*\1""".r.findFirstMatchIn(s).isDefined && s.sliding(3).exists(p => p(0) == p(2))

    def A = input.count(niceA)
    def B = input.count(niceB)
  }

  object day6 {
    val input = Source.fromFile("data/6.data").getLines().toList

    val parser = """(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)""".r

    def pix(pos: (Int, Int)) = pos._1 * 1000 + pos._2

    def operationA(state: Array[Int], cmd: String, pos: (Int, Int)) = cmd match {
      case "turn on"  => state(pix(pos)) = 1
      case "turn off" => state(pix(pos)) = 0
      case "toggle"   => state(pix(pos)) ^= 1
    }

    def operationB(state: Array[Int], cmd: String, pos: (Int, Int)) = cmd match {
      case "turn on"  => state(pix(pos)) += 1
      case "turn off" => state(pix(pos)) = math.max(0, state(pix(pos))-1)
      case "toggle"   => state(pix(pos)) += 2
    }

    def run(operation: (Array[Int], String, (Int, Int)) => Unit) = {
      val state = new Array[Int](1000*1000)
      input.foreach {
        case parser(cmd, x0, y0, x1, y1) =>
          (x0.toInt to x1.toInt).map(x => (y0.toInt to y1.toInt).map(y => (x, y))).flatten.foreach(operation(state, cmd, _))
      }
      state.sum
    }

    def A = run(operationA)
    def B = run(operationB)
  }

  object day7 {
    val input = Source.fromFile("data/7.data").getLines().toList

    val wires = input.map(_.split(" -> ")).map { case Array(op, wire) => wire -> op}.toMap

    var memo = Map[String, Int]()

    def evaluate(s: String): Int = {
      if (s(0).isDigit)
        s.toInt
      else memo.getOrElse(s, {
        val value = wires(s).split(" ") match {
          case Array(a) => evaluate(a)
          case Array(a, "AND", b)    => evaluate(a) & evaluate(b)
          case Array(a, "OR", b)     => evaluate(a) | evaluate(b)
          case Array(a, "LSHIFT", b) => evaluate(a) << evaluate(b)
          case Array(a, "RSHIFT", b) => evaluate(a) >> evaluate(b)
          case Array("NOT", a)       => ~evaluate(a)
        }
        memo += s -> value
        value
      })
    }

    def A = evaluate("a")

    def B = {
      memo = Map("b" -> evaluate("a"))
      evaluate("a")
    }
  }

  object day8 {
    val input = Source.fromFile("data/8.data").getLines().toList

    val p = """\\\\|\\"|\\x\w\w""".r

    def A = input.map(line => line.length - (p replaceAllIn (line, ".")).length + 2).sum
    def B = input.map(_.count(Seq('\\', '"').contains)+2).sum
  }

  object day9 {
    val input = Source.fromFile("data/9.data").getLines().toList

    val distances = input.flatMap(_.split(" ") match {
      case Array(from, _, to, _, dist) => Seq(Seq(from, to) -> dist.toInt, Seq(to, from) -> dist.toInt)
    }).toMap

    val cities = distances.keys.flatten.toSeq.distinct

    def A = cities.permutations.map(_.sliding(2).map(distances).sum).min
    def B = cities.permutations.map(_.sliding(2).map(distances).sum).max
  }

  object day10 {
    val input = "1113122113"

    def lookandsay(s: List[Int], res: List[Int] = Nil): List[Int] = if (s.isEmpty) res.reverse else {
      val (init, rest) = s.span(_ == s.head)
      lookandsay(rest, init.head :: init.length :: res)
    }

    def A = (1 to 40).foldLeft(input.map(_.toInt - '0').toList){(s, n) => lookandsay(s, Nil)}.length
    def B = (1 to 50).foldLeft(input.map(_.toInt - '0').toList){(s, n) => lookandsay(s, Nil)}.length
  }

  object day11 {
    val input = "vzbxkghb"

    val pair = """(.)\1.*(.)\2""".r
    val alphabet = ('a' to 'z').filter(!"iol".contains(_)).mkString
    val triples = alphabet.sliding(3).toList
    val inc = alphabet.sliding(2).map(l => l(0).toString -> l(1).toString).toMap

    private def next(s: String): String = {
      val (prefix, last) = s.splitAt(s.length - 1)
      inc.get(last).map(prefix + _).getOrElse(next(prefix) + "a")
    }

    def A = Iterator.iterate(input)(next).filter(s => triples.exists(s.contains(_)) && pair.findFirstIn(s).isDefined).next()

    def B = Iterator.iterate(next(A))(next).filter(s => triples.exists(s.contains(_)) && pair.findFirstIn(s).isDefined).next()
  }

  object day12 {
    val input = Source.fromFile("data/12.data").mkString

    def calcA: Any => Int = {
      case JArray(l) => l.map(calcA).sum
      case JObject(kvs) => kvs.map(p => calcA(p._2)).sum
      case JInt(n) => n.toInt
      case _ => 0
    }

    def calcB: Any => Int = {
      case JArray(l) => l.map(calcB).sum
      case JObject(kvs) if !kvs.exists(_._2 == JString("red")) => kvs.map(p => calcB(p._2)).sum
      case JInt(n) => n.toInt
      case _ => 0
    }

    def A = calcA(parse(input))
    def B = calcB(parse(input))
  }

  object day13 {
    val input = Source.fromFile("data/13.data").getLines().toList

    val parse = """(\w+) would (gain|lose) (\d+).*to (\w+)\.""".r

    val gains = input.map {
      case parse(a, dir, p, b) => (a, b) -> (if (dir=="gain") 1 else -1)*p.toInt
    }.toMap

    val persons = gains.keys.map(_._1).toList.distinct

    val A = persons.permutations.map(l => l(l.length-1) :: l).map(_.sliding(2).map{
      case List(a, b) => gains((a, b)) + gains((b, a))
    }.sum).max

    val gainsB = gains ++ persons.flatMap(p => Seq((p, "Me") -> 0, ("Me", p) -> 0))

    val B = ("Me" :: persons).permutations.map(l => l(l.length-1) :: l).map(_.sliding(2).map{
      case List(a, b) => gainsB((a, b)) + gainsB((b, a))
    }.sum).max
  }

  object day14 {
    val input = Source.fromFile("data/14.data").getLines().toList

    val reindeers = input.map("""\d+""".r.findAllMatchIn(_).toList.map(_.toString().toInt))

    def distance(t: Int): List[Int] => Int = { case List(s, t1, t2) => t/(t1+t2)*t1*s + math.min(t1, t%(t1+t2))*s }

    val A = reindeers.map(distance(2503)).max

    val B = (1 to 2503).map { t =>
      val distances = reindeers.map(distance(t))
      distances.map(d => if (d == distances.max) 1 else 0)
    }.transpose.map(_.sum).max
  }

  object gchq {
    val grid = Array(
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 2, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 2, 2, 0, 0, 0, 0, 2, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 2, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    )

    val rows = Seq(
      Seq(7, 3, 1, 1, 7),
      Seq(1, 1, 2, 2, 1, 1),
      Seq(1, 3, 1, 3, 1, 1, 3, 1),
      Seq(1, 3, 1, 1, 6, 1, 3, 1),
      Seq(1, 3, 1, 5, 2, 1, 3, 1),
      Seq(1, 1, 2, 1, 1),
      Seq(7, 1, 1, 1, 1, 1, 7),
      Seq(3, 3),
      Seq(1, 2, 3, 1, 1, 3, 1, 1, 2),
      Seq(1, 1, 3, 2, 1, 1),
      Seq(4, 1, 4, 2, 1, 2),
      Seq(1, 1, 1, 1, 1, 4, 1, 3),
      Seq(2, 1, 1, 1, 2, 5),
      Seq(3, 2, 2, 6, 3, 1),
      Seq(1, 9, 1, 1, 2, 1),
      Seq(2, 1, 2, 2, 3, 1),
      Seq(3, 1, 1, 1, 1, 5, 1),
      Seq(1, 2, 2, 5),
      Seq(7, 1, 2, 1, 1, 1, 3),
      Seq(1, 1, 2, 1, 2, 2, 1),
      Seq(1, 3, 1, 4, 5, 1),
      Seq(1, 3, 1, 3, 10, 2),
      Seq(1, 3, 1, 1, 6, 6),
      Seq(1, 1, 2, 1, 1, 2),
      Seq(7, 2, 1, 2, 5)
    ).map(_.map(-1 -> _))

    val cols = Seq(
      Seq(7, 2, 1, 1, 7),
      Seq(1, 1, 2, 2, 1, 1),
      Seq(3, 1, 3, 1, 3, 1, 3, 1),
      Seq(1, 3, 1, 1, 5, 1, 3, 1),
      Seq(1, 3, 1, 1, 4, 1, 3, 1),
      Seq(1, 1, 1, 2, 1, 1),
      Seq(7, 1, 1, 1, 1, 1, 7),
      Seq(1, 1, 3),
      Seq(2, 1, 2, 1, 8, 2, 1),
      Seq(2, 2, 1, 2, 1, 1, 1, 2),
      Seq(1, 7, 3, 2, 1),
      Seq(1, 2, 3, 1, 1, 1, 1, 1),
      Seq(4, 1, 1, 2, 6),
      Seq(3, 3, 1, 1, 1, 3, 1),
      Seq(1, 2, 5, 2, 2),
      Seq(2, 1, 1, 1, 1, 1, 2, 1),
      Seq(1, 3, 3, 2, 1, 8, 1),
      Seq(6, 2, 1),
      Seq(7, 1, 4, 1, 1, 3),
      Seq(1, 1, 1, 1, 4),
      Seq(1, 3, 1, 3, 7, 1),
      Seq(3, 1, 1, 1, 2, 1, 1, 4),
      Seq(1, 3, 1, 4, 3, 3),
      Seq(1, 1, 2, 2, 2, 6, 1),
      Seq(7, 1, 3, 2, 1, 1)
    ).map(_.map(-1 -> _))

    def row_alts(streaks: Seq[(Int, Int)], row: Array[Int]) = {
      if (streaks.map(_._2).sum + streaks.length - 1 == 25) {
        Some(streaks.scanLeft(-1->0) { case ((a, b), (c, d)) => a + b + 1 -> d})
      } else if (row.sum > 0) {

      } else
        None
    }

  }

  def main(args: Array[String]) {
    println(day14.A)
    println(day14.B)
    //println(day13.B)
    //import gchq._
    //println(row_alts(rows(6), grid(6)))
  }
}
