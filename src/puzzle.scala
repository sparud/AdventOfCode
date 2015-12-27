import java.security.MessageDigest
import org.json4s.JsonAST.{JString, JInt, JObject, JArray}
import org.json4s.native.JsonMethods.parse

import scala.io.Source
import javax.xml.bind.DatatypeConverter.printHexBinary

object puzzle {
  object day1 {
    val input = Source.fromFile("data/1.data").mkString

    val dir = Map('(' -> 1, ')' -> -1)

    def part1 = input.map(dir).sum
    def part2 = input.map(dir).scanLeft(0)(_ + _).indexOf(-1)
  }

  object day2 {

    val input = Source.fromFile("data/2.data").getLines().toList

    def part1 = input.map(_.split("x").map(_.toInt))
        .map { case Array(a, b, c) => Array(a*b, a*c, b*c) }
        .map(sides => 2 * sides.sum + sides.min)
        .sum
    def part2 = input.map(_.split("x").map(_.toInt).sorted).map { case Array(a, b, c) => 2*(a+b) + a*b*c }.sum
  }

  object day3 {
    val s = Source.fromFile("data/3.data").mkString

    def positions(s: String) = s.scanLeft((0, 0)) {
      case ((x, y), '^') => (x, y + 1)
      case ((x, y), '>') => (x + 1, y)
      case ((x, y), '<') => (x - 1, y)
      case ((x, y), 'v') => (x, y - 1)
    }

    def part1 = positions(s).distinct.size
    def part2 = s.grouped(2).toList.transpose.map(_.mkString).flatMap(positions).distinct.size
  }

  object day4 {
    val secretKey = "iwrupvqb"

    def md5(s: String) = printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes))

    def withLeading(leading: String) =
       Stream.from(1).map(n => (md5(s"$secretKey$n"), n)).filter(_._1.startsWith(leading)).head._2

    def part1 = withLeading("00000")
    def part2 = withLeading("000000")
  }

  object day5 {
    val input = Source.fromFile("data/5.data").getLines().toList

    val vowels = "aeiou".toSet
    val forbidden = List("ab", "cd", "pq", "xy")

    def nice1(s: String) =
      s.count(vowels) >= 3 && s.sliding(2).exists(p => p(0) == p(1)) && !forbidden.exists(s.contains)

    def nice2(s: String) =
      """(..).*\1""".r.findFirstMatchIn(s).isDefined && s.sliding(3).exists(p => p(0) == p(2))

    def part1 = input.count(nice1)
    def part2 = input.count(nice2)
  }

  object day6 {
    val input = Source.fromFile("data/6.data").getLines().toList

    val parser = """(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)""".r

    def pix(pos: (Int, Int)) = pos._1 * 1000 + pos._2

    def operation1(state: Array[Int], cmd: String, pos: (Int, Int)) = cmd match {
      case "turn on"  => state(pix(pos)) = 1
      case "turn off" => state(pix(pos)) = 0
      case "toggle"   => state(pix(pos)) ^= 1
    }

    def operation2(state: Array[Int], cmd: String, pos: (Int, Int)) = cmd match {
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

    def part1 = run(operation1)
    def part2 = run(operation2)
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

    def part1 = evaluate("a")

    def part2 = {
      memo = Map("b" -> evaluate("a"))
      evaluate("a")
    }
  }

  object day8 {
    val input = Source.fromFile("data/8.data").getLines().toList

    val p = """\\\\|\\"|\\x\w\w""".r

    def part1 = input.map(line => line.length - (p replaceAllIn (line, ".")).length + 2).sum
    def part2 = input.map(_.count(Seq('\\', '"').contains)+2).sum
  }

  object day9 {
    val input = Source.fromFile("data/9.data").getLines().toList

    val distances = input.flatMap(_.split(" ") match {
      case Array(from, _, to, _, dist) => Seq(Seq(from, to) -> dist.toInt, Seq(to, from) -> dist.toInt)
    }).toMap

    val cities = distances.keys.flatten.toSeq.distinct

    def part1 = cities.permutations.map(_.sliding(2).map(distances).sum).min
    def part2 = cities.permutations.map(_.sliding(2).map(distances).sum).max
  }

  object day10 {
    val input = "1113122113"

    def lookandsay(s: List[Int], res: List[Int] = Nil): List[Int] = if (s.isEmpty) res.reverse else {
      val (init, rest) = s.span(_ == s.head)
      lookandsay(rest, init.head :: init.length :: res)
    }

    def part1 = (1 to 40).foldLeft(input.map(_.toInt - '0').toList){(s, n) => lookandsay(s, Nil)}.length
    def part2 = (1 to 50).foldLeft(input.map(_.toInt - '0').toList){(s, n) => lookandsay(s, Nil)}.length
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

    def part1 = Iterator.iterate(input)(next).filter(s => triples.exists(s.contains(_)) && pair.findFirstIn(s).isDefined).next()

    def part2 = Iterator.iterate(next(part1))(next).filter(s => triples.exists(s.contains(_)) && pair.findFirstIn(s).isDefined).next()
  }

  object day12 {
    val input = Source.fromFile("data/12.data").mkString

    def calc1: Any => Int = {
      case JArray(l) => l.map(calc1).sum
      case JObject(kvs) => kvs.map(p => calc1(p._2)).sum
      case JInt(n) => n.toInt
      case _ => 0
    }

    def calc2: Any => Int = {
      case JArray(l) => l.map(calc2).sum
      case JObject(kvs) if !kvs.exists(_._2 == JString("red")) => kvs.map(p => calc2(p._2)).sum
      case JInt(n) => n.toInt
      case _ => 0
    }

    def part1 = calc1(parse(input))
    def part2 = calc2(parse(input))
  }

  object day13 {
    val input = Source.fromFile("data/13.data").getLines().toList

    val parse = """(\w+) would (gain|lose) (\d+).*to (\w+)\.""".r

    val gains = input.map {
      case parse(a, dir, p, b) => (a, b) -> (if (dir=="gain") 1 else -1)*p.toInt
    }.toMap

    val persons = gains.keys.map(_._1).toList.distinct

    val part1 = persons.permutations.map(l => l(l.length-1) :: l).map(_.sliding(2).map{
      case List(a, b) => gains((a, b)) + gains((b, a))
    }.sum).max

    val gainsB = gains ++ persons.flatMap(p => Seq((p, "Me") -> 0, ("Me", p) -> 0))

    val part2 = ("Me" :: persons).permutations.map(l => l(l.length-1) :: l).map(_.sliding(2).map{
      case List(a, b) => gainsB((a, b)) + gainsB((b, a))
    }.sum).max
  }

  object day14 {
    val input = Source.fromFile("data/14.data").getLines().toList

    val reindeers = input.map("""\d+""".r.findAllMatchIn(_).toList.map(_.toString().toInt))

    def distance(t: Int): List[Int] => Int = { case List(s, t1, t2) => t/(t1+t2)*t1*s + math.min(t1, t%(t1+t2))*s }

    val part1 = reindeers.map(distance(2503)).max

    val part2 = (1 to 2503).map { t =>
      val distances = reindeers.map(distance(t))
      distances.map(d => if (d == distances.max) 1 else 0)
    }.transpose.map(_.sum).max
  }

  object day15 {
    val input = Source.fromFile("data/15.data").getLines().toSeq
    val ingredients = input.map(_.split(":") match {
      case Array(name, rest) => """-?\d+""".r.findAllMatchIn(rest).toList.map(_.toString().toInt)
    })
    val features = ingredients.map(_.dropRight(1)).transpose
    val calories = ingredients.map(_.last)

    def recipes(ingredients: Int, teaspoons: Int): Seq[List[Int]] = ingredients match {
      case 1 => Seq(List(teaspoons))
      case n => (0 to teaspoons).flatMap(s => recipes(ingredients-1, teaspoons-s).map(s :: _))
    }

    def dotProduct(as: Seq[Int], bs: Seq[Int]) = as.zip(bs).map{ case(a, b) => a * b }.sum

    def part1 = recipes(ingredients.length, 100).map (recipe =>
      features.map(dotProduct(_, recipe).max(0)).product
    ).max

    def part2 = recipes(ingredients.length, 100).filter(dotProduct(_, calories) == 500).map(recipe =>
      features.map(dotProduct(_, recipe).max(0)).product
    ).max
  }

  object day16 {
    val input = Source.fromFile("data/16.data").getLines().toList

    val tape = Map(
      "children" -> 3, "cats" -> 7, "samoyeds" -> 2, "pomeranians" -> 3, "akitas" -> 0, "vizslas" -> 0,
      "goldfish" -> 5, "trees" -> 3, "cars" -> 2, "perfumes" -> 1
    )

    def parseSue(s: String) = """(\w+): (\d+)""".r.findAllMatchIn(s).toList.map(g => (g.group(1), g.group(2).toInt)).toMap
    val sues = Stream.from(1).zip(input.map(parseSue)).toMap

    def part1 = sues.mapValues{prefs => prefs.keys.map(key => math.pow(prefs(key) - tape(key), 2)).sum}.minBy(_._2)._1

    val greaterKeys = Seq("cats", "trees")
    val smallerKeys = Seq("pomeranians", "goldfish")

    val sues2 = sues.filter { case (sue, prefs) =>
      greaterKeys.forall(key => prefs.getOrElse(key, Int.MaxValue) > tape(key)) &&
      smallerKeys.forall(key => prefs.getOrElse(key, Int.MinValue) < tape(key))
    }.mapValues(_ -- (greaterKeys ++ smallerKeys))

    def part2 = sues2.mapValues{prefs => prefs.keys.map(key => math.pow(prefs(key) - tape(key), 2)).sum}.minBy(_._2)._1
  }

  object day17 {
    val input = Source.fromFile("data/17.data").getLines().toList

    val boxes = input.map(_.toInt).sorted.reverse

    def pack(boxes: List[Int], left: Int): Int = boxes match {
      case first :: rest =>
        (if (first == left) 1 else 0) + (if (first < left) pack(rest, left - first) else 0) + pack(rest, left)
      case _ => 0
    }

    def part1 = pack(boxes, 150)

    def pack2(boxes: List[Int], n: Int, left: Int): Iterable[Int] = boxes match {
      case first :: rest =>
        (if (first == left) Some(n+1) else None) ++ (if (first < left) pack2(rest, n+1, left - first) else Nil) ++ pack2(rest, n, left)
      case _ => Nil
    }

    def part2 = pack2(boxes, 0, 150).groupBy(identity).min._2.size

  }

  object day18 {
    val input = Source.fromFile("data/18.data").getLines().toList
    val size = 100

    val state = input.map(line => line.map(Map('.' -> false, '#' -> true))).toIndexedSeq

    val surrounding = List(-1, 0, 1).flatMap(r => List(-1, 0, 1).map((r, _))).filter(_ != (0, 0))

    def get(state: IndexedSeq[IndexedSeq[Boolean]], stuck: Boolean)(p: (Int, Int)) = try {
      stuck && (p._1 == 0 || p._1 == 99) && (p._2 == 0 || p._2 == 99) || state(p._1)(p._2)
    } catch {
      case _: Throwable => false
    }

    def add(p1: (Int, Int))(p2: (Int, Int)) = (p1._1 + p2._1, p1._2 + p2._2)

    def valCount(state: IndexedSeq[IndexedSeq[Boolean]], stuck: Boolean, p: (Int, Int)) =
      (get(state, stuck)(p), surrounding.map(add(p)).count(get(state, stuck)))

    def next(state: IndexedSeq[IndexedSeq[Boolean]], stuck: Boolean) =
      (0 to size-1).map(row => (0 to size-1).map{col => valCount(state, stuck, (row, col)) match {
        case (true, n) => n == 2 || n == 3 || stuck && (row == 0 || row == 99) && (col == 0 || col == 99)
        case (false, n) => n == 3
      }})

    def part1 = (1 to size).foldLeft(state){ case (s, n) => next(s, false) }.map(_.count(identity)).sum

    def part2 = (1 to size).foldLeft(state){ case (s, n) => next(s, true) }.map(_.count(identity)).sum
  }

  object day19 {
    val input = Source.fromFile("data/19.data").getLines().toList

    val (rulesData, rest) = input.span(_ != "")
    val rules = rulesData.map(_.split(" => ")).map(l => (l(0), l(1)))
    val molecule = rest(1)

    def part1 = rules.flatMap {case (a, b) => a.r.findAllMatchIn(molecule).map(m => molecule.substring(0, m.start) + b + molecule.substring(m.end)) }.distinct.size

    val atoms = molecule.length - molecule.count(_.isLower)
    def part2 = atoms - "Rn|Ar".r.findAllMatchIn(molecule).size - "Y".r.findAllMatchIn(molecule).size*2 - 1

  }

  object day20 {
    val input = 36000000
    val sum = input / 10

    def factors(n:Int):List[Int] = {
        def divides(d:Int, n:Int) = (n % d) == 0
        def ld(n:Int):Int =  ldf(2, n)
        def ldf(k:Int, n:Int):Int = {
          if (divides(k, n)) k
          else if ((k*k) > n) n
          else ldf((k+1), n)
        }
        n match {
          case 1 => Nil
          case _ => val p = ld(n); p :: factors(n / p)
        }
      }

    def presents(n: Int) = {
      val combs = factors(n)
      (1 to combs.size).flatMap(combs.combinations).map(_.product).sum + 1
    }

    def part1 = Stream.from(2).map(n => (n, presents(n))).filter(_._2 >= sum).head._1

    val sum2 = input / 11

    def presents2(n: Int) = {
      val combs = factors(n)
      (1 to combs.size).flatMap(combs.combinations).map(_.product).filter(n / _ <= 50).sum + 1
    }

    def part2 = Stream.from(2).map(n => (n, presents2(n))).filter(_._2 >= sum2).head._1

  }

  object day21 {
    val boss = (109, 8, 2)

    case class Item(name: String, cost: Int, damage: Int, armor: Int)

    val weapons = Seq(
      Item("Dagger", 8, 4, 0),
      Item("Shortsword", 10, 5, 0),
      Item("Warhammer", 25, 6, 0),
      Item("Longsword", 40, 7, 0),
      Item("Greataxe", 74, 8, 0)
    )

    val armors = Seq(
      Item("Leather", 13, 0, 1),
      Item("Chainmail", 31, 0, 2),
      Item("Splintmail", 53, 0, 3),
      Item("Bandedmail", 75, 0, 4),
      Item("Platemail", 102, 0, 5)
    )

    val rings = Seq(
      Item("Damage +1", 25, 1, 0),
      Item("Damage +2", 50, 2, 0),
      Item("Damage +3", 100, 3, 0),
      Item("Defense +1", 20, 0, 1),
      Item("Defense +2", 40, 0, 2),
      Item("Defense +3", 80, 0, 3)
    )

    val weaponry = for {
      boughtWeapon <- weapons
      boughtArmors <- (0 to 1).flatMap(armors.combinations)
      boughtRings  <- (0 to 2).flatMap(rings.combinations)
    } yield Seq(boughtWeapon) ++ boughtArmors ++ boughtRings

    def playerWins(effect: Seq[Int]) =
      boss._1 / math.max(1, effect(0) - boss._3) <= 100 / math.max(1, boss._2 - effect(1))

    def part1 = weaponry
      .map(items => (
        items.map(_.cost).sum,
        items.map(item => List(item.damage, item.armor)).transpose.map(_.sum))
      )
      .filter(p => playerWins(p._2))
      .minBy(_._1)

    def part2 = weaponry
      .map(items => (
        items.map(_.cost).sum,
        items.map(item => List(item.damage, item.armor)).transpose.map(_.sum))
      )
      .filter(p => !playerWins(p._2))
      .maxBy(_._1)

  }

  object day22 {
    val bossDamage = 10

    case class State(myHitPoints: Int, myMana: Int, bossHitPoints: Int, penalty: Int = 0, spent: Int = 0,
                     shield: Int = 0, poison: Int = 0, recharge: Int = 0) {
      def next(best: Int) =
        if (spent >= best) Nil else List(
          spend(53,  copy(bossHitPoints = bossHitPoints - 4)),
          spend(73,  copy(bossHitPoints = bossHitPoints - 2, myHitPoints = myHitPoints + 2)),
          spend(113, copy(shield = 6), shield == 0),
          spend(173, copy(poison = 6), poison == 0),
          spend(229, copy(recharge = 5), recharge == 0)
        ).flatten.map(_.applyRules.copy(myHitPoints = myHitPoints - penalty))

      def spend(mana: Int, state: => State, cond: => Boolean = true) =
        if (mana <= myMana && cond) Some(state.copy(myMana = myMana - mana, spent = spent + mana)) else None

      def playBoss = copy(myHitPoints = myHitPoints - math.max(1, bossDamage - (if (shield > 0) 7 else 0))).applyRules
      def bossLost = bossHitPoints <= 0
      def alive = myHitPoints > 0

      def applyShield = if (shield > 0) copy(shield = shield - 1) else this
      def applyPoison = if (poison > 0) copy(bossHitPoints = bossHitPoints - 3, poison = poison - 1) else this
      def applyRecharge = if (recharge > 0) copy(myMana = myMana + 101, recharge = recharge - 1) else this

      def applyRules = applyShield.applyPoison.applyRecharge
    }

    def play(best: Int, state: State): Int = {
      val (won, afterMe) = state.next(best).partition(_.bossLost)
      val (won2, afterBoss) = afterMe.map(_.playBoss).partition(_.bossLost)
      val newBest = (won ++ won2).map(_.spent).foldLeft(best)(_ min _)
      afterBoss.filter(_.alive).foldLeft(newBest)(play)
    }

    def part1 = play(Int.MaxValue, State(50, 500, 71))

    def part2 = play(Int.MaxValue, State(50, 500, 71, penalty = 1))

  }

  object day23 {
    val input = Source.fromFile("data/23.data").getLines().toVector.map(_.split("[ ,]+"))

    def eval(i: Int, registers: Map[String, Long]): Map[String, Long] =
      if (i >= input.size)
        registers
      else input(i) match {
        case Array("hlf", r)    => eval(i+1, registers + (r -> (registers(r) / 2L)))
        case Array("tpl", r)    => eval(i+1, registers + (r -> (registers(r) * 3L)))
        case Array("inc", r)    => eval(i+1, registers + (r -> (registers(r) + 1L)))
        case Array("jmp", s)    => eval(i + s.toInt, registers)
        case Array("jie", r, s) => eval(i + (if (registers(r) % 2 == 0) s.toInt else 1), registers)
        case Array("jio", r, s) => eval(i + (if (registers(r) == 1) s.toInt else 1), registers)
        case s                  => throw new Exception(s"Illegal instruction: $s")
      }

      def part1 = eval(0, Map[String, Long]("a" -> 0L, "b" -> 0L))("b")

      def part2 = eval(0, Map[String, Long]("a" -> 1L, "b" -> 0L))("b")
  }

  object day24 {
    val input = Source.fromFile("data/24.data").getLines().toList.map(_.toLong)
    val total = input.sum

    def balanced(parts: Int)(l: List[Long]) = Iterator.from(1).exists(n => (input.toSet -- l).toList.combinations(n).exists(_.sum == total/parts))

    def smallest(parts: Int) = Iterator.from(1)
      .map(input.combinations)
      .map(_.filter(_.sum == input.sum / parts))
      .filter(_.nonEmpty)
      .next()
      .filter(balanced(parts))
      .map(l => (l.product, l))
      .minBy(_._1)

    def part1 = smallest(3)

    def part2 = smallest(4)
  }

  object day25 {
    val row = 3010
    val col = 3019
    val start = 20151125l
    val multiplier = 252533l
    val divisor = 33554393l

    val repeats = (1 to row + col - 2).foldLeft(1)(_ + _) + col - 1

    def part1 = (2 to repeats).foldLeft(start){ case (prev, n) => prev * multiplier % divisor }

    def part2 = ??? // It's the right answer, believe it or not...
  }

  def main(args: Array[String]) {
    println(day25.part1)
    println(day25.part2)
  }
}
