import java.security.MessageDigest
import javax.xml.bind.DatatypeConverter._

import scala.collection.mutable
import scala.io.Source
import Stream._

object aoc2016 {
  object day1 {
    val turns = Map('L' -> -1, 'R' -> 1)
    val input = Source.fromFile("data2016/1").mkString.split(", ")
      .map(move => (turns(move(0)), move.substring(1).toInt)).toList
    val xdirs = Map(0 -> 0, 1 -> 1, 2 -> 0, 3 -> -1)
    val ydirs = Map(0 -> 1, 1 -> 0, 2 -> -1, 3 -> 0)
    val positions = input.scanLeft((0, List((0, 0)))){ case ((dir, coords), (turn, blocks)) =>
      val newdir = (dir + turn + 4) % 4
      val (x, y) = coords.last
      (newdir, (1 to blocks).map(steps => (x + xdirs(newdir)*steps, y + ydirs(newdir)*steps)).toList)
    }.flatMap(_._2)

    val (endX, endY) = positions.last
    val part1 = Math.abs(endX) + Math.abs(endY)

    val (endX2, endY2) = positions.foldLeft((Set[(Int, Int)](), List[(Int, Int)]())) {
      case ((seen, doubles), p) => (seen + p, if (seen.contains(p)) p :: doubles else doubles)
    }._2.last
    val part2 = Math.abs(endX2) + Math.abs(endY2)
  }

  object day2 {
    case class Pos(x: Int, y: Int) {
      def +(other: Pos) = Pos(x + other.x, y + other.y)
    }

    val dirs = Map('U' -> Pos(0, -1), 'R' -> Pos(1, 0), 'D' -> Pos(0, 1), 'L' -> Pos(-1, 0))
    val input = Source.fromFile("data2016/2").getLines.map(_.map(dirs)).toSeq

    def move(f: Pos => Boolean)(p1: Pos, p2: Pos) = if (f(p1+p2)) p1+p2 else p1

    def follow(f: Pos => Boolean)(start: Pos, steps: Seq[Pos]) = (start /: steps)(move(f))

    def code(f: Pos => Boolean, start: Pos, moves: Seq[Seq[Pos]]) = moves.scanLeft(start)(follow(f)).toList.tail

    def solve(keyboard: String, moves: Seq[Seq[Pos]]) = {
      val layout = keyboard.split(",").map(_.zipWithIndex).zipWithIndex
        .flatMap{ case (cols, y) => cols.flatMap { case (c, x) => if (c == ' ') None else Some(Pos(x, y) -> c) }}
        .toMap
      code(layout.contains, layout.map(_.swap).apply('5'), moves).map(layout).mkString
    }

    val part1 = solve("123,456,789", input)
    val part2 = solve("  1  , 234 ,56789, ABC ,  D  ", input)
  }

  object day3 {
    val input = Source.fromFile("data2016/3").getLines.toList.map(_.split(" +").tail.map(_.toInt).toList)

    def triangular(l: Seq[Int]) = l.sorted.take(2).sum > l.max

    val part1 = input.count(triangular)

    val part2 = input.grouped(3).flatMap(_.transpose).count(triangular)
  }

  object day4 {
    val p = """([a-z-]+)([0-9]+)\[([a-z]+)\]""".r
    def parse(s: String) = s match {
      case p(a, b, c) => (a.filter(_ != '-'), b.toInt, c)
    }

    val input = Source.fromFile("data2016/4").getLines.map(parse).toList

    def calcChecksum(s: String) =
      s.groupBy(identity).mapValues(-_.length).toList.map(_.swap).sorted.map(_._2).mkString.take(5)

    def ok(tuple: (String, Int, String)) = if (calcChecksum(tuple._1) == tuple._3) Some(tuple._2) else None

    val part1 = input.flatMap(ok).sum

    val transform = ('z' -> 'a' :: (('a' to 'y') map (c => c -> (c.toByte+1).toChar)).toList).toMap

    def repeat[A](n: Int, a: A, f: A => A) = (a /: (1 to n)) { case (b, _) => f(b) }

    def decode(tuple: (String, Int, String)) = repeat(tuple._2 % 26, tuple._1,  {s: String => s.map(transform)})

    val part2 = input.map(tuple => (tuple._2, decode(tuple))).find(_._2 == "northpoleobjectstorage").map(_._1).get
  }

  object day5 {
    val doorId = "reyedfim"

    def md5(s: String) = printHexBinary(MessageDigest.getInstance("MD5").digest(s.getBytes))

    val part1 = Stream.from(0).map(n => md5(doorId + n)).filter(_.startsWith("00000")).take(8).map(_(5)).mkString

    val part2 = Stream.from(0).map(n => md5(doorId + n))
      .filter(h => h.startsWith("00000") && h(5) >= '0' && h(5) <= '7').scanLeft(List[(Char, Char)]()){
      case (state, hash) => if (state.map(_._1).contains(hash(5))) state else (hash(5), hash(6)) :: state
    }.find(_.size == 8).get.sorted.map(_._2).mkString
  }


  object day6 {
    val part1 = Source.fromFile("data2016/6")
      .getLines.toSeq.transpose.map(_.groupBy(identity).mapValues(_.size).map(_.swap).toSeq.sorted.last._2).mkString

    val part2 = Source.fromFile("data2016/6")
      .getLines.toSeq.transpose.map(_.groupBy(identity).mapValues(_.size).map(_.swap).toSeq.sorted.head._2).mkString

  }

  object day7 {
    def parse(s: String) = s.split("\\[|\\]").toList

    val input = Source.fromFile("data2016/7").getLines.map(parse).toList.map(_.map(_.toList))

    def everyother[T](isLeft: Boolean)(s: List[T]): (List[T], List[T]) = s match {
      case Nil => (List[T](), List[T]())
      case c :: rest =>
        val (left, right) = everyother(!isLeft)(rest)
        if (isLeft) (c :: left, right) else (left, c:: right)
    }

    def abba(s: List[Char]): Boolean = s match {
      case a :: b :: c :: d :: rest => a == d && b == c && a != b || abba(b :: c :: d :: rest)
      case _ => false
    }

    val part1 = input.map(everyother(isLeft=true)).count{ case (musts, mustnots) => musts.exists(abba) && !mustnots.exists(abba) }

    def aba(s: List[Char]) = s.sliding(3).flatMap { case l@(a :: b :: c :: Nil) => if (a == c && a != b) Some(l) else None }
    def aba2bab(s: String) = "" + s(1) + s(0) + s(1)
    def abasSet(parts: List[List[Char]]) = parts.flatMap(aba).map(_.mkString).toSet

    val part2 = input.map(everyother(isLeft=true)).count{ case (outer, inner) =>
      abasSet(outer).intersect(abasSet(inner).map(aba2bab)).nonEmpty
    }

  }

  object day8 {
    val input = Source.fromFile("data2016/8").getLines.toList
    val ROWS = 6
    val COLUMNS = 50

    val state = Array.fill(ROWS, COLUMNS)(false)

    def rotate[T](list: List[T]) = list.last :: list.slice(0, list.size-1)
    def repeat[T](n: Int, f: T => T, start: T) = (start /: (1 to n)){case p  => f(p._1)}

    input.foreach(_.split(" ") match {
      case Array("rotate", "column", xeq, "by", nval) =>
        val col = xeq.split("=").last.toInt
        val n = nval.toInt
        repeat(n, rotate[Boolean], (0 until ROWS).map(row => state(row)(col)).toList).zip(0 until ROWS).foreach { case (v, row) =>
          state(row)(col) = v
        }
      case Array("rotate", "row", yeq, "by", nval) =>
        val row = yeq.split("=").last.toInt
        val n = nval.toInt
        repeat(n, rotate[Boolean], state(row).toList).zip(0 until COLUMNS).foreach { case (v, col) =>
          state(row)(col) = v
        }
      case Array("rect", colsbyrows) =>
        val Array(cols, rows) = colsbyrows.split("x")
        (0 until rows.toInt).foreach(row => (0 until cols.toInt).foreach(col => state(row)(col) = true))
    })

    val part1 = state.map(_.count(identity)).sum

    // Part 2: look at the result of this:
    println(state.map(_.map(v => Map(false -> '.', true -> '#')(v)).mkString).mkString("\n"))
  }

  object day11 {
    val subjects = List("strontium", "plutonium", "thulium", "ruthenium", "curium")
    val FLOORS = 4

    case class Floor(chips: Set[Char] = Set(), generators: Set[Char] = Set()) {
      def ok      = (chips -- generators).isEmpty || (generators -- chips).isEmpty
      def isEmpty = chips.isEmpty && generators.isEmpty
      def moves   = {
        val chipsMoves = List() :: chips.map(List(_)).toList ++ chips.toList.combinations(2)
        val generatorMoves = List() :: generators.map(List(_)).toList ++ generators.toList.combinations(2)
        chipsMoves.flatMap(cs => generatorMoves.flatMap{gs =>
          val len = cs.size + gs.size
          if (len > 0 && len <= 2) Some(Floor(cs.toSet, gs.toSet)) else None
        })
      }
      def +(other: Floor) = Floor(chips ++ other.chips, generators ++ other.generators)
      def -(other: Floor) = Floor(chips -- other.chips, generators -- other.generators)
      override def toString = chips.toList.sorted.mkString + ":" + generators.toList.sorted.mkString
    }

    case class State(elevator: Int, floors: List[Floor]) {
      def id    = elevator.toString + floors.map(_.toString).mkString
      def ok    = floors.forall(_.ok)
      def done  = floors.tail.forall(_.isEmpty)
      def moves = Stream(elevator + 1, elevator -1)
        .filter(f => f >=0 && f < FLOORS)
        .flatMap(f => floors(elevator).moves.map(move =>
          replaceFloor(elevator, floors(elevator) - move).replaceFloor(f, floors(f) + move).elevateTo(f)))
        .filter(_.ok)

      def replaceFloor(n: Int, floor: Floor) =
        State(elevator, floors.zip(0 until FLOORS).map{ case (f, i) => if (i == n) floor else f })
      def elevateTo(n: Int) = State(n, floors)
      override def toString = s"$elevator#${floors.map(_.toString).mkString(",")}"
    }

    var visited = mutable.Set[String]()
    def unseenState(state: State) = {
      val id = state.toString
      !visited.contains(id) && visited.add(id)
    }

    def path(distance: Int)(states: Stream[State]): Stream[(State, Int)] = {
      val next = states.filter(unseenState)
      println(distance)
      //next.foreach(println)
      next.map((_, distance + 1)).map{x => println(x); x} ++ path(distance + 1)(next.flatMap(_.moves))
    }

    def neighborsWithHistory(state: State): Stream[State] =
      state.moves


    def newNeighborsOnly(neighbors: Stream[State], explored: Set[State]): Stream[State] =
        neighbors.filter(!explored.contains(_))

    def from(distance: Int,
             initial: Stream[State],
             explored: Set[State]): Stream[(State, Int)] = {
      val neighbors = for {
        state <- initial
        neighbor <- neighborsWithHistory(state)
      } yield neighbor
      val newNeighbors = newNeighborsOnly(neighbors, explored)
      //println(s"New :${newNeighbors.toList}")
      newNeighbors.map((_, distance)) ++ (
        if (newNeighbors.isEmpty)
          Stream.empty
        else
          from(distance+1, newNeighbors, explored ++ newNeighbors))
    }



    val initialState1 = State(3, List(
      Floor(),
      Floor(chips = Set('t')),
      Floor(chips = Set('r', 'c'), generators = Set('t', 'r', 'c')),
      Floor(chips = Set('s', 'p'), generators = Set('s', 'p'))
    ))

    val initialState = State(3, List(
      Floor(),
      Floor(generators = Set('l')),
      Floor(generators = Set('h')),
      Floor(chips = Set('l', 'h'))
    ))

    lazy val pathsFromStart: Stream[(State, Int)] =
        from(0, Stream(initialState), Set(initialState))

    lazy val pathsToGoal: Stream[(State, Int)] = {
      pathsFromStart.filter{case (state, distance) => state.done}
    }

    lazy val solution = pathsToGoal.headOption match {
      case None => ???
      case Some((state, distance)) => (state, distance)
    }

    val part1 = solution // path(0)(Stream(initialState)) // .filter(_._1.done).head._2
  }

  def main(args: Array[String]) {
    println(day11.part1 /*.take(1).map(_.toString).mkString("\n")*/)
    //println(day8.part2)
  }
}