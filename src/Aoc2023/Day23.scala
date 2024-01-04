package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day23 {
  val input1 =
    """#.#####################
      |#.......#########...###
      |#######.#########.#.###
      |###.....#.>.>.###.#.###
      |###v#####.#v#.###.#.###
      |###.>...#.#.#.....#...#
      |###v###.#.#.#########.#
      |###...#.#.#.......#...#
      |#####.#.#.#######.#.###
      |#.....#.#.#.......#...#
      |#.#####.#.#.#########v#
      |#.#...#...#...###...>.#
      |#.#.#v#######v###.###v#
      |#...#.>.#...>.>.#.###.#
      |#####v#.#.###v#.#.###.#
      |#.....#...#...#.#.#...#
      |#.#########.###.#.#.###
      |#...###...#...#...#.###
      |###.###.#.###v#####v###
      |#...#...#.#.>.>.#.>.###
      |#.###.###.#.###.#.#v###
      |#.....###...###...#...#
      |#####################.#""".stripMargin.split("\n")

  val input = Source.fromFile("data2023/23").mkString.split("\n")

  case class Pos(row: Int, col: Int) {
    def +(other: Pos) = Pos(row + other.row, col + other.col)

    def *(factor: Int) = Pos(row * factor, col * factor)

    def go(dir: Dir, steps: Int) = (1 to steps).map(copy() + dir * _).toList
  }

  class Dir(row: Int, col: Int, val isHorizontal: Boolean) extends Pos(row, col) {
    def turnLeft: Dir = new Dir(-col, row, !isHorizontal)

    def turnRight: Dir = new Dir(col, -row, !isHorizontal)

    val isVertical = !isHorizontal
  }

  object Right extends Dir(0, 1, true)

  object Left extends Dir(0, -1, true)

  object Up extends Dir(-1, 0, false)

  object Down extends Dir(1, 0, false)

  val grid = input.zipWithIndex.flatMap { case (row, rowNo) => row.zipWithIndex.map { case (col, colNo) =>
    (Pos(rowNo, colNo), col)
  }
  }.toMap

  def isWall(p: Pos) = grid.get(p).forall(_ == '#')

  def canGo(dir: Dir, ch: Char) = ch match {
    case '>' => dir == Right
    case '<' => dir == Left
    case '^' => dir == Up
    case 'v' => dir == Down
    case _ => true
  }

  def start = Pos(0, input.head.indexOf("."))

  def goal = Pos(input.length - 1, input.last.indexOf("."))

  case class Entry(start: Pos, dir: Dir, current: Pos, length: Int = 1)

  case class Edge(from: Pos, to: Pos, length: Int)

  class Part1 {
    def canGo(dir: Dir, ch: Char) = ch match {
      case '>' => dir == Right
      case '<' => dir == Left
      case '^' => dir == Up
      case 'v' => dir == Down
      case _ => true
    }

    @tailrec
    final def findEdges(queue: List[Entry], edges: List[Edge] = Nil, seen: Set[(Pos, Dir)] = Set()): List[Edge] = queue match {
      case Nil => edges
      case entry :: entries if seen((entry.current, entry.dir)) => findEdges(entries, edges, seen)
      case entry :: entries if !canGo(entry.dir, grid(entry.current)) => findEdges(entries, edges, seen)
      case entry :: entries if entry.current == goal =>
        findEdges(entries, Edge(entry.start, entry.current, entry.length) :: edges, seen + ((entry.current, entry.dir)))
      case entry :: entries =>
        val dirs = List(entry.dir, entry.dir.turnLeft, entry.dir.turnRight).filterNot(d => isWall(entry.current + d))
        if (dirs.length == 1)
          findEdges(Entry(entry.start, dirs.head, entry.current + dirs.head, entry.length + 1) :: entries, edges, seen + ((entry.current, entry.dir)))
        else
          findEdges(dirs.map(d => Entry(entry.current, d, entry.current + d)) ++ queue, Edge(entry.start, entry.current, entry.length) :: edges, seen + ((entry.current, entry.dir)))
    }

    case class Route(nodes: List[Pos], length: Int = 0)

    @tailrec
    final def findRoutes(queue: List[Route], full: List[Route] = Nil): List[Route] = queue match {
      case Nil => full
      case route :: routes if route.nodes.head == goal => findRoutes(routes, route :: full)
      case route :: routes if route.nodes.tail.contains(route.nodes.head) => findRoutes(routes, full)
      case route :: routes =>
        val next = edges
          .filter(e => e.from == route.nodes.head)
          .map(e => Route(e.to :: route.nodes, e.length + route.length))
        findRoutes(next ++ routes, full)
    }

    lazy val edges = findEdges(List(Entry(start, Down, start)))

    def go() = findRoutes(edges.filter(_.from == start).map(p => Route(List(p.from)))).map(_.length - 1).max
  }


  lazy val part1 = new Part1().go()

  class Part2 extends Part1 {
    override def canGo(dir: Dir, ch: Char) = true
  }

  lazy val part2 = new Part2().go()
}

object Day23Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day23.part2)
    println(LocalDateTime.now())
  }
}
