package Aoc2023
import java.time.LocalDateTime
import scala.io.Source

import scala.collection.mutable.{Set, Map, HashSet, HashMap, Queue, PriorityQueue}

case class Edge[A](node: Node[A], weight: Int)

class Node[A](val name: A) {
  private val _edges:Set[Edge[A]] = new HashSet[Edge[A]]
  var distance = Integer.MAX_VALUE
  var visited = false
  var previous:Node[A] = null

  def addEdge(edge:Edge[A]) {
    _edges += edge
  }

  def edges = _edges.toList
}

object DSPA {

  // For the purposes of the DSPA algorithm, nodes will be ordered
  // according to the distance from source
  // PriorityQueue orders according to highest priority first and
  // we want elements with shorter distances to have priority, so
  // we reverse the operation
  def QueueOrdering[A] = new Ordering[Node[A]] {
    def compare(a:Node[A], b:Node[A]) = b.distance - a.distance
  }

  // Take edges as a list of tuples (NodeName, NodeName, Weight),
  // a start NodeName and an optional boolean that indicates whether the graph is directed
  // (default is false = undirected graph)
  def run[A](edges:List[(A, A, Int)], start:A, directed:Boolean = false) = {
    val graph:Map[A, Node[A]] = new HashMap[A, Node[A]]

    // Traverse edges list and build graph structure
    // If the graph is directed, we build the a -> b edge only
    // If the graph is undirected, we build the b -> a edge as well
    for ((a, b, w) <- edges) {
      val na = graph.getOrElseUpdate(a, new Node(a))
      val nb = graph.getOrElseUpdate(b, new Node(b))
      na.addEdge(Edge(nb, w))
      if ( !directed )
        nb.addEdge(Edge(na, w))
    }

    // Queue of nodes not yet visited, prioritised by distance from source
    val newNodes = new PriorityQueue[Node[A]]()(QueueOrdering[A])

    // Initial node has a distance of 0
    val startNode = graph(start)
    startNode.distance = 0

    // Add initial node to the newNodes queue
    newNodes += startNode

    // Get closest unvisited node. For each edge leading to an unvisited node,
    // if the distance to that node though here is smaller than any previously computed distance,
    // update that node and insert it into the new node queue. Finally, flag the current node
    // as visited.
    while ( newNodes.nonEmpty ) {
      val node = newNodes.dequeue
      if (!node.visited) {
        for (edge <- node.edges) {
          val otherNode = edge.node
          if (!otherNode.visited && otherNode.distance > node.distance + edge.weight) {
            otherNode.distance = node.distance + edge.weight
            otherNode.previous = node
            newNodes += otherNode
          }
        }
        node.visited = true
      }
    }

    // Helper function that retrieves the shortest path to a node
    def path(node:Node[A], acc:List[A] = List()):List[A] = node match {
      case `startNode` => startNode.name :: acc
      case null        => List()
      case _           => path(node.previous, node.name :: acc)
    }

    // Return results as a map of NodeName -> (ShortestDistance, ShortestPath)
    for ((name, node) <- graph) yield (name, (node.distance, path(node)))
  }
}

object Day17 {
  case class Pos(row: Int, col: Int) {
    def +(other: Pos) = Pos(row + other.row, col + other.col)
    def *(factor: Int) = Pos(row*factor, col*factor)
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

  val input = Source.fromFile("data2023/17").mkString.split("\n")

  val height = input.size
  val width = input.head.length

  def inside(pos: Pos) = pos.row >= 0 && pos.row < height && pos.col >= 0 && pos.col < width

  val grid = input.zipWithIndex.flatMap{case (row, rowNo) =>
    row.zipWithIndex.map{ case (c, colNo) => (Pos(rowNo, colNo) -> (c-'0'))}
  }.toMap

  case class NodeId(pos: Pos, dir: Dir, n: Int) {
    def go(newDir: Dir): NodeId = NodeId(pos+newDir, newDir, 0)
    def canGo(newDir: Dir) = newDir match {
      case Right => pos.col + 3 < width
      case Left => pos.col - 3 >= 0
      case Up => pos.row - 3 >= 0
      case Down => pos.row + 3 < height
    }
  }

  def buildGraph() = {
    val queue = new Queue[NodeId]
    queue.enqueue(NodeId(Pos(0, 0), Right, 0))
    queue.enqueue(NodeId(Pos(0, 0), Down, 0))
    val links = HashSet[(NodeId, NodeId)]()

    def add(from: NodeId, to: NodeId): Unit = {
      val limkId = (from, to)
      if (inside(to.pos) && !links.contains(limkId)) {
        links.add(limkId)
        queue.enqueue(to)
      }
    }

    while (queue.nonEmpty) {
      val start = queue.dequeue()
      if (start.n < 2)
        add(start, start.copy(pos=start.pos+start.dir, n=start.n+1))
      add(start, start.go(start.dir.turnLeft))
      add(start, start.go(start.dir.turnRight))
    }
    links.map{ case (from, to) => (from, to, grid(to.pos))}.toList
  }

  lazy val part1 = {
    val edges = buildGraph()
    val paths = DSPA.run[NodeId](edges, NodeId(Pos(0, 0), Right, 0), true)
    paths.filterKeys(nodeId => nodeId.pos == Pos(height-1, width-1))
      .valuesIterator.map(_._1).toList.min
  }

  def buildGraph2() = {
    val queue = new Queue[NodeId]
    queue.enqueue(NodeId(Pos(0, 0), Right, 0))
    queue.enqueue(NodeId(Pos(0, 0), Down, 0))
    val links = HashSet[(NodeId, NodeId)]()

    def add(from: NodeId, to: NodeId): Unit = {
      val linkId = (from, to)
      if (inside(to.pos) && !links.contains(linkId)) {
        links.add(linkId)
        queue.enqueue(to)
      }
    }

    while (queue.nonEmpty) {
      val start = queue.dequeue()
      if (start.n < 10) {
        add(start, start.copy(pos = start.pos + start.dir, n = start.n + 1))
        if (start.n > 2) {
          if (start.canGo(start.dir.turnLeft))
            add(start, start.go(start.dir.turnLeft))
          if (start.canGo(start.dir.turnRight))
            add(start, start.go(start.dir.turnRight))
        }
      }
    }
    links.map { case (from, to) => (from, to, grid(to.pos)) }.toList
  }

  lazy val part2 = {
    val edges = buildGraph2()
    val paths = DSPA.run[NodeId](edges, NodeId(Pos(0, 0), Down, 0), true)
    paths.filterKeys(nodeId => nodeId.pos == Pos(height - 1, width - 1))
      .valuesIterator.map(_._1).toList.min
  }
}

object Day17Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day17.part2)
    println(LocalDateTime.now())
  }
}
