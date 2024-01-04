package Aoc2023
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.io.Source

object Day25 {
  val input = Source.fromFile("data2023/25").mkString.split("\n")

  val graph = input.flatMap(_.split(": ") match {
    case Array(left, right) => right.split(" ").flatMap(r => List((left, r), (r, left)))
  }).groupBy(_._1).map{case (node, links) => (node, links.map(_._2).toSet)}.toMap

  def createEdge(node1: String, node2: String) = if (node1 < node2) (node1, node2, 1) else (node2, node1, 1)

  val edges = input.flatMap(_.split(": ") match {
    case Array(left, right) =>
      right.split(" ").map(link => createEdge(left, link))
  }).toSet

  def findAndRemoveShortest(node1: String, node2: String, edges: collection.Set[(String, String, Int)]) = {
    val path = DSPA.run[String](edges, node1, directed = false)(node2)
      ._2.sliding(2).map { case List(l, r) => createEdge(l, r) }.toList
    (edges -- path, path)
  }

  def checkPaths(node1: String, node2: String) = {
    val (edges1, _) = findAndRemoveShortest(node1, node2, edges - createEdge(node1, node2))
    val (edges2, _) = findAndRemoveShortest(node1, node2, edges1)
    val (_, path) = findAndRemoveShortest(node1, node2, edges2)
    path.isEmpty
  }

  lazy val cuts = edges.filter { case (node1, node2, _) => checkPaths(node1, node2)}
  lazy val cutEdges = edges -- cuts

  def neighbors(node: String) =
    cutEdges.filter(edge => edge._1 == node || edge._2 == node)
      .flatMap(edge => List(edge._1, edge._2))
      .filterNot(_ == node)

  @tailrec
  def clusterSize(queue: List[String], visited: Set[String] = Set()): Int = queue match {
    case Nil => visited.size
    case node :: nodes if visited(node) => clusterSize(nodes, visited)
    case node :: nodes =>
      clusterSize(neighbors(node).toList ++ nodes, visited + node)
  }

  lazy val part1 = {
    val size = clusterSize(List(graph.head._1))
    (graph.size-size) * size
  }
}

object Day25Runner {
  def main(args: Array[String]) {
    println(LocalDateTime.now())
    println(Day25.part1)
    println(LocalDateTime.now())
  }
}
