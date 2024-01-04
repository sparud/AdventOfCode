package Aoc2023

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

  def QueueOrdering[A] = new Ordering[Node[A]] {
    def compare(a:Node[A], b:Node[A]) = b.distance - a.distance
  }

  // Take edges as a list of tuples (NodeName, NodeName, Weight),
  // a start NodeName and an optional boolean that indicates whether the graph is directed
  // (default is false = undirected graph)
  def run[A](edges:scala.collection.Set[(A, A, Int)], start:A, directed:Boolean = false) = {
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
