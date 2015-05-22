package graphs

import scala.collection.mutable.Stack


case class Vertex[T](data: T)

case class Edge[T](v1: Vertex[T], v2: Vertex[T])

class Graph[T](E: List[Edge[T]]) {

  //get all vertices
  val V = E.flatMap(e => List(e.v1, e.v2)).distinct

  //Map vertex -> neighbouring vertices
  val neighbours: Map[Vertex[T], Set[Vertex[T]]] = V.map { v =>
    (v,
      E.filter(e => (e.v1 == v || e.v2 == v))
        .map(x => if (x.v1 == v) x.v2 else x.v1).toSet)
  }.toMap


  /**
   * Depth-First Search non-recursive implementation.
   * @param startNode Node to start searching from.
   * @param targetNode Node to search for.
   * @return Boolean indicating whether node was found in graph
   */
  def dfs(startNode: Vertex[T], targetNode: Vertex[T]): Boolean = {

    val s: Stack[Vertex[T]] = new Stack()

    var nodesSeen: Set[Vertex[T]] = Set.empty

    s.push(startNode)

    //pop stack and push neighbours until found
    while (!s.isEmpty) {

      var top = s.top

      if (top == targetNode) return true

      s.pop()

      if (!nodesSeen.contains(top)) {
        nodesSeen += top
        neighbours(top).foreach(n => s.push(n))
        nodesSeen += top
      }

    }

    false
  }
}
