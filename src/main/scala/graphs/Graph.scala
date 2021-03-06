package graphs

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue
import scala.collection.mutable.Stack


case class Vertex[T](data: T)

case class Edge[T](v1: Vertex[T], v2: Vertex[T])

class Graph[T](E: Map[Edge[T], Double])(implicit ord: Ordering[Double]) {

  //get all vertices
  val V = E.keys.flatMap(e => List(e.v1, e.v2)).toList.distinct

  //Map vertex -> neighbouring vertices
  val neighbours: Map[Vertex[T], Set[Vertex[T]]] = V.map { v =>
    (v,
      E.keys.filter(e => (e.v1 == v || e.v2 == v))
        .map(x => if (x.v1 == v) x.v2 else x.v1).toSet)
  }.toMap


  /**
   * Depth-First Search non-recursive implementation.
   * @param start Node to start searching from.
   * @param target Node to search for.
   * @return Boolean indicating whether node was found in graph
   */
  def dfs(start: Vertex[T], target: Vertex[T]): Boolean = {

    val s: Stack[Vertex[T]] = new Stack()

    var nodesSeen: Set[Vertex[T]] = Set.empty

    s.push(start)

    //pop stack, check if it is target, if yes return true, if no push neighbours and repeat until found or return false
    while (!s.isEmpty) {

      var top = s.top

      if (top == target) return true

      s.pop()

      if (!nodesSeen.contains(top)) {
        nodesSeen += top
        neighbours(top).foreach(n => s.push(n))
        nodesSeen += top
      }
    }

    false
  }

  /**
   * Breadth-First Search non-recursive implementation.
   * @param start Node to start searching from.
   * @param target Node to search for.
   * @return Boolean indicating whether node was found in graph
   */
  def bfs(start: Vertex[T], target: Vertex[T]): Boolean = {

    val s: Queue[Vertex[T]] = new Queue()

    var nodesSeen: Set[Vertex[T]] = Set.empty

    s.enqueue(start)

    nodesSeen += start

    //dequeue front of queue, check if target, if yes return true, if not enqueue unseen neighbours and repeat
    while (!s.isEmpty) {

      var top = s.front

      if (top == target) return true

      s.dequeue()

      neighbours(top)
        .filter(n => !nodesSeen.contains(n))
        .foreach { x =>
        s.enqueue(x) //add unvisited neighbours to queue
        nodesSeen += x //mark them as visited
      }
    }

    false
  }

  /**
   * Find the shortest path from the start vertex to end vertex using Dijkstra's algorithm
   * @param start Vertex to start from.
   * @param end Vertex to end at.
   * @return Cost of shortest path. -1 if no path exists.
   */
  def shortestPath(start: Vertex[T], end: Vertex[T]): Double = {

    //Ordering for priority Queue
    def vertexOrdering = new Ordering[(Vertex[T], Double)] {
      def compare(a: (Vertex[T], Double), b: (Vertex[T], Double)) = ord.reverse.compare(a._2, b._2)
    }

    val s: PriorityQueue[(Vertex[T], Double)] = new PriorityQueue()(vertexOrdering)

    //map to hold mutable cumulative weights
    var distances:Map[Vertex[T],Double]=Map(start -> 0.0)

    s.enqueue((start, 0.0))

    //Dijkstra's algorithm: Vertex v popped will be one with minimum cumulative weight.  For each of it's neighbors x,
    //if going to x from v has lower cumulative weight than before then replace cumulative weight.  Once end vertex is
    //popped we have the minimum cumulative weight from start to end vertex.
    while (!s.isEmpty) {

      val top = s.head

      val v = top._1
      val d = top._2

      if (v == end) return d

      s.dequeue()

      neighbours(v)
        .foreach { x =>

        val neighborCost = if (E.contains(Edge(v, x))) E(Edge(v, x)) else E(Edge(x, v))  //this could be handled better
        val alt=distances(v)+neighborCost

          if(!distances.contains(x) || alt < distances(x)){
            distances+= (x -> alt)
            s.enqueue((x, alt))
          }
      }
    }
    -1
  }

}
