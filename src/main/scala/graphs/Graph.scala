package graphs

import scala.collection.mutable.Queue
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
  def bfs(start:Vertex[T],target:Vertex[T]):Boolean={

    val s: Queue[Vertex[T]] = new Queue()

    var nodesSeen: Set[Vertex[T]] = Set.empty

    s.enqueue(start)

    nodesSeen+=start

    //dequeue front of queue, check if target, if yes return true, if not enqueue unseen neighbours and repeat
    while(!s.isEmpty){

      var top=s.front

      if(top==target) return true

      s.dequeue()

      neighbours(top)
        .filter(n=> !nodesSeen.contains(n))
        .foreach{ x=>
          s.enqueue(x)        //add unvisited neighbours to queue
          nodesSeen += x      //mark them as visited
      }
    }

    false
  }
}
