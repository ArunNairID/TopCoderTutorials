import graphs.{Graph, Edge, Vertex}
import org.scalatest.{WordSpec, Matchers}


class GraphSpec extends WordSpec with Matchers {


  "Graph" should {

    "find the correct neighbours" in {

      val A = Vertex("A")
      val B = Vertex("B")
      val C = Vertex("C")
      val D = Vertex("D")
      val E = Vertex("E")
      val F = Vertex("F")
      val G = Vertex("G")

      val edges = Map(
        (Edge(A, B) -> 0.0),
        (Edge(A, C) -> 0.0),
        (Edge(C, D) -> 0.0),
        (Edge(D, E) -> 0.0),
        (Edge(E, F) -> 0.0),
        (Edge(F, G) -> 0.0),
        (Edge(C, F) -> 0.0),
        (Edge(D, G) -> 0.0)
      )

      val g = new Graph(edges)

      val neighbours = g.neighbours

      neighbours(A) should equal(Set(B, C))
      neighbours(B) should equal(Set(A))
      neighbours(C) should equal(Set(D, A, F))
      neighbours(D) should equal(Set(C, G, E))
      neighbours(E) should equal(Set(F, D))
      neighbours(F) should equal(Set(C, G, E))
      neighbours(G) should equal(Set(D, F))
    }


    "perform DFS correctly" in {

      val A = Vertex("A")
      val B = Vertex("B")
      val C = Vertex("C")
      val D = Vertex("D")
      val E = Vertex("E")
      val F = Vertex("F")
      val G = Vertex("G")

      val edges = Map(
        (Edge(A, B) -> 0.0),
        (Edge(A, C) -> 0.0),
        (Edge(C, D) -> 0.0),
        (Edge(D, E) -> 0.0),
        (Edge(E, F) -> 0.0),
        (Edge(F, G) -> 0.0),
        (Edge(C, F) -> 0.0),
        (Edge(D, G) -> 0.0)
      )

      val g = new Graph(edges)

      val newVert = Vertex("Z")

      g.dfs(A, F) should equal(true)
      g.dfs(A, newVert) should equal(false)

    }

    "perform BFS correctly" in {

      val A = Vertex("A")
      val B = Vertex("B")
      val C = Vertex("C")
      val D = Vertex("D")
      val E = Vertex("E")
      val F = Vertex("F")
      val G = Vertex("G")

      val edges = Map(
        (Edge(A, B) -> 0.0),
        (Edge(A, C) -> 0.0),
        (Edge(C, D) -> 0.0),
        (Edge(D, E) -> 0.0),
        (Edge(E, F) -> 0.0),
        (Edge(F, G) -> 0.0),
        (Edge(C, F) -> 0.0),
        (Edge(D, G) -> 0.0)
      )

      val g = new Graph(edges)

      val newVert = Vertex("Z")

      g.bfs(A, F) should equal(true)
      g.bfs(A, newVert) should equal(false)
    }


    "perform shortest-path correctly" in {

      val A = Vertex("A")
      val B = Vertex("B")
      val C = Vertex("C")
      val D = Vertex("D")
      val E = Vertex("E")
      val F = Vertex("F")
      val G = Vertex("G")

      //Shortest path by steps is A->D, but shortest path by weight is A->C->D
      val edges = Map(
        (Edge(A, D) -> 3.0),
        (Edge(A, C) -> 1.0),
        (Edge(C, D) -> 1.0),
        (Edge(D, E) -> 1.0),
        (Edge(E, F) -> 1.0),
        (Edge(F, G) -> 1.0),
        (Edge(C, F) -> 1.0),
        (Edge(D, G) -> 1.0)
      )

      val g = new Graph(edges)

      g.shortestPath(A, D) should equal(2.0)
    }

  }

}
