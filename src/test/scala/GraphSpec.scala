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

      val edges = List(
        Edge(A, B),
        Edge(A, C),
        Edge(C, D),
        Edge(D, E),
        Edge(E, F),
        Edge(F, G),
        Edge(C, F),
        Edge(D, G)
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

      val edges = List(
        Edge(A, B),
        Edge(A, C),
        Edge(C, D),
        Edge(D, E),
        Edge(E, F),
        Edge(F, G),
        Edge(C, F),
        Edge(D, G)
      )

      val g = new Graph(edges)

      val newVert = Vertex("Z")

      g.dfs(A, F) should equal(true)
      g.dfs(A, newVert) should equal(false)

    }

  }

}
