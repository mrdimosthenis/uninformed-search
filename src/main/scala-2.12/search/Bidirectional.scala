package search

import models.data_structure.IntTable
import models.direction._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Bidirectional {

  def tileSlide(puzzleTable: IndexedSeq[IndexedSeq[Int]]): Vector[Direction] = {

    val puzzleSize = puzzleTable.length

    val target = IndexedSeq.tabulate(puzzleSize, puzzleSize)
    {(i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0 else puzzleSize * i + j + 1}

    case class Node(instance: IndexedSeq[IndexedSeq[Int]], moves: Vector[Direction])

    case class SearchState(queue: Queue[Node], trackSet: Set[IndexedSeq[IndexedSeq[Int]]]) {

      def neighbors(node: Node, trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Node] = {
        val intTable = IntTable(node.instance)
        val (i, j) = intTable.indexesOf(0)
        Vector(Tuple3(i + 1, j, Up), Tuple3(i, j + 1, Left), Tuple3(i - 1, j, Down), Tuple3(i, j - 1, Right))
          .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
          .map(t => Node(intTable.exchanged(i, j, t._1, t._2), node.moves :+ t._3))
          .filter(n => !trackSet.contains(n.instance))
      }

      def nextInstance(): SearchState = {
        val currentNode = queue.dequeue._1
        SearchState(queue.dequeue._2.enqueue(neighbors(currentNode, trackSet)), trackSet + currentNode.instance)
      }

    }

    @tailrec
    def recur(forwardSearch: SearchState, backwardSearch: SearchState): Vector[Direction] = {
      /*println(forwardSearch.queue.dequeue._1.instance.foreach(println(_)))
      println()
      println(backwardSearch.queue.dequeue._1.instance.foreach(println(_)))
      println()
      println()
      Thread.sleep(2000)*/
      val solution = (for (n1 <- forwardSearch.queue.toList; n2 <- backwardSearch.queue.toList) yield (n1, n2))
        .find(t => t._1.instance == t._2.instance)
      if (solution.isDefined) solution.get._1.moves ++ solution.get._2.moves.reverse.map(m => m.opposite)
      else recur(forwardSearch.nextInstance(), backwardSearch.nextInstance())
    }

    recur(SearchState(Queue(Node(puzzleTable, Vector.empty[Direction])), Set.empty[IndexedSeq[IndexedSeq[Int]]]),
      SearchState(Queue(Node(target, Vector.empty[Direction])), Set.empty[IndexedSeq[IndexedSeq[Int]]]))

  }

}
