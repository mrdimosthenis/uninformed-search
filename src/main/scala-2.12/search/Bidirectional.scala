package search

import models.data_structure.IntTable
import models.direction._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Bidirectional {

  def tileSlide(puzzleTable: IndexedSeq[IndexedSeq[Int]]): Vector[Direction] = {

    case class Node(instance: IndexedSeq[IndexedSeq[Int]], moves: Vector[Direction])

    val puzzleSize = puzzleTable.length

    val target = IndexedSeq.tabulate(puzzleSize, puzzleSize)
    { (i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0 else puzzleSize * i + j + 1 }

    def neighbors(node: Node, trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Node] = {
      val intTable = IntTable(node.instance)
      val (i, j) = intTable.indexesOf(0)
      Vector(Tuple3(i + 1, j, Up), Tuple3(i, j + 1, Left), Tuple3(i - 1, j, Down), Tuple3(i, j - 1, Right))
        .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
        .map(t => Node(intTable.exchanged(i, j, t._1, t._2), node.moves :+ t._3))
        .filter(n => !trackSet.contains(n.instance))
    }

    case class ForwardSearch(queue: Queue[Node], trackSet: Set[IndexedSeq[IndexedSeq[Int]]]) {
      def nextInstance(): ForwardSearch = {
        val currentNode = queue.dequeue._1
        ForwardSearch(queue.dequeue._2.enqueue(neighbors(currentNode, trackSet)), trackSet + currentNode.instance)
      }
    }

    case class BackwardSearch(queue: Queue[Node], trackMap: Map[IndexedSeq[IndexedSeq[Int]], Vector[Direction]]) {
      def nextInstance(): BackwardSearch = {
        val currentNode = queue.dequeue._1
        BackwardSearch(queue.dequeue._2.enqueue(neighbors(currentNode, trackMap.keySet)),
          trackMap + (currentNode.instance -> currentNode.moves))
      }
    }

    @tailrec
    def recur(forwardSearch: ForwardSearch, backwardSearch: BackwardSearch): Vector[Direction] = {
      val currentForwardNode = forwardSearch.queue.dequeue._1
      val solution = backwardSearch.trackMap.find(kv => kv._1 == currentForwardNode.instance)
      if (solution.isDefined) currentForwardNode.moves ++ solution.get._2.reverse.map(m => m.opposite)
      else recur(forwardSearch.nextInstance(), backwardSearch.nextInstance())
    }

    recur(ForwardSearch(Queue(Node(puzzleTable, Vector.empty[Direction])), Set(puzzleTable)),
      BackwardSearch(Queue(Node(target, Vector.empty[Direction])), Map(target -> Vector.empty[Direction])))

  }

}
