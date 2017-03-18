import models.IntTable
import models.direction._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Bidirectional {

  def tileSlide(puzzleTable: IndexedSeq[IndexedSeq[Int]]): Vector[Direction] = {

    case class Node(content: IndexedSeq[IndexedSeq[Int]], path: Vector[Direction])

    val puzzleSize = puzzleTable.length

    val target = IndexedSeq.tabulate(puzzleSize, puzzleSize)
    { (i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0 else puzzleSize * i + j + 1 }

    def neighbors(node: Node, trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Node] = {
      val intTable = IntTable(node.content)
      val (i, j) = intTable.indexesOf(0)
      Vector(Tuple3(i + 1, j, Up), Tuple3(i, j + 1, Left), Tuple3(i - 1, j, Down), Tuple3(i, j - 1, Right))
        .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
        .map(t => Node(intTable.exchanged(i, j, t._1, t._2), node.path :+ t._3))
        .filter(n => !trackSet.contains(n.content))
    }

    case class Search(queue: Queue[Node], trackMap: Map[IndexedSeq[IndexedSeq[Int]], Vector[Direction]]) {
      val currentNode: Node = queue.dequeue._1
      def trackMatch(node: Node): Option[(IndexedSeq[IndexedSeq[Int]], Vector[Direction])] = {
        trackMap.find(kv => kv._1 == node.content)
      }
      def nextContent(): Search = {
        val currentNode = queue.dequeue._1
        Search(queue.dequeue._2.enqueue(neighbors(currentNode, trackMap.keySet)),
          trackMap + (currentNode.content -> currentNode.path))
      }
    }

    @tailrec
    def recur(forwardSearch: Search, backwardSearch: Search): Vector[Direction] = {
      val forwardMeet = backwardSearch.trackMatch(forwardSearch.currentNode)
      val backwardMeet = forwardSearch.trackMatch(backwardSearch.currentNode)
      if (forwardMeet.isDefined)
        forwardSearch.currentNode.path ++ forwardMeet.get._2.reverse.map(m => m.opposite)
      else if (backwardMeet.isDefined)
        backwardMeet.get._2 ++ backwardSearch.currentNode.path.reverse.map(m => m.opposite)
      else
        recur(forwardSearch.nextContent(), backwardSearch.nextContent())
    }

    recur(Search(Queue(Node(puzzleTable, Vector.empty[Direction])), Map(puzzleTable -> Vector.empty[Direction])),
        Search(Queue(Node(target, Vector.empty[Direction])), Map(target -> Vector.empty[Direction])))

  }

  def hanoi(numOfDisks: Int): Vector[(Int, Int)] = {

    case class Node(content: IndexedSeq[List[Int]], path: Vector[(Int, Int)])

    val fullStack = Range(0, numOfDisks).toList
    val startingContent = IndexedSeq(fullStack, List.empty[Int], List.empty[Int])
    val goalContent = IndexedSeq(List.empty[Int], List.empty[Int], fullStack)

    def neighbors(node: Node, trackSet: Set[IndexedSeq[List[Int]]]): Vector[Node] = {
      (for (i <- Range(0,3); j <- Range(0,3)) yield (i, j)).filter(t => {
        val fromStack = node.content(t._1)
        val toStack = node.content(t._2)
        t._1 != t._2 && fromStack.nonEmpty && (toStack.isEmpty || toStack.head > fromStack.head)
      }).map(t => {
        val content = node.content
        val from = t._1
        val to = t._2
        Node(content.updated(to, content(from).head :: content(to)).updated(from, content(from).tail),
          node.path :+ (from, to))
      }).filter(n => !trackSet.contains(n.content)).toVector
    }

    case class Search(queue: Queue[Node], trackMap: Map[IndexedSeq[List[Int]], Vector[(Int, Int)]]) {
      val currentNode: Node = queue.dequeue._1
      def trackMatch(node: Node): Option[(IndexedSeq[List[Int]], Vector[(Int, Int)])] = {
        trackMap.find(kv => kv._1 == node.content)
      }
      def nextContent(): Search = {
        val currentNode = queue.dequeue._1
        Search(queue.dequeue._2.enqueue(neighbors(currentNode, trackMap.keySet)),
          trackMap + (currentNode.content -> currentNode.path))
      }
    }

    @tailrec
    def recur(forwardSearch: Search, backwardSearch: Search): Vector[(Int, Int)] = {
      val forwardMeet = backwardSearch.trackMatch(forwardSearch.currentNode)
      val backwardMeet = forwardSearch.trackMatch(backwardSearch.currentNode)
      if (forwardMeet.isDefined)
        forwardSearch.currentNode.path ++ forwardMeet.get._2.reverse.map(m => (m._2, m._1))
      else if (backwardMeet.isDefined)
        backwardMeet.get._2 ++ backwardSearch.currentNode.path.reverse.map(m => (m._2, m._1))
      else
        recur(forwardSearch.nextContent(), backwardSearch.nextContent())
    }

    recur(
      Search(Queue(Node(startingContent, Vector.empty[(Int, Int)])), Map(startingContent -> Vector.empty[(Int, Int)])),
      Search(Queue(Node(goalContent, Vector.empty[(Int, Int)])), Map(goalContent -> Vector.empty[(Int, Int)])))

  }

}
