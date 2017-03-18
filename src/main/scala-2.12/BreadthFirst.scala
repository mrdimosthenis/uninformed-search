import models.IntTable
import models.direction._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object BreadthFirst {

  def nQueens(puzzleSize: Int): Vector[Int] = {

    def neighbors(node: Vector[Int]): Vector[Vector[Int]] = {
      val i = node.indexOf(-1)
      Range(0, puzzleSize).map(e => node.updated(i, e)).filter(v => {
        val subVec = v.slice(0, i + 1)
        subVec.distinct.length == subVec.length &&
          subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == subVec.length &&
          subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == subVec.length
      }).toVector
    }

    @tailrec
    def recur(queue: Queue[Vector[Int]]): Vector[Int] = {
      val currentNode = queue.dequeue._1
      if (!currentNode.contains(-1)) currentNode
      else recur(queue.dequeue._2.enqueue(neighbors(currentNode)))
    }

    recur(Queue(Vector.fill(puzzleSize)(-1)))

  }

  def sudoku(puzzleTable: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = {

    val sqrSize = Math.sqrt(puzzleTable.length).toInt

    def neighbors(node: IndexedSeq[IndexedSeq[Int]]): Vector[IndexedSeq[IndexedSeq[Int]]] = {
      val intTable = IntTable(node)
      val (i, j) = intTable.indexesOf(0)
      Range(1, puzzleTable.length + 1).map(e => intTable.updated(i, j, e)).filter(tbl => {
        def isUniqueFilled(vec: IndexedSeq[Int]) = vec.count(e => e != 0) == vec.filter(e => e != 0).distinct.length
        val iMod = i - (i % sqrSize)
        val jMod = j - (j % sqrSize)
        val flattenMiniSquare = tbl.slice(iMod, iMod + sqrSize).flatMap(v => v.slice(jMod, jMod + sqrSize))
        isUniqueFilled(tbl(i)) && isUniqueFilled(tbl.transpose.apply(j)) && isUniqueFilled(flattenMiniSquare)
      }).toVector
    }

    @tailrec
    def recur(queue: Queue[IndexedSeq[IndexedSeq[Int]]]): IndexedSeq[IndexedSeq[Int]] = {
      val currentNode = queue.dequeue._1
      if (!currentNode.exists(v => v.contains(0))) currentNode
      else recur(queue.dequeue._2.enqueue(neighbors(currentNode)))
    }

    recur(Queue(puzzleTable))

  }

  def tileSlide(puzzleTable: IndexedSeq[IndexedSeq[Int]]): Vector[Direction] = {

    case class Node(instance: IndexedSeq[IndexedSeq[Int]], path: Vector[Direction])

    val puzzleSize = puzzleTable.length

    def isSolution(node: Node): Boolean = IndexedSeq.tabulate(puzzleSize, puzzleSize)
      {(i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0 else puzzleSize * i + j + 1} == node.instance

    def neighbors(node: Node, trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Node] = {
      val intTable = IntTable(node.instance)
      val (i, j) = intTable.indexesOf(0)
      Vector(Tuple3(i + 1, j, Up), Tuple3(i, j + 1, Left), Tuple3(i - 1, j, Down), Tuple3(i, j - 1, Right))
        .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
        .map(t => Node(intTable.exchanged(i, j, t._1, t._2), node.path :+ t._3))
        .filter(n => !trackSet.contains(n.instance))
    }

    @tailrec
    def recur(queue: Queue[Node], trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Direction] = {
      val currentNode = queue.dequeue._1
      if (isSolution(currentNode)) currentNode.path
      else recur(queue.dequeue._2.enqueue(neighbors(currentNode, trackSet)), trackSet + currentNode.instance)
    }

    recur(Queue(Node(puzzleTable, Vector.empty[Direction])), Set.empty[IndexedSeq[IndexedSeq[Int]]])

  }

  def hanoi(numOfDisks: Int): Vector[(Int, Int)] = {

    case class Node(instance: IndexedSeq[List[Int]], path: Vector[(Int, Int)])

    val fullStack = Range(0, numOfDisks).toList
    val startingInstance = IndexedSeq(fullStack, List.empty[Int], List.empty[Int])
    val goalInstance = IndexedSeq(List.empty[Int], List.empty[Int], fullStack)

    def isSolution(node: Node): Boolean =
      node.instance == goalInstance

    def neighbors(node: Node, trackSet: Set[IndexedSeq[List[Int]]]): Vector[Node] = {
      (for (i <- Range(0,3); j <- Range(0,3)) yield (i, j)).filter(t => {
        val fromStack = node.instance(t._1)
        val toStack = node.instance(t._2)
        t._1 != t._2 && fromStack.nonEmpty && (toStack.isEmpty || toStack.head > fromStack.head)
      }).map(t => {
        val instance = node.instance
        val from = t._1
        val to = t._2
        Node(instance.updated(to, instance(from).head :: instance(to)).updated(from, instance(from).tail),
          node.path :+ (from, to))
      }).filter(n => !trackSet.contains(n.instance)).toVector
    }

    @tailrec
    def recur(queue: Queue[Node], trackSet: Set[IndexedSeq[List[Int]]]): Vector[(Int, Int)] = {
      val currentNode = queue.dequeue._1
      if (isSolution(currentNode)) currentNode.path
      else recur(queue.dequeue._2.enqueue(neighbors(currentNode, trackSet)), trackSet + currentNode.instance)
    }

    recur(Queue(Node(startingInstance, Vector.empty[(Int, Int)])), Set.empty[IndexedSeq[List[Int]]])

  }

}
