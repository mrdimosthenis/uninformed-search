package search

import models.data_structure.IntTable
import models.direction._

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object BreadthFirst {

  def nQueens(puzzleSize: Int): Vector[Int] = {

    def neighbors(node: Vector[Int]): Vector[Vector[Int]] = {
      val i = node.indexOf(-1)
      Range(0, puzzleSize).map(e => node.updated(i, e)).filter(v => {
        val subVec = v.slice(0, i + 1)
        subVec.distinct.length == i + 1 &&
          subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i + 1 &&
          subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i + 1
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

    def neighbors(node: IndexedSeq[IndexedSeq[Int]]): Vector[IndexedSeq[IndexedSeq[Int]]] = {
      val intTable = IntTable(node)
      val (i, j) = intTable.indexesOf(0)
      Range(1, puzzleTable.length + 1).map(e => intTable.updated(i, j, e)).filter(tbl => {
        val transposed = tbl.transpose
        val numOfNonZeroInRow = tbl(i).filter(e => e != 0)
        val numOfNonZeroInColumn = transposed(j).filter(e => e != 0)
        numOfNonZeroInRow.length == numOfNonZeroInRow.distinct.length &&
          numOfNonZeroInColumn.length == numOfNonZeroInColumn.distinct.length
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

    case class Node(instance: IndexedSeq[IndexedSeq[Int]], moves: Vector[Direction])

    val puzzleSize = puzzleTable.length

    def isSolution(node: Node): Boolean = {
      IndexedSeq.tabulate(puzzleSize, puzzleSize)
      {(i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0 else puzzleSize * i + j + 1} == node.instance
    }

    def neighbors(node: Node, trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Node] = {
      val intTable = IntTable(node.instance)
      val (i, j) = intTable.indexesOf(0)
      Vector(Tuple3(i + 1, j, Up), Tuple3(i, j + 1, Left), Tuple3(i - 1, j, Down), Tuple3(i, j - 1, Right))
        .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
        .map(t => Node(intTable.exchanged(i, j, t._1, t._2), node.moves :+ t._3))
        .filter(n => !trackSet.contains(n.instance))
    }

    @tailrec
    def recur(queue: Queue[Node], trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Direction] = {
      val currentNode = queue.dequeue._1
      if (isSolution(currentNode)) currentNode.moves
      else recur(queue.dequeue._2.enqueue(neighbors(currentNode, trackSet)), trackSet + currentNode.instance)
    }

    recur(Queue(Node(puzzleTable, Vector.empty[Direction])), Set.empty[IndexedSeq[IndexedSeq[Int]]])

  }

}
