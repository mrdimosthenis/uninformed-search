package search

import scala.collection.immutable.Queue

object BreadthFirst {

  def nQueens(puzzleSize: Int): Any = {
    var queue = Queue(Vector.fill(puzzleSize)(-1))
    while (true){
      val currentNode = queue.dequeue._1
      if (!currentNode.contains(-1)) return currentNode
      queue = queue.enqueue({
        val i = currentNode.zipWithIndex.find(t => t._1 == -1).get._2
        Range(0, puzzleSize).map(e => currentNode.updated(i, e)).filter(v => {
          val subVec = v.slice(0, i + 1)
          subVec.distinct.length == i + 1 &&
            subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i + 1 &&
            subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i + 1
        })
      })
      queue = queue.dequeue._2
    }
  }

  def sudoku(puzzleTable: IndexedSeq[IndexedSeq[Int]]): Any = {
    var queue = Queue(puzzleTable)
    while (true){
      val currentNode = queue.dequeue._1
      if (!currentNode.exists(v => v.contains(0))) return currentNode
      queue = queue.enqueue({
        val i = currentNode.zipWithIndex.find(t => t._1.contains(0)).get._2
        val j = currentNode(i).zipWithIndex.find(t => t._1 == 0).get._2
        Range(1, puzzleTable.length + 1).map(e => currentNode.updated(i, currentNode(i).updated(j, e))).filter(tbl => {
          val transposed = tbl.transpose
          val numOfNonZeroInRow = tbl(i).filter(e => e != 0)
          val numOfNonZeroInColumn = transposed(j).filter(e => e != 0)
          numOfNonZeroInRow.length == numOfNonZeroInRow.distinct.length &&
            numOfNonZeroInColumn.length == numOfNonZeroInColumn.distinct.length
        })
      })
      queue = queue.dequeue._2
    }
  }

  def tileSlide(puzzleTable: IndexedSeq[IndexedSeq[Int]]): Any = {
    import models.direction._
    case class Node(instance: IndexedSeq[IndexedSeq[Int]], moves: Vector[Direction])
    val puzzleSize = puzzleTable.length
    var trackSet = Set.empty[IndexedSeq[IndexedSeq[Int]]]
    var queue = Queue(Node(puzzleTable, Vector.empty[Direction]))
    while (true){
      val currentNode = queue.dequeue._1
      trackSet = trackSet + currentNode.instance
      if (IndexedSeq.tabulate(puzzleSize, puzzleSize){(i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0
      else puzzleSize * i + j + 1} == currentNode.instance) return currentNode.moves
      queue = queue.enqueue({
        val i = currentNode.instance.zipWithIndex.find(t => t._1.contains(0)).get._2
        val j = currentNode.instance(i).zipWithIndex.find(t => t._1 == 0).get._2
        def exchange(i0: Int, j0: Int, i1: Int, j1: Int): IndexedSeq[IndexedSeq[Int]] = {
          val updatedByI =
            currentNode.instance.updated(i0, currentNode.instance(i0).updated(j0, currentNode.instance(i1)(j1)))
          updatedByI.updated(i1, updatedByI(i1).updated(j1, currentNode.instance(i0)(j0)))
        }
        Vector(Tuple3(i + 1, j, Down), Tuple3(i, j + 1, Right), Tuple3(i - 1, j, Up), Tuple3(i, j - 1, Left))
          .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
          .map(t => Node(exchange(i, j, t._1, t._2), currentNode.moves :+ t._3))
          .filter(n => !trackSet.contains(n.instance))
      })
      queue = queue.dequeue._2
    }
  }

}
