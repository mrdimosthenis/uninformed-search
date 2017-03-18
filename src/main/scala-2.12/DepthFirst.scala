import models.IntTable
import models.direction._

import scala.annotation.tailrec

object DepthFirst {

  def nQueens(puzzleSize: Int): Vector[Int] = {

    def neighbors(node: Vector[Int]): Vector[Vector[Int]] = {
      val i = node.indexOf(-1)
      if (i < 0) Vector.empty[Vector[Int]]
      else {
        Range(0, puzzleSize).map(e => node.updated(i, e)).filter(v => {
          val subVec = v.slice(0, i + 1)
          subVec.distinct.length == subVec.length &&
            subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == subVec.length &&
            subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == subVec.length
        }).toVector
      }
    }

    @tailrec
    def recur(stack: List[Vector[Int]]): Vector[Int] = {
      val currentNode = stack.head
      if (!currentNode.contains(-1)) currentNode
      else recur(neighbors(currentNode).toList ++ stack.tail)
    }

    recur(List(Vector.fill(puzzleSize)(-1)))

  }

  def sudoku(puzzleTable: IndexedSeq[IndexedSeq[Int]]): IndexedSeq[IndexedSeq[Int]] = {

    val sqrSize = Math.sqrt(puzzleTable.length).toInt

    def neighbors(node: IndexedSeq[IndexedSeq[Int]]): Vector[IndexedSeq[IndexedSeq[Int]]] = {
      val intTable = IntTable(node)
      val (i, j) = intTable.indexesOf(0)
      if (i < 0) Vector.empty[IndexedSeq[IndexedSeq[Int]]]
      else {
        Range(1, puzzleTable.length + 1).map(e => intTable.updated(i, j, e)).filter(tbl => {
          def isUniqueFilled(vec: IndexedSeq[Int]) = vec.count(e => e != 0) == vec.filter(e => e != 0).distinct.length
          val iMod = i - (i % sqrSize)
          val jMod = j - (j % sqrSize)
          val flattenMiniSquare = tbl.slice(iMod, iMod + sqrSize).flatMap(v => v.slice(jMod, jMod + sqrSize))
          isUniqueFilled(tbl(i)) && isUniqueFilled(tbl.transpose.apply(j)) && isUniqueFilled(flattenMiniSquare)
        }).toVector
      }
    }

    @tailrec
    def recur(stack: List[IndexedSeq[IndexedSeq[Int]]]): IndexedSeq[IndexedSeq[Int]] = {
      val currentNode = stack.head
      if (!currentNode.exists(v => v.contains(0))) currentNode
      else recur(neighbors(currentNode).toList ++ stack.tail)
    }

    recur(List(puzzleTable))

  }

  def tileSlide(puzzleTable: IndexedSeq[IndexedSeq[Int]]): Vector[Direction] = {

    case class Node(content: IndexedSeq[IndexedSeq[Int]], path: Vector[Direction])

    val puzzleSize = puzzleTable.length

    def isSolution(node: Node): Boolean = IndexedSeq.tabulate(puzzleSize, puzzleSize)
      {(i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0 else puzzleSize * i + j + 1} == node.content

    def neighbors(node: Node, trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Node] = {
      val intTable = IntTable(node.content)
      val (i, j) = intTable.indexesOf(0)
      Vector(Tuple3(i + 1, j, Up), Tuple3(i, j + 1, Left), Tuple3(i - 1, j, Down), Tuple3(i, j - 1, Right))
        .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
        .map(t => Node(intTable.exchanged(i, j, t._1, t._2), node.path :+ t._3))
        .filter(n => !trackSet.contains(n.content))
    }

    @tailrec
    def recur(stack: List[Node], trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[Direction] = {
      val currentNode = stack.head
      if (isSolution(currentNode)) currentNode.path
      else recur(neighbors(currentNode, trackSet).toList ++ stack.tail, trackSet + currentNode.content)
    }

    recur(List(Node(puzzleTable, Vector.empty[Direction])), Set(puzzleTable))

  }

  def hanoi(numOfDisks: Int): Vector[(Int, Int)] = {

    case class Node(content: IndexedSeq[List[Int]], path: Vector[(Int, Int)])

    val fullStack = Range(0, numOfDisks).toList
    val startingContent = IndexedSeq(fullStack, List.empty[Int], List.empty[Int])
    val goalContent = IndexedSeq(List.empty[Int], List.empty[Int], fullStack)

    def isSolution(node: Node): Boolean =
      node.content == goalContent

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

    @tailrec
    def recur(stack: List[Node], trackSet: Set[IndexedSeq[List[Int]]]): Vector[(Int, Int)] = {
      val currentNode = stack.head
      if (isSolution(currentNode)) currentNode.path
      else recur(neighbors(currentNode, trackSet).toList ++ stack.tail, trackSet + currentNode.content)
    }

    recur(List(Node(startingContent, Vector.empty[(Int, Int)])), Set.empty[IndexedSeq[List[Int]]])

  }

}
