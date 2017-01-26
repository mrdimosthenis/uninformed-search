package puzzles

class Tile(puzzleTable: IndexedSeq[IndexedSeq[Int]]) extends Puzzle[IndexedSeq[IndexedSeq[Int]]] {

  /*trait Direction
  case object Right extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Up extends Direction*/

  private val tableSize = puzzleTable.length

  override val needTracking: Boolean = true

  override val rootNode: IndexedSeq[IndexedSeq[Int]] = puzzleTable

  override def isSolution(node: IndexedSeq[IndexedSeq[Int]]): Boolean =
    IndexedSeq.tabulate(3,3){(i,j) => if (i == 2 && j == 2) 0 else 3 * i + j + 1} == node

  override def neighbors(node: IndexedSeq[IndexedSeq[Int]]): Vector[IndexedSeq[IndexedSeq[Int]]] = {
    val i = node.zipWithIndex.find(t => t._1.contains(0)).get._2
    val j = node(i).zipWithIndex.find(t => t._1 == 0).get._2
    def exchange(i0: Int, j0: Int, i1: Int, j1: Int): IndexedSeq[IndexedSeq[Int]] = {
      val updatedByI = node.updated(i0, node(i0).updated(j0, node(i1)(j1)))
      updatedByI.updated(i1, updatedByI(i1).updated(j1, node(i0)(j0)))
    }
    Vector(Tuple2(i + 1, j), Tuple2(i, j + 1), Tuple2(i - 1, j), Tuple2(i, j - 1))
      .filter(t => t._1 >= 0 && t._1 < tableSize && t._2 >= 0 && t._2 < tableSize)
      .map(t => exchange(i, j, t._1, t._2))
  }

  override def pPrint(node: IndexedSeq[IndexedSeq[Int]]): Unit = node.foreach(v => println(v))

}
