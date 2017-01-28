package puzzles

trait Direction
case object Right extends Direction
case object Down extends Direction
case object Left extends Direction
case object Up extends Direction
case class TileNode(currentTable: IndexedSeq[IndexedSeq[Int]], moves: Vector[Direction]) extends Node{
  override val instance: Any = currentTable
}

class Tile(puzzleTable: IndexedSeq[IndexedSeq[Int]]) extends Puzzle[TileNode] {

  private val tableSize = puzzleTable.length

  override val needTracking: Boolean = true

  override val rootNode: TileNode = TileNode(puzzleTable, Vector.empty[Direction])

  override def isSolution(node: TileNode): Boolean =
    IndexedSeq.tabulate(3,3){(i,j) => if (i == 2 && j == 2) 0 else 3 * i + j + 1} == node.currentTable

  override def neighbors(node: TileNode): Vector[TileNode] = {
    val i = node.currentTable.zipWithIndex.find(t => t._1.contains(0)).get._2
    val j = node.currentTable(i).zipWithIndex.find(t => t._1 == 0).get._2
    def exchange(i0: Int, j0: Int, i1: Int, j1: Int): IndexedSeq[IndexedSeq[Int]] = {
      val updatedByI = node.currentTable.updated(i0, node.currentTable(i0).updated(j0, node.currentTable(i1)(j1)))
      updatedByI.updated(i1, updatedByI(i1).updated(j1, node.currentTable(i0)(j0)))
    }
    Vector(Tuple3(i + 1, j, Down), Tuple3(i, j + 1, Right), Tuple3(i - 1, j, Up), Tuple3(i, j - 1, Left))
      .filter(t => t._1 >= 0 && t._1 < tableSize && t._2 >= 0 && t._2 < tableSize)
      .map(t => TileNode(exchange(i, j, t._1, t._2), node.moves :+ t._3))
  }

  override def pPrint(node: TileNode): Unit = println(node)//node.currentTable.foreach(v => println(v))

}
