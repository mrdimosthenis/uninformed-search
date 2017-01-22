package puzzles

class Sudoku(puzzleTable: IndexedSeq[IndexedSeq[Int]]) extends Puzzle[IndexedSeq[IndexedSeq[Int]]] {

  private val tableSize = puzzleTable.length

  val rootNode: IndexedSeq[IndexedSeq[Int]] = puzzleTable

  def isSolution(table: IndexedSeq[IndexedSeq[Int]]): Boolean = !table.exists(v => v.contains(0))

  def children(table: IndexedSeq[IndexedSeq[Int]]): Vector[IndexedSeq[IndexedSeq[Int]]] = {
    val i = table.zipWithIndex.find(t => t._1.contains(0)).get._2
    val j = table(i).zipWithIndex.find(t => t._1 == 0).get._2
    Range(1, tableSize + 1).map(e => table.updated(i, table(i).updated(j, e))).filter(tbl => {
      val transposed = tbl.transpose
      val numOfNonZeroInRow = tbl(i).filter(e => e != 0)
      val numOfNonZeroInColumn = transposed(j).filter(e => e != 0)
      numOfNonZeroInRow.length == numOfNonZeroInRow.distinct.length &&
        numOfNonZeroInColumn.length == numOfNonZeroInColumn.distinct.length
    }).toVector
  }

  def pPrint(table: IndexedSeq[IndexedSeq[Int]]): Unit = table.foreach(v => println(v))

}
