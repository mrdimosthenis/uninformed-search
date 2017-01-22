package puzzles

class Sudoku(puzzleTable: IndexedSeq[IndexedSeq[Int]]) extends Puzzle[IndexedSeq[IndexedSeq[Int]]] {

  private val tableSize = puzzleTable.length

  val rootNode: IndexedSeq[IndexedSeq[Int]] = puzzleTable

  def isSolution(table: IndexedSeq[IndexedSeq[Int]]): Boolean =
    table.count(v => v.filter(e => e != 0).distinct.length == tableSize) == tableSize &&
      table.transpose.count(v => v.filter(e => e != 0).distinct.length == tableSize) == tableSize

  def children(table: IndexedSeq[IndexedSeq[Int]]): Vector[IndexedSeq[IndexedSeq[Int]]] = {
    val i = table.zipWithIndex.find(t => t._1.contains(0)).get._2
    val j = table(i).zipWithIndex.find(t => t._1 == 0).get._2
    val transposed = table.transpose
    val sameRowOrColumnNumbers = table(i).filter(e => e != 0).toSet.union(transposed(j).filter(e => e != 0).toSet)
    Range(1, tableSize + 1).toSet.diff(sameRowOrColumnNumbers).map(e => table.updated(i, table(i).updated(j, e))).toVector
  }

  def pPrint(table: IndexedSeq[IndexedSeq[Int]]): Unit = table.foreach(v => println(v))

}
