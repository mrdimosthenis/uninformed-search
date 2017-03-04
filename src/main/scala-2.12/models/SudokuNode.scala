package models

case class SudokuNode(content: IndexedSeq[IndexedSeq[Int]]) extends Node {

  override def neighbors: Vector[Node] = {
    val intTable = IntTable(content)
    val (i, j) = intTable.indexesOf(0)
    if ((i, j) == (-1, -1)) Vector.empty[SudokuNode]
    else {
      val sqrSize = Math.sqrt(content.length).toInt
      Range(1, content.length + 1).map(e => intTable.updated(i, j, e)).filter(tbl => {
        def isUniqueFilled(vec: IndexedSeq[Int]) = vec.count(e => e != 0) == vec.filter(e => e != 0).distinct.length
        val iMod = i - (i % sqrSize)
        val jMod = j - (j % sqrSize)
        val flattenMiniSquare = tbl.slice(iMod, iMod + sqrSize).flatMap(v => v.slice(jMod, jMod + sqrSize))
        isUniqueFilled(tbl(i)) && isUniqueFilled(tbl.transpose.apply(j)) && isUniqueFilled(flattenMiniSquare)
      }).map(n => SudokuNode(n)).toVector
    }
  }

  override def isSolution: Boolean = !content.exists(v => v.contains(0))

}
