package puzzles

class NQueens(puzzleSize: Int) extends Puzzle[Vector[Int]] {

  override val needTracking: Boolean = false

  override val rootNode: Vector[Int] = Vector.fill(puzzleSize)(-1)

  override def isSolution(node: Vector[Int]): Boolean = !node.contains(-1)

  override def neighbors(node: Vector[Int]): Vector[Vector[Int]] = {
    val i = node.zipWithIndex.find(t => t._1 == -1).get._2
    Range(0, puzzleSize).map(e => node.updated(i, e)).filter(v => {
      val subVec = v.slice(0, i + 1)
      subVec.distinct.length == i + 1 &&
        subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i + 1 &&
        subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i + 1
    }).toVector
  }

  override def pPrint(node: Vector[Int]): Unit =
    node.map(i => {
      if (i != -1) Vector.fill(puzzleSize)(0).updated(i, 1)
      else Vector.fill(puzzleSize)(0)
    }).foreach(v => println(v))

}
