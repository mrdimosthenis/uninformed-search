package puzzles

class NQueens(puzzleSize: Int) extends Puzzle[Vector[Int]] {

  val rootNode: Vector[Int] = Vector.fill(puzzleSize)(-1)

  def isSolution(node: Vector[Int]): Boolean = !node.contains(-1)

  def children(node: Vector[Int]): Vector[Vector[Int]] = {
    val i = node.zipWithIndex.find(t => t._1 == -1).get._2
    Range(0, puzzleSize).map(e => node.updated(i, e)).filter(v => {
      val subVec = v.slice(0, i)
      subVec.distinct.length == i &&
        subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i &&
        subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == i
    }).toVector
  }

  def pPrint(node: Vector[Int]): Unit =
    node.map(i => {
      if (i != -1) Vector.fill(puzzleSize)(0).updated(i, 1)
      else Vector.fill(puzzleSize)(0)
    }).foreach(v => println(v))

}
