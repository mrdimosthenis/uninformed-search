package puzzles

class NQueens(puzzleSize: Int) extends Puzzle[Vector[Int]] {

  val rootNode = Vector.empty[Int]

  def isSolution(node: Vector[Int]): Boolean = node.distinct.length == puzzleSize &&
    node.zipWithIndex.map(t => t._1 - t._2).distinct.length == puzzleSize &&
    node.reverse.zipWithIndex.map(t => t._1 - t._2).distinct.length == puzzleSize

  def children(node: Vector[Int]): Vector[Vector[Int]] =
    Range(0, puzzleSize).map(i => node :+ i).toVector

  def pPrint(node: Vector[Int]): Unit =
    node.indices.map(i => Vector.fill(puzzleSize)(0).updated(node(i), 1)).foreach(v => println(v))

}
