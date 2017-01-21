package puzzles

class NQueens(puzzleSize: Int) extends Puzzle[Vector[Int]]{

  val rootNode = Vector.empty[Int]

  def isSolution(node: Vector[Int]): Boolean =
    node.length == puzzleSize &&
      node.length == node.distinct.length &&
      !node.dropRight(1).zip(node.drop(1)).exists(t => Math.abs(t._1 - t._2) == 1)

  def children(node: Vector[Int]): Vector[Vector[Int]] =
    (0 until puzzleSize).map(i => node :+ i).toVector

  def worthGenerate(node: Vector[Int]): Boolean = node.length <= puzzleSize

  def pPrint(node: Vector[Int]): Unit =
    node.indices.map(i => Vector.fill(puzzleSize)(0).updated(node(i), 1)).foreach(v => println(v))

}
