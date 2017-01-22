package puzzles

trait Puzzle[Any] {

  val rootNode: Any
  def isSolution(node: Any): Boolean
  def children(node: Any): Vector[Any]
  def pPrint(node: Any): Unit

}
