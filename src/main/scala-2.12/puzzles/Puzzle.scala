package puzzles

trait Puzzle[Any] {

  val needTracking: Boolean
  val rootNode: Any
  def isSolution(node: Any): Boolean
  def neighbors(node: Any): Vector[Any]
  def pPrint(node: Any): Unit

}
