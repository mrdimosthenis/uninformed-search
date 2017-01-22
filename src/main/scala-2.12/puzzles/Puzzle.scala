package puzzles

abstract class Puzzle[Any] {

  val rootNode: Any
  def isSolution(node: Any): Boolean
  def children(node: Any): Vector[Any]
  def worthGenerate(node: Any): Boolean

}
