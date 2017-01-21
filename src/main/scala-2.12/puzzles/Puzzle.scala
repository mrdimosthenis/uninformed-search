package puzzles

trait Puzzle[A] {

  val rootNode: A
  def isSolution(node: A): Boolean
  def children(node: A): Vector[A]
  def worthGenerate(node: A): Boolean
  def pPrint(node: A): Unit

}
