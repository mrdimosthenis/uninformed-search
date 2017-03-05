package models

trait Node {

  val content: Any
  def neighbors: Vector[Node]
  def isSolution: Boolean

}
