package models

trait Node[A <: Any] {

  val content: A
  def neighbors: Vector[Node[A]]
  def isSolution: Boolean

}
