package models

trait Node[A] {

  val content: A
  def neighbors: Vector[Node[A]]
  def isSolution: Boolean

}
