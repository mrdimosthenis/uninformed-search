package models

trait Node {

  def neighbors: Vector[Node]
  def isSolution: Boolean

}
