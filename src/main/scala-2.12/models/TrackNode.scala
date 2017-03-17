package models

trait TrackNode[A, B] {

  val instance: A
  val moves: Vector[B]
  def neighbors(trackSet: Set[A]): Vector[TrackNode[A, B]]
  def isSolution: Boolean

}