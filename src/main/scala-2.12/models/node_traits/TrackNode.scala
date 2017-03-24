package models.node_traits

trait TrackNode[A, B] {

  val content: A
  val path: Vector[B]
  def neighbors(trackSet: Set[A]): Vector[TrackNode[A, B]]
  def isSolution: Boolean

}
