import models.node_traits.{Node, TrackNode}

import scala.annotation.tailrec

object Search {

  def simple[A](startingNode: Node[A], isBreadthFirst: Boolean = true): Option[A] = {

    @tailrec
    def recur(seq: Seq[Node[A]]): Option[A] = {
      if (seq.isEmpty) None
      else {
        val currentNode = seq match {
          case _: Vector[Node[A]] => seq.head
          case _: List[Node[A]] => seq.last
        }
        if (currentNode.isSolution) Some(currentNode.content)
        else {
          val restNodes = seq match {
            case _: Vector[Node[A]] => seq.drop(1)
            case _: List[Node[A]] => seq.dropRight(1)
          }
          recur(restNodes ++ currentNode.neighbors)
        }
      }
    }

    val startingSeq = if (isBreadthFirst) Vector(startingNode) else List(startingNode)
    recur(startingSeq)

  }

  def withTracking[A, B](startingNode: TrackNode[A, B], isBreadthFirst: Boolean = true): Option[Vector[B]] = {

    @tailrec
    def recur(seq: Seq[TrackNode[A, B]], trackSet: Set[A]): Option[Vector[B]] = {
      if (seq.isEmpty) None
      else {
        val currentNode = seq match {
          case _: Vector[TrackNode[A, B]] => seq.head
          case _: List[TrackNode[A, B]] => seq.last
        }
        if (currentNode.isSolution) Some(currentNode.path)
        else {
          val restNodes = seq match {
            case _: Vector[TrackNode[A, B]] => seq.drop(1)
            case _: List[TrackNode[A, B]] => seq.dropRight(1)
          }
          recur(restNodes ++ currentNode.neighbors(trackSet), trackSet + currentNode.content)
        }
      }
    }

    val startingSeq = if (isBreadthFirst) Vector(startingNode) else List(startingNode)
    recur(startingSeq, Set.empty[A])

  }

}
