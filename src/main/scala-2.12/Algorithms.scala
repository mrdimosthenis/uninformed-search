import models.{Node, TrackNode}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Algorithms {

  def search[A](startingNode: Node[A], isBreadthFirst: Boolean = true): Option[A] = {

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

  //

  def breadthFirstWithTracking[A, B](startingNode: TrackNode[A, B]): Option[Vector[B]] = {

    @tailrec
    def recur(queue: Queue[TrackNode[A, B]], trackSet: Set[A]): Option[Vector[B]] = {
      if (queue.isEmpty) None
      else {
        val currentNode = queue.dequeue._1
        if (currentNode.isSolution) Some(currentNode.path)
        else recur(queue.dequeue._2.enqueue(currentNode.neighbors(trackSet)), trackSet + currentNode.content)
      }
    }

    recur(Queue(startingNode), Set.empty[A])

  }

  def depthFirstWithTracking[A, B](startingNode: TrackNode[A, B]): Option[Vector[B]] = {

    @tailrec
    def recur(stack: List[TrackNode[A, B]], trackSet: Set[A]): Option[Vector[B]] = {
      if (stack.isEmpty) None
      else {
        val currentNode = stack.head
        if (currentNode.isSolution) Some(currentNode.path)
        else recur(currentNode.neighbors(trackSet).toList ++ stack.tail, trackSet + currentNode.content)
      }
    }

    recur(List(startingNode), Set.empty[A])

  }

}
