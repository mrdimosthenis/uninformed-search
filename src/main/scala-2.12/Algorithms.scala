import models.{Node, TrackNode}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Algorithms {

  def search[A](startingNode: Node[A], isBreadthFirst: Boolean = true): Option[Node[A]] = {

    @tailrec
    def recur(coll: Seq[Node[A]]): Option[Node[A]] = {
      if (coll.isEmpty) None
      else {
        val currentNode = coll match {
          case x: List[Node[A]] => coll.head
          case y: Vector[Node[A]] => coll.last
        }
        if (currentNode.isSolution) Some(currentNode)
        else {
          val restNodes = coll match {
            case x: List[Node[A]] => coll.drop(1)
            case y: Vector[Node[A]] => coll.dropRight(1)
          }
          recur(restNodes ++ currentNode.neighbors)
        }
      }
    }

    val searchCollection = if (isBreadthFirst) List(startingNode) else Vector(startingNode)
    recur(searchCollection)

  }

  //

  def breadthFirstWithTracking[A, B](startingNode: TrackNode[A, B]): Option[Vector[B]] = {

    @tailrec
    def recur(queue: Queue[TrackNode[A, B]], trackSet: Set[A]): Option[Vector[B]] = {
      if (queue.isEmpty) None
      else {
        val currentNode = queue.dequeue._1
        if (currentNode.isSolution) Some(currentNode.moves)
        else recur(queue.dequeue._2.enqueue(currentNode.neighbors(trackSet)), trackSet + currentNode.instance)
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
        if (currentNode.isSolution) Some(currentNode.moves)
        else recur(currentNode.neighbors(trackSet).toList ++ stack.tail, trackSet + currentNode.instance)
      }
    }

    recur(List(startingNode), Set.empty[A])

  }

}
