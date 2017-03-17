import models.{Node, TrackNode}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Algorithms {

  def breadthFirst[A](startingNode: Node[A]): Option[Node[A]] = {

    @tailrec
    def recur(queue: Queue[Node[A]]): Option[Node[A]] = {
      if (queue.isEmpty) None
      else {
        val currentNode = queue.dequeue._1
        if (currentNode.isSolution) Some(currentNode)
        else recur(queue.dequeue._2.enqueue(currentNode.neighbors))
      }
    }

    recur(Queue(startingNode))

  }

  def depthFirst[A](startingNode: Node[A]): Option[Node[A]] = {

    @tailrec
    def recur(stack: List[Node[A]]): Option[Node[A]] = {
      if (stack.isEmpty) None
      else {
        val currentNode = stack.head
        if (currentNode.isSolution) Some(currentNode)
        else recur(currentNode.neighbors.toList ++ stack.tail)
      }
    }

    recur(List(startingNode))

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
