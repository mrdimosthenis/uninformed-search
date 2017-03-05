import models.{Node, TrackNode}

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Algorithms {

  def breadthFirst(startingNode: Node[Any]): Option[Node[Any]] = {

    @tailrec
    def recur(queue: Queue[Node[Any]]): Option[Node[Any]] = {
      if (queue.isEmpty) None
      else {
        val currentNode = queue.dequeue._1
        if (currentNode.isSolution) Some(currentNode)
        else recur(queue.dequeue._2.enqueue(currentNode.neighbors))
      }
    }

    recur(Queue(startingNode))

  }

  def depthFirst(startingNode: Node[Any]): Option[Node[Any]] = {

    @tailrec
    def recur(stack: List[Node[Any]]): Option[Node[Any]] = {
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

  def breadthFirstWithTracking(startingNode: TrackNode[Any, Any]): Option[Vector[Any]] = {

    @tailrec
    def recur(queue: Queue[TrackNode[Any, Any]], trackSet: Set[Any]): Option[Vector[Any]] = {
      if (queue.isEmpty) None
      else {
        val currentNode = queue.dequeue._1
        if (currentNode.isSolution) Some(currentNode.moves)
        else recur(queue.dequeue._2.enqueue(currentNode.neighbors(trackSet)), trackSet + currentNode.instance)
      }
    }

    recur(Queue(startingNode), Set.empty[Any])

  }

}
