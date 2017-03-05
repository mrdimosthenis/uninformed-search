import models.Node

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

}
