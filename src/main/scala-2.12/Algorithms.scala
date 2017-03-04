import models.Node

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Algorithms {

  def breadthFirst(startingNode: Node): Option[Node] = {

    @tailrec
    def recur(queue: Queue[Node]): Option[Node] = {
      if (queue.isEmpty) None
      else {
        val currentNode = queue.dequeue._1
        if (currentNode.isSolution) Some(currentNode)
        else recur(queue.dequeue._2.enqueue(currentNode.neighbors))
      }
    }

    recur(Queue(startingNode))

  }

  def depthFirst(startingNode: Node): Option[Node] = {

    @tailrec
    def recur(stack: List[Node]): Option[Node] = {
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
