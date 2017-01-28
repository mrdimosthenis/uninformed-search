package algorithms

import puzzles.{Node, Puzzle}

import scala.collection.immutable.Queue

object Search {

  def breadthFirst(puzzle: Puzzle[Any]): Any = {
    var trackSet = Set.empty[Any]
    var queue = Queue(puzzle.rootNode)
    while (true){
      val currentNode = queue.dequeue._1
      if (puzzle.needTracking) trackSet = trackSet + currentNode.asInstanceOf[Node].instance
      //puzzle.pPrint(currentNode); println(); Thread.sleep(1000)
      if (puzzle.isSolution(currentNode)) return currentNode
      queue = queue.enqueue({
        val neighbors = puzzle.neighbors(currentNode)
        if (puzzle.needTracking) neighbors.filter(n => !trackSet.contains(n.asInstanceOf[Node].instance))
        else neighbors
      })
      queue = queue.dequeue._2
    }
  }

}
