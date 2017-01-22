package algorithms

import puzzles.Puzzle

import scala.collection.immutable.Queue

object Search {

  def breadthFirst(puzzle: Puzzle[Any]): Any = {
    var queue = Queue(puzzle.rootNode)
    while (true){
      val currentNode = queue.dequeue._1
      if (puzzle.isSolution(currentNode)) return currentNode
      queue = queue.enqueue(puzzle.children(currentNode))
      queue = queue.dequeue._2
    }
  }

}
