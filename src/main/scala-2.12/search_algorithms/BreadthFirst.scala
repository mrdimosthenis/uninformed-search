package search_algorithms

import puzzles.Puzzle

import scala.collection.immutable.Queue

class BreadthFirst(puzzle: Puzzle[Any]){

  val solutions: Vector[Any] = {
    var result = Vector.empty[Any]
    var queue = Queue(puzzle.rootNode)
    while (puzzle.worthGenerate(queue.dequeue._1)){
      val currentNode = queue.dequeue._1
      if (puzzle.isSolution(currentNode)) result = result :+ currentNode
      queue = queue.enqueue(puzzle.children(currentNode))
      queue = queue.dequeue._2
    }
    result
  }

}
