import algorithms.Search
import puzzles.{NQueens, Puzzle}

object Main extends App{

  val puzzle = new NQueens(6)

  val solution = Search.breadthFirst(puzzle.asInstanceOf[Puzzle[Any]])

  puzzle.pPrint(solution.asInstanceOf[Vector[Int]])

}
