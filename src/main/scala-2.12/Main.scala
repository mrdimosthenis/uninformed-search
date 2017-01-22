import algorithms.Search
import puzzles.{NQueens, Puzzle, Sudoku}

object Main extends App{

  /*val puzzle = new NQueens(6)
  val solution = Search.breadthFirst(puzzle.asInstanceOf[Puzzle[Any]])
  puzzle.pPrint(solution.asInstanceOf[Vector[Int]])*/

  val puzzle = new Sudoku(IndexedSeq(
    IndexedSeq(1, 0, 0, 0),
    IndexedSeq(0, 3, 0, 0),
    IndexedSeq(0, 4, 2, 0),
    IndexedSeq(0, 0, 0, 4)))
  val solution = Search.breadthFirst(puzzle.asInstanceOf[Puzzle[Any]])
  puzzle.pPrint(solution.asInstanceOf[IndexedSeq[IndexedSeq[Int]]])

}
