import algorithms.Search
import puzzles.{NQueens, Puzzle, Sudoku, Tile}

object Main extends App{

  /*val puzzle = new NQueens(5)
  val solution = Search.breadthFirst(puzzle.asInstanceOf[Puzzle[Any]])
  puzzle.pPrint(solution.asInstanceOf[Vector[Int]])
  println()
  println(solution)*/

  /*val puzzle1 = new Sudoku(IndexedSeq(
    IndexedSeq(0, 2, 0, 1, 0),
    IndexedSeq(0, 0, 0, 0, 0),
    IndexedSeq(4, 0, 1, 0, 5),
    IndexedSeq(0, 0, 0, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0)))
  val solution1 = Search.breadthFirst(puzzle1.asInstanceOf[Puzzle[Any]])
  puzzle1.pPrint(solution1.asInstanceOf[IndexedSeq[IndexedSeq[Int]]])*/

  val puzzle2 = new Tile(IndexedSeq(
    IndexedSeq(4, 6, 3),
    IndexedSeq(2, 5, 0),
    IndexedSeq(1, 8, 7)))
  val solution2 = Search.breadthFirst(puzzle2.asInstanceOf[Puzzle[Any]])
  puzzle2.pPrint(solution2.asInstanceOf[IndexedSeq[IndexedSeq[Int]]])

}
