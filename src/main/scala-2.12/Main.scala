import algorithms.Search
import puzzles._

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
    IndexedSeq(8, 2, 3),
    IndexedSeq(6, 4, 7),
    IndexedSeq(5, 0, 1)))
  val solution2 = Search.breadthFirst(puzzle2.asInstanceOf[Puzzle[Any]])
  println(solution2.asInstanceOf[TileNode].moves)

}
