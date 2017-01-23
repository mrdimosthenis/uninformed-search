import algorithms.Search
import org.scalatest.FunSuite
import puzzles.{NQueens, Puzzle, Sudoku}

class Combinations extends FunSuite {

  test("breadth first 1 queen") {
    assert(Search.breadthFirst({new NQueens(1)}.asInstanceOf[Puzzle[Any]]) == Vector(0))
  }

  test("breadth first 4 queens") {
    assert(Search.breadthFirst({new NQueens(4)}.asInstanceOf[Puzzle[Any]]) == Vector(1, 3, 0, 2))
  }

  test("breadth first 5 queens") {
    assert(Search.breadthFirst({new NQueens(5)}.asInstanceOf[Puzzle[Any]]) == Vector(0, 2, 4, 1, 3))
  }

  test("breadth first 1x1 sudoku") {
    assert(Search.breadthFirst({new Sudoku(IndexedSeq(IndexedSeq(0)))}.asInstanceOf[Puzzle[Any]]) ==
      IndexedSeq(IndexedSeq(1)))
  }

  test("breadth first 4x4 sudoku") {
    assert(Search.breadthFirst({new Sudoku(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4)))}.asInstanceOf[Puzzle[Any]]) == IndexedSeq(
      IndexedSeq(1, 2, 4, 3),
      IndexedSeq(4, 3, 1, 2),
      IndexedSeq(3, 4, 2, 1),
      IndexedSeq(2, 1, 3, 4)))
  }

  test("breadth first 5x5 sudoku") {
    assert(Search.breadthFirst({new Sudoku(IndexedSeq(
      IndexedSeq(0, 2, 0, 1, 0),
      IndexedSeq(0, 0, 0, 0, 0),
      IndexedSeq(4, 0, 1, 0, 5),
      IndexedSeq(0, 0, 0, 0, 0),
      IndexedSeq(0, 0, 0, 0, 0)))}.asInstanceOf[Puzzle[Any]]) == IndexedSeq(
      IndexedSeq(3, 2, 5, 1, 4),
      IndexedSeq(1, 4, 2, 5, 3),
      IndexedSeq(4, 3, 1, 2, 5),
      IndexedSeq(2, 5, 3, 4, 1),
      IndexedSeq(5, 1, 4, 3, 2)))
  }

}
