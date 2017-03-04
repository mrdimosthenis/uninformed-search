import models.{NQueensNode, SudokuNode}
import org.scalatest.FunSuite

class AlgorithmsTest extends FunSuite {

  test("sudoku") {

    assert(Algorithms.breadthFirst(SudokuNode(IndexedSeq(IndexedSeq(0)))).contains(SudokuNode(IndexedSeq(IndexedSeq(1)))))

    assert(Algorithms.breadthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4)))).contains(SudokuNode(IndexedSeq(
      IndexedSeq(1, 2, 4, 3),
      IndexedSeq(4, 3, 1, 2),
      IndexedSeq(3, 4, 2, 1),
      IndexedSeq(2, 1, 3, 4)))))

    assert(Algorithms.breadthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(9, 0, 7, 8, 0, 0, 0, 4, 0),
      IndexedSeq(0, 0, 0, 0, 3, 1, 0, 0, 0),
      IndexedSeq(0, 6, 0, 0, 0, 4, 5, 2, 0),
      IndexedSeq(0, 9, 0, 0, 0, 0, 8, 7, 3),
      IndexedSeq(0, 0, 2, 0, 0, 3, 0, 0, 0),
      IndexedSeq(4, 8, 0, 0, 5, 0, 6, 0, 0),
      IndexedSeq(1, 0, 0, 0, 0, 0, 0, 0, 5),
      IndexedSeq(0, 3, 5, 0, 6, 0, 0, 8, 0),
      IndexedSeq(0, 4, 0, 2, 7, 0, 0, 0, 9)))).contains(SudokuNode(IndexedSeq(
      IndexedSeq(9, 5, 7, 8, 2, 6, 3, 4, 1),
      IndexedSeq(8, 2, 4, 5, 3, 1, 7, 9, 6),
      IndexedSeq(3, 6, 1, 7, 9, 4, 5, 2, 8),
      IndexedSeq(5, 9, 6, 4, 1, 2, 8, 7, 3),
      IndexedSeq(7, 1, 2, 6, 8, 3, 9, 5, 4),
      IndexedSeq(4, 8, 3, 9, 5, 7, 6, 1, 2),
      IndexedSeq(1, 7, 9, 3, 4, 8, 2, 6, 5),
      IndexedSeq(2, 3, 5, 1, 6, 9, 4, 8, 7),
      IndexedSeq(6, 4, 8, 2, 7, 5, 1, 3, 9)))))

    assert(Algorithms.breadthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 1, 4)))).isEmpty)

    //

    assert(Algorithms.depthFirst(SudokuNode(IndexedSeq(IndexedSeq(0)))).contains(SudokuNode(IndexedSeq(IndexedSeq(1)))))

    assert(Algorithms.depthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4)))).contains(SudokuNode(IndexedSeq(
      IndexedSeq(1, 2, 4, 3),
      IndexedSeq(4, 3, 1, 2),
      IndexedSeq(3, 4, 2, 1),
      IndexedSeq(2, 1, 3, 4)))))

    assert(Algorithms.depthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(9, 0, 7, 8, 0, 0, 0, 4, 0),
      IndexedSeq(0, 0, 0, 0, 3, 1, 0, 0, 0),
      IndexedSeq(0, 6, 0, 0, 0, 4, 5, 2, 0),
      IndexedSeq(0, 9, 0, 0, 0, 0, 8, 7, 3),
      IndexedSeq(0, 0, 2, 0, 0, 3, 0, 0, 0),
      IndexedSeq(4, 8, 0, 0, 5, 0, 6, 0, 0),
      IndexedSeq(1, 0, 0, 0, 0, 0, 0, 0, 5),
      IndexedSeq(0, 3, 5, 0, 6, 0, 0, 8, 0),
      IndexedSeq(0, 4, 0, 2, 7, 0, 0, 0, 9)))).contains(SudokuNode(IndexedSeq(
      IndexedSeq(9, 5, 7, 8, 2, 6, 3, 4, 1),
      IndexedSeq(8, 2, 4, 5, 3, 1, 7, 9, 6),
      IndexedSeq(3, 6, 1, 7, 9, 4, 5, 2, 8),
      IndexedSeq(5, 9, 6, 4, 1, 2, 8, 7, 3),
      IndexedSeq(7, 1, 2, 6, 8, 3, 9, 5, 4),
      IndexedSeq(4, 8, 3, 9, 5, 7, 6, 1, 2),
      IndexedSeq(1, 7, 9, 3, 4, 8, 2, 6, 5),
      IndexedSeq(2, 3, 5, 1, 6, 9, 4, 8, 7),
      IndexedSeq(6, 4, 8, 2, 7, 5, 1, 3, 9)))))

    assert(Algorithms.depthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 1, 4)))).isEmpty)

  }

  test("NQueens") {

    assert(Algorithms.breadthFirst(NQueensNode(Vector(-1))).contains(NQueensNode(Vector(0))))

    assert(Algorithms.breadthFirst(NQueensNode(Vector.fill(4)(-1))).contains(NQueensNode(Vector(1, 3, 0, 2))))

    assert(Algorithms.breadthFirst(NQueensNode(Vector.fill(5)(-1))).contains(NQueensNode(Vector(0, 2, 4, 1, 3))))

    //

    assert(Algorithms.depthFirst(NQueensNode(Vector(-1))).contains(NQueensNode(Vector(0))))

    assert(Algorithms.depthFirst(NQueensNode(Vector.fill(4)(-1))).contains(NQueensNode(Vector(1, 3, 0, 2))))

    assert(Algorithms.depthFirst(NQueensNode(Vector.fill(5)(-1))).contains(NQueensNode(Vector(0, 2, 4, 1, 3))))

  }

}
