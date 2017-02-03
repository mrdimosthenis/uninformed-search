package search

import org.scalatest.FunSuite

class BreadthFirstTest extends FunSuite {

  test("nQueens") {
    assert(BreadthFirst.nQueens(1) == Vector(0))
    assert(BreadthFirst.nQueens(4) == Vector(1, 3, 0, 2))
    assert(BreadthFirst.nQueens(5) == Vector(0, 2, 4, 1, 3))
  }

  test("sudoku") {

    assert(BreadthFirst.sudoku(IndexedSeq(IndexedSeq(0))) == IndexedSeq(IndexedSeq(1)))

    assert(BreadthFirst.sudoku(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4))) == IndexedSeq(
      IndexedSeq(1, 2, 4, 3),
      IndexedSeq(4, 3, 1, 2),
      IndexedSeq(3, 4, 2, 1),
      IndexedSeq(2, 1, 3, 4)))

    assert(BreadthFirst.sudoku(IndexedSeq(
      IndexedSeq(0, 2, 0, 1, 0),
      IndexedSeq(0, 0, 0, 0, 0),
      IndexedSeq(4, 0, 1, 0, 5),
      IndexedSeq(0, 0, 0, 0, 0),
      IndexedSeq(0, 0, 0, 0, 0))) == IndexedSeq(
      IndexedSeq(3, 2, 5, 1, 4),
      IndexedSeq(1, 4, 2, 5, 3),
      IndexedSeq(4, 3, 1, 2, 5),
      IndexedSeq(2, 5, 3, 4, 1),
      IndexedSeq(5, 1, 4, 3, 2)))

  }

  test("tileSlide") {

    import models.direction._

    assert(BreadthFirst.tileSlide(IndexedSeq(
      IndexedSeq(1, 2),
      IndexedSeq(3, 0))) == Vector.empty[Direction])

    assert(BreadthFirst.tileSlide(IndexedSeq(
      IndexedSeq(2, 0),
      IndexedSeq(1, 3))) == Vector(Right, Up, Left))

    assert(BreadthFirst.tileSlide(IndexedSeq(
      IndexedSeq(1, 0, 5),
      IndexedSeq(4, 8, 3),
      IndexedSeq(6, 7, 2))) == Vector(Right, Up, Left, Up, Right, Down, Left, Left, Up, Right, Right, Down, Down, Left,
      Up, Left, Down, Right, Up, Left, Up))

  }

}
