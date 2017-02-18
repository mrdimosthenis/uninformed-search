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
      IndexedSeq(9, 0, 7, 8, 0, 0, 0, 4, 0),
      IndexedSeq(0, 0, 0, 0, 3, 1, 0, 0, 0),
      IndexedSeq(0, 6, 0, 0, 0, 4, 5, 2, 0),
      IndexedSeq(0, 9, 0, 0, 0, 0, 8, 7, 3),
      IndexedSeq(0, 0, 2, 0, 0, 3, 0, 0, 0),
      IndexedSeq(4, 8, 0, 0, 5, 0, 6, 0, 0),
      IndexedSeq(1, 0, 0, 0, 0, 0, 0, 0, 5),
      IndexedSeq(0, 3, 5, 0, 6, 0, 0, 8, 0),
      IndexedSeq(0, 4, 0, 2, 7, 0, 0, 0, 9))) == IndexedSeq(
      IndexedSeq(9, 5, 7, 8, 2, 6, 3, 4, 1),
      IndexedSeq(8, 2, 4, 5, 3, 1, 7, 9, 6),
      IndexedSeq(3, 6, 1, 7, 9, 4, 5, 2, 8),
      IndexedSeq(5, 9, 6, 4, 1, 2, 8, 7, 3),
      IndexedSeq(7, 1, 2, 6, 8, 3, 9, 5, 4),
      IndexedSeq(4, 8, 3, 9, 5, 7, 6, 1, 2),
      IndexedSeq(1, 7, 9, 3, 4, 8, 2, 6, 5),
      IndexedSeq(2, 3, 5, 1, 6, 9, 4, 8, 7),
      IndexedSeq(6, 4, 8, 2, 7, 5, 1, 3, 9)))

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

  test("hanoi") {
    assert(BreadthFirst.hanoi(1) == Vector((0,2)))
    assert(BreadthFirst.hanoi(2) == Vector((0,1), (0,2), (1,2)))
    assert(BreadthFirst.hanoi(3) == Vector((0,2), (0,1), (2,1), (0,2), (1,0), (1,2), (0,2)))
  }

}
