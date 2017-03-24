import models.direction.{Direction, Down, Left, Right, Up}
import models.node_classes.{HanoiNode, NQueensNode, SudokuNode, TileSlideNode}
import org.scalatest.FunSuite

class SearchTest extends FunSuite {

  test("sudoku") {

    assert(Search.simple(SudokuNode(IndexedSeq(IndexedSeq(0)))).contains(IndexedSeq(IndexedSeq(1))))

    assert(Search.simple(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4)))).contains(IndexedSeq(
      IndexedSeq(1, 2, 4, 3),
      IndexedSeq(4, 3, 1, 2),
      IndexedSeq(3, 4, 2, 1),
      IndexedSeq(2, 1, 3, 4))))

    assert(Search.simple(SudokuNode(IndexedSeq(
      IndexedSeq(9, 0, 7, 8, 0, 0, 0, 4, 0),
      IndexedSeq(0, 0, 0, 0, 3, 1, 0, 0, 0),
      IndexedSeq(0, 6, 0, 0, 0, 4, 5, 2, 0),
      IndexedSeq(0, 9, 0, 0, 0, 0, 8, 7, 3),
      IndexedSeq(0, 0, 2, 0, 0, 3, 0, 0, 0),
      IndexedSeq(4, 8, 0, 0, 5, 0, 6, 0, 0),
      IndexedSeq(1, 0, 0, 0, 0, 0, 0, 0, 5),
      IndexedSeq(0, 3, 5, 0, 6, 0, 0, 8, 0),
      IndexedSeq(0, 4, 0, 2, 7, 0, 0, 0, 9)))).contains(IndexedSeq(
      IndexedSeq(9, 5, 7, 8, 2, 6, 3, 4, 1),
      IndexedSeq(8, 2, 4, 5, 3, 1, 7, 9, 6),
      IndexedSeq(3, 6, 1, 7, 9, 4, 5, 2, 8),
      IndexedSeq(5, 9, 6, 4, 1, 2, 8, 7, 3),
      IndexedSeq(7, 1, 2, 6, 8, 3, 9, 5, 4),
      IndexedSeq(4, 8, 3, 9, 5, 7, 6, 1, 2),
      IndexedSeq(1, 7, 9, 3, 4, 8, 2, 6, 5),
      IndexedSeq(2, 3, 5, 1, 6, 9, 4, 8, 7),
      IndexedSeq(6, 4, 8, 2, 7, 5, 1, 3, 9))))

    assert(Search.simple(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 1, 4)))).isEmpty)

    //

    assert(Search.simple(SudokuNode(IndexedSeq(IndexedSeq(0))), isBreadthFirst = false).contains(IndexedSeq(IndexedSeq(1))))

    assert(Search.simple(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4))), isBreadthFirst = false).contains(IndexedSeq(
      IndexedSeq(1, 2, 4, 3),
      IndexedSeq(4, 3, 1, 2),
      IndexedSeq(3, 4, 2, 1),
      IndexedSeq(2, 1, 3, 4))))

    assert(Search.simple(SudokuNode(IndexedSeq(
      IndexedSeq(9, 0, 7, 8, 0, 0, 0, 4, 0),
      IndexedSeq(0, 0, 0, 0, 3, 1, 0, 0, 0),
      IndexedSeq(0, 6, 0, 0, 0, 4, 5, 2, 0),
      IndexedSeq(0, 9, 0, 0, 0, 0, 8, 7, 3),
      IndexedSeq(0, 0, 2, 0, 0, 3, 0, 0, 0),
      IndexedSeq(4, 8, 0, 0, 5, 0, 6, 0, 0),
      IndexedSeq(1, 0, 0, 0, 0, 0, 0, 0, 5),
      IndexedSeq(0, 3, 5, 0, 6, 0, 0, 8, 0),
      IndexedSeq(0, 4, 0, 2, 7, 0, 0, 0, 9))), isBreadthFirst = false).contains(IndexedSeq(
      IndexedSeq(9, 5, 7, 8, 2, 6, 3, 4, 1),
      IndexedSeq(8, 2, 4, 5, 3, 1, 7, 9, 6),
      IndexedSeq(3, 6, 1, 7, 9, 4, 5, 2, 8),
      IndexedSeq(5, 9, 6, 4, 1, 2, 8, 7, 3),
      IndexedSeq(7, 1, 2, 6, 8, 3, 9, 5, 4),
      IndexedSeq(4, 8, 3, 9, 5, 7, 6, 1, 2),
      IndexedSeq(1, 7, 9, 3, 4, 8, 2, 6, 5),
      IndexedSeq(2, 3, 5, 1, 6, 9, 4, 8, 7),
      IndexedSeq(6, 4, 8, 2, 7, 5, 1, 3, 9))))

    assert(Search.simple(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 1, 4))), isBreadthFirst = false).isEmpty)

  }

  test("NQueens") {

    assert(Search.simple(NQueensNode(Vector(-1))).contains(Vector(0)))

    assert(Search.simple(NQueensNode(Vector.fill(4)(-1))).contains(Vector(1, 3, 0, 2)))

    assert(Search.simple(NQueensNode(Vector.fill(5)(-1))).contains(Vector(0, 2, 4, 1, 3)))

    //

    assert(Search.simple(NQueensNode(Vector(-1)), isBreadthFirst = false).contains(Vector(0)))

    assert(Search.simple(NQueensNode(Vector.fill(4)(-1)), isBreadthFirst = false).contains(Vector(2, 0, 3, 1)))

    assert(Search.simple(NQueensNode(Vector.fill(5)(-1)), isBreadthFirst = false).contains(Vector(4, 2, 0, 3, 1)))

  }

  test("TileSlide") {

    assert(Search.withTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(1, 2),
      IndexedSeq(3, 0)))).contains(Vector.empty[Direction]))

    assert(Search.withTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 0),
      IndexedSeq(1, 3)))).contains(Vector(Right, Up, Left)))

    assert(Search.withTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(1, 0, 5),
      IndexedSeq(4, 8, 3),
      IndexedSeq(6, 7, 2))))
      .contains(Vector(Right, Up, Left, Up, Right, Down, Left, Left, Up, Right, Right, Down, Down, Left, Up, Left, Down,
        Right, Up, Left, Up)))

    assert(Search.withTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 1),
      IndexedSeq(3, 0)))).isEmpty)

    //

    assert(Search.withTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(1, 2),
      IndexedSeq(3, 0))), isBreadthFirst = false).contains(Vector.empty[Direction]))

    assert(Search.withTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 0),
      IndexedSeq(1, 3))), isBreadthFirst = false).contains(Vector(Right, Up, Left)))

    assert(Search.withTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 1),
      IndexedSeq(3, 0))), isBreadthFirst = false).isEmpty)

  }

  test("Hanoi") {

    assert(Search.withTracking(
      HanoiNode(IndexedSeq(List(0), List.empty[Int], List.empty[Int]))).contains(Vector((0,2))))

    assert(Search.withTracking(
      HanoiNode(IndexedSeq(List(0, 1), List.empty[Int], List.empty[Int]))).contains(Vector((0,1), (0,2), (1,2))))

    assert(Search.withTracking(HanoiNode(IndexedSeq(List(0, 1 ,2), List.empty[Int], List.empty[Int])))
      .contains(Vector((0,2), (0,1), (2,1), (0,2), (1,0), (1,2), (0,2))))

    assert(Search.withTracking(HanoiNode(IndexedSeq(List(1, 2), List.empty[Int], List.empty[Int]))).isEmpty)

    //

    assert(Search.withTracking(HanoiNode(IndexedSeq(List(0), List.empty[Int], List.empty[Int])),
      isBreadthFirst = false).contains(Vector((0,2))))

    assert(Search.withTracking(HanoiNode(IndexedSeq(List(0, 1), List.empty[Int], List.empty[Int])),
      isBreadthFirst = false).contains(Vector((0,2), (2,1), (0,2), (1,2))))

    assert(Search.withTracking(HanoiNode(IndexedSeq(List(0, 1 ,2), List.empty[Int], List.empty[Int])),
      isBreadthFirst = false)
      .contains(Vector((0,2), (2,1), (0,2), (1,2), (2,0), (2,1), (0,2), (2,1), (0,2), (1,2), (2,0), (1,2), (0,2))))

    assert(Search.withTracking(HanoiNode(IndexedSeq(List(1, 2), List.empty[Int], List.empty[Int])),
      isBreadthFirst = false).isEmpty)

  }

}
