import models.direction.{Direction, Down, Left, Right, Up}
import models._
import org.scalatest.FunSuite

class AlgorithmsTest extends FunSuite {

  test("sudoku") {

    assert(Algorithms.breadthFirst(SudokuNode(IndexedSeq(IndexedSeq(0))).asInstanceOf[Node[Any]])
      .contains(SudokuNode(IndexedSeq(IndexedSeq(1)))))

    assert(Algorithms.breadthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4))).asInstanceOf[Node[Any]]).contains(SudokuNode(IndexedSeq(
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
      IndexedSeq(0, 4, 0, 2, 7, 0, 0, 0, 9))).asInstanceOf[Node[Any]]).contains(SudokuNode(IndexedSeq(
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
      IndexedSeq(0, 0, 1, 4))).asInstanceOf[Node[Any]]).isEmpty)

    //

    assert(Algorithms.depthFirst(SudokuNode(IndexedSeq(IndexedSeq(0))).asInstanceOf[Node[Any]]).contains(SudokuNode(IndexedSeq(IndexedSeq(1)))))

    assert(Algorithms.depthFirst(SudokuNode(IndexedSeq(
      IndexedSeq(1, 0, 0, 0),
      IndexedSeq(0, 3, 0, 0),
      IndexedSeq(0, 4, 2, 0),
      IndexedSeq(0, 0, 0, 4))).asInstanceOf[Node[Any]]).contains(SudokuNode(IndexedSeq(
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
      IndexedSeq(0, 4, 0, 2, 7, 0, 0, 0, 9))).asInstanceOf[Node[Any]]).contains(SudokuNode(IndexedSeq(
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
      IndexedSeq(0, 0, 1, 4))).asInstanceOf[Node[Any]]).isEmpty)

  }

  test("NQueens") {

    assert(Algorithms.breadthFirst(NQueensNode(Vector(-1)).asInstanceOf[Node[Any]])
      .contains(NQueensNode(Vector(0))))

    assert(Algorithms.breadthFirst(NQueensNode(Vector.fill(4)(-1)).asInstanceOf[Node[Any]])
      .contains(NQueensNode(Vector(1, 3, 0, 2))))

    assert(Algorithms.breadthFirst(NQueensNode(Vector.fill(5)(-1)).asInstanceOf[Node[Any]])
      .contains(NQueensNode(Vector(0, 2, 4, 1, 3))))

    //

    assert(Algorithms.depthFirst(NQueensNode(Vector(-1)).asInstanceOf[Node[Any]])
      .contains(NQueensNode(Vector(0))))

    assert(Algorithms.depthFirst(NQueensNode(Vector.fill(4)(-1)).asInstanceOf[Node[Any]])
      .contains(NQueensNode(Vector(1, 3, 0, 2))))

    assert(Algorithms.depthFirst(NQueensNode(Vector.fill(5)(-1)).asInstanceOf[Node[Any]])
      .contains(NQueensNode(Vector(0, 2, 4, 1, 3))))

  }

  test("TileSlide") {

    assert(Algorithms.breadthFirstWithTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(1, 2),
      IndexedSeq(3, 0)), Vector.empty[Direction]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector.empty[Direction]))

    assert(Algorithms.breadthFirstWithTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 0),
      IndexedSeq(1, 3)), Vector.empty[Direction]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector(Right, Up, Left)))

    assert(Algorithms.breadthFirstWithTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(1, 0, 5),
      IndexedSeq(4, 8, 3),
      IndexedSeq(6, 7, 2)), Vector.empty[Direction]).asInstanceOf[TrackNode[Any, Any]])
      .contains(Vector(Right, Up, Left, Up, Right, Down, Left, Left, Up, Right, Right, Down, Down, Left, Up, Left, Down,
        Right, Up, Left, Up)))

    assert(Algorithms.breadthFirstWithTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 1),
      IndexedSeq(3, 0)), Vector.empty[Direction]).asInstanceOf[TrackNode[Any, Any]]).isEmpty)

    //

    assert(Algorithms.depthFirstWithTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(1, 2),
      IndexedSeq(3, 0)), Vector.empty[Direction]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector.empty[Direction]))

    assert(Algorithms.depthFirstWithTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 0),
      IndexedSeq(1, 3)), Vector.empty[Direction]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector(Up, Right, Down,
      Left, Up, Right, Down, Left, Up)))

    assert(Algorithms.depthFirstWithTracking(TileSlideNode(IndexedSeq(
      IndexedSeq(2, 1),
      IndexedSeq(3, 0)), Vector.empty[Direction]).asInstanceOf[TrackNode[Any, Any]]).isEmpty)

  }

  test("Hanoi") {

    assert(Algorithms.breadthFirstWithTracking(HanoiNode(IndexedSeq(List(0), List.empty[Int], List.empty[Int]),
      Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector((0,2))))

    assert(Algorithms.breadthFirstWithTracking(HanoiNode(IndexedSeq(List(0, 1), List.empty[Int], List.empty[Int]),
      Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector((0,1), (0,2), (1,2))))

    assert(Algorithms.breadthFirstWithTracking(HanoiNode(IndexedSeq(List(0, 1 ,2), List.empty[Int], List.empty[Int]),
        Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]])
      .contains(Vector((0,2), (0,1), (2,1), (0,2), (1,0), (1,2), (0,2))))

    assert(Algorithms.breadthFirstWithTracking(HanoiNode(IndexedSeq(List(1, 2), List.empty[Int], List.empty[Int]),
      Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]]).isEmpty)

    //

    assert(Algorithms.depthFirstWithTracking(HanoiNode(IndexedSeq(List(0), List.empty[Int], List.empty[Int]),
      Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector((0,1), (1,2))))

    assert(Algorithms.depthFirstWithTracking(HanoiNode(IndexedSeq(List(0, 1), List.empty[Int], List.empty[Int]),
      Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector((0,1), (0,2), (1,0), (0,2))))

    assert(Algorithms.depthFirstWithTracking(HanoiNode(IndexedSeq(List(0, 1 ,2), List.empty[Int], List.empty[Int]),
      Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]]).contains(Vector((0,1), (0,2), (1,0), (0,2), (0,1),
      (2,0), (0,1), (2,0), (1,0), (1,2), (0,1), (0,2), (1,0), (0,2))))

    assert(Algorithms.depthFirstWithTracking(HanoiNode(IndexedSeq(List(1, 2), List.empty[Int], List.empty[Int]),
      Vector.empty[(Int, Int)]).asInstanceOf[TrackNode[Any, Any]]).isEmpty)

  }

}
