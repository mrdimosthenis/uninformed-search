package search

import org.scalatest.FunSuite

class BidirectionalTest extends FunSuite {

  test("tileSlide") {

    import models.direction._

    assert(Bidirectional.tileSlide(IndexedSeq(
      IndexedSeq(1, 2),
      IndexedSeq(3, 0))) == Vector.empty[Direction])

    assert(Bidirectional.tileSlide(IndexedSeq(
      IndexedSeq(2, 0),
      IndexedSeq(1, 3))) == Vector(Right, Up, Left))

    assert(Bidirectional.tileSlide(IndexedSeq(
      IndexedSeq(8, 3, 2),
      IndexedSeq(1, 6, 4),
      IndexedSeq(5, 0, 7))) == Vector(Down, Left, Up, Right, Down, Down, Right, Up, Up, Left, Left, Down, Right, Down,
      Left, Up, Right, Up, Left, Down, Right, Right, Up, Left, Left))

  }

  test("hanoi") {
    assert(Bidirectional.hanoi(1) == Vector((0,2)))
    assert(Bidirectional.hanoi(2) == Vector((0,1), (0,2), (1,2)))
    assert(Bidirectional.hanoi(3) == Vector((0,2), (0,1), (2,1), (0,2), (1,0), (1,2), (0,2)))
  }

}
