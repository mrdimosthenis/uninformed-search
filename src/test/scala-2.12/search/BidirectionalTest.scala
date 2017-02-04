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
      IndexedSeq(1, 6, 5),
      IndexedSeq(0, 7, 2),
      IndexedSeq(3, 4, 8))) == Vector(Up, Left, Down, Down, Left, Up, Right, Right, Up, Left, Left, Down, Right, Down,
      Left, Up, Up))

  }

}
