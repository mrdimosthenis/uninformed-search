import search.{Bidirectional, BreadthFirst, DepthFirst}

object Main extends App{

  println(Bidirectional.tileSlide(IndexedSeq(
    IndexedSeq(7, 3, 5),
    IndexedSeq(1, 4, 0),
    IndexedSeq(6, 8, 2))))

}
