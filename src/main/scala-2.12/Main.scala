import search.{Bidirectional, BreadthFirst, DepthFirst}

object Main extends App{

  println(Bidirectional.tileSlide(IndexedSeq(
    IndexedSeq(1, 2),
    IndexedSeq(3, 0))))

}
