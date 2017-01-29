import search.{BreadthFirst, DepthFirst}

object Main extends App{

  println(DepthFirst.tileSlide(IndexedSeq(
    IndexedSeq(1, 0, 5),
    IndexedSeq(4, 8, 3),
    IndexedSeq(6, 7, 2))).length)

}
