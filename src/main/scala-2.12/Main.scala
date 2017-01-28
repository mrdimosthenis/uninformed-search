import search.BreadthFirst

object Main extends App{

  println(BreadthFirst.tileSlide(IndexedSeq(
    IndexedSeq(1, 2),
    IndexedSeq(3, 0))))

}
