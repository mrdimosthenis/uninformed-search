package models

import models.direction.{Direction, Down, Left, Right, Up}

case class TileSlideNode(instance: IndexedSeq[IndexedSeq[Int]], moves: Vector[Direction])
  extends TrackNode[IndexedSeq[IndexedSeq[Int]], Direction]{

  val puzzleSize: Int = instance.length

  override def neighbors(trackSet: Set[IndexedSeq[IndexedSeq[Int]]]): Vector[TileSlideNode] = {
    val intTable = IntTable(instance)
    val (i, j) = intTable.indexesOf(0)
    Vector(Tuple3(i + 1, j, Up), Tuple3(i, j + 1, Left), Tuple3(i - 1, j, Down), Tuple3(i, j - 1, Right))
      .filter(t => t._1 >= 0 && t._1 < puzzleSize && t._2 >= 0 && t._2 < puzzleSize)
      .map(t => TileSlideNode(intTable.exchanged(i, j, t._1, t._2), moves :+ t._3))
      .filter(n => !trackSet.contains(instance))
  }

  override def isSolution: Boolean = IndexedSeq.tabulate(puzzleSize, puzzleSize)
  {(i, j) => if (i == puzzleSize - 1 && j == puzzleSize - 1) 0 else puzzleSize * i + j + 1} == instance

}
