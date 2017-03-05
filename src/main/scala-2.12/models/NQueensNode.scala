package models

case class NQueensNode(content: Vector[Int]) extends Node[Vector[Int]] {

  override def neighbors: Vector[Node[Vector[Int]]] = {
    val i = content.indexOf(-1)
    if (i == -1) Vector.empty[Node[Vector[Int]]]
    else {
      content.indices.map(e => content.updated(i, e)).filter(v => {
        val subVec = v.slice(0, i + 1)
        subVec.distinct.length == subVec.length &&
          subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == subVec.length &&
          subVec.zipWithIndex.map(t => t._1 - t._2).distinct.length == subVec.length
      }).map(n => NQueensNode(n)).toVector
    }
  }

  override def isSolution: Boolean = content.indexOf(-1) == -1

}
