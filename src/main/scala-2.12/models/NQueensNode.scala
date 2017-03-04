package models

case class NQueensNode(content: Vector[Int]) extends Node {

  override def neighbors: Vector[Node] = {
    val i = content.indexOf(-1)
    if (i == -1) Vector.empty[NQueensNode]
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
