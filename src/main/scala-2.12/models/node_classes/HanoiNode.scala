package models.node_classes

import models.node_traits.TrackNode

case class HanoiNode(content: IndexedSeq[List[Int]], path: Vector[(Int, Int)] = Vector.empty[(Int, Int)])
  extends TrackNode[IndexedSeq[List[Int]], (Int, Int)]{

  val numOfDisks: Int = content.map(st => st.length).sum
  val fullStack:List[Int] = Range(0, numOfDisks).toList
  val goalContent = IndexedSeq(List.empty[Int], List.empty[Int], fullStack)

  override def neighbors(trackSet: Set[IndexedSeq[List[Int]]]): Vector[HanoiNode] = {
    (for (i <- Range(0,3); j <- Range(0,3)) yield (i, j)).filter(t => {
      val fromStack = content(t._1)
      val toStack = content(t._2)
      t._1 != t._2 && fromStack.nonEmpty && (toStack.isEmpty || toStack.head > fromStack.head)
    }).map(t => {
      val from = t._1
      val to = t._2
      HanoiNode(content.updated(to, content(from).head :: content(to)).updated(from, content(from).tail),
        path :+ (from, to))
    }).filter(n => !trackSet.contains(n.content)).toVector
  }

  override def isSolution: Boolean = content == goalContent

}
