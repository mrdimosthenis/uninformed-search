package models

case class HanoiNode(instance: IndexedSeq[List[Int]], path: Vector[(Int, Int)] = Vector.empty[(Int, Int)])
  extends TrackNode[IndexedSeq[List[Int]], (Int, Int)]{

  val numOfDisks: Int = instance.map(st => st.length).sum
  val fullStack:List[Int] = Range(0, numOfDisks).toList
  val goalInstance = IndexedSeq(List.empty[Int], List.empty[Int], fullStack)

  override def neighbors(trackSet: Set[IndexedSeq[List[Int]]]): Vector[HanoiNode] = {
    (for (i <- Range(0,3); j <- Range(0,3)) yield (i, j)).filter(t => {
      val fromStack = instance(t._1)
      val toStack = instance(t._2)
      t._1 != t._2 && fromStack.nonEmpty && (toStack.isEmpty || toStack.head > fromStack.head)
    }).map(t => {
      val from = t._1
      val to = t._2
      HanoiNode(instance.updated(to, instance(from).head :: instance(to)).updated(from, instance(from).tail),
        path :+ (from, to))
    }).filter(n => !trackSet.contains(n.instance)).toVector
  }

  override def isSolution: Boolean = instance == goalInstance

}
