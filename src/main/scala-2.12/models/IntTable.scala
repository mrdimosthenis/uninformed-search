package models

case class IntTable(content: IndexedSeq[IndexedSeq[Int]]) {

  def indexesOf(x :Int): (Int, Int) = {
    val i = content.indexWhere(v => v.contains(0))
    (i, content(i).indexOf(0))
  }

  def updated(i: Int, j: Int, newVal: Int): IndexedSeq[IndexedSeq[Int]] = {
    content.updated(i, content(i).updated(j, newVal))
  }

  def exchanged(i0: Int, j0: Int, i1: Int, j1: Int): IndexedSeq[IndexedSeq[Int]] = {
    val updatedByI = content.updated(i0, content(i0).updated(j0, content(i1)(j1)))
    updatedByI.updated(i1, updatedByI(i1).updated(j1, content(i0)(j0)))
  }

}
