package search_algorithms.breadth_first

import scala.collection.immutable.Queue

object NQueens extends App{

  val n = 4

  var queue = Queue(Vector.empty[Int])
  while (queue.nonEmpty){
    queue = queue.enqueue((0 until n).map(i => queue.dequeue._1 :+ i))
    queue = queue.dequeue._2
    Thread.sleep(500)
    println(queue)
  }

}
