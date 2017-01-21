package search_algorithms.breadth_first

object NQueens extends App{

  val n = 4

  var queue = List.empty[Vector[Int]]
  queue = Vector.empty[Int] :: queue
  while (queue.nonEmpty){
    val children = (0 until n).map(i => queue.last :+ i)
    queue = queue.dropRight(1)
    children.foreach(ch => {
      queue = ch :: queue
      Thread.sleep(500)
      println(ch)
    })
  }

}
