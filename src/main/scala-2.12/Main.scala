import puzzles.{NQueens, Puzzle}
import search_algorithms.BreadthFirst

object Main extends App{

  val fourQueens = new NQueens(5)

  val breadthFirstFourQueensSolutions = {new BreadthFirst(fourQueens.asInstanceOf[Puzzle[Any]])}.solutions

  println(breadthFirstFourQueensSolutions.length)

  /*breadthFirstFourQueensSolutions.foreach(s => {
    fourQueens.pPrint(s.asInstanceOf[Vector[Int]])
    println()
    println(s)
    println()
    println()
  })*/

}
