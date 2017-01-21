import puzzles.{NQueens, Puzzle}
import search_algorithms.BreadthFirst

object Main extends App{

  val fourQueens = {new NQueens(5)}.asInstanceOf[Puzzle[Any]]

  val breadthFirstFourQueensSolutions = {new BreadthFirst(fourQueens)}.solutions

  breadthFirstFourQueensSolutions.foreach(s => {
    fourQueens.pPrint(s)
    println()
  })

}
