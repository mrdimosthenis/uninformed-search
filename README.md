# Uninformed Search

## Overview

This project implements various uninformed search algorithms in Scala. Uninformed search algorithms are a type of search
algorithm that operate in a brute-force manner, without any domain-specific knowledge. The project includes
implementations for solving different types of puzzles and problems using these algorithms.

* Breadth-first search
* Depth-first search
* Breadth-first search with tracking
* Depth-first search with tracking

## Features

* Search Algorithms: Implements both breadth-first search (BFS) and depth-first search (DFS) algorithms.
* Puzzle Solvers: Includes solvers for Sudoku, N-Queens, Tile Slide, and Hanoi Tower problems.
* Tracking: Provides functionality to track the path taken to reach the solution.

## Getting Started

### Prerequisites

* Scala 2.12.1
* sbt (Scala Build Tool)

### Building the Project

To build the project, run the following command in the project root directory:

```bash
sbt compile
```

### Running Tests

To run the tests, execute the following command:

```bash
sbt test
```

## Usage

You can use the search algorithms to solve different puzzles by creating instances of the puzzle nodes and passing them to the Search object methods. For example, to solve a Sudoku puzzle:
    
```scala
import models.node_classes.SudokuNode
import Search.simple

val initialSudoku = IndexedSeq(
  IndexedSeq(1, 0, 0, 0),
  IndexedSeq(0, 3, 0, 0),
  IndexedSeq(0, 4, 2, 0),
  IndexedSeq(0, 0, 0, 4)
)

val solution = simple(SudokuNode(initialSudoku))
println(solution)
```
