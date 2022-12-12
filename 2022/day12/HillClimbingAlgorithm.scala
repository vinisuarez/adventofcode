package day12

import scala.annotation.tailrec
import scala.io.Source

object HillClimbingAlgorithm {
  def main(args: Array[String]): Unit = {
    val src = Source.fromFile(System.getProperty("user.dir") + "/2022/day12/input.txt")
    val input = src.getLines().toSeq
    var goalPos = (0, 0)
    val board = input.flatten(_.grouped(1)).grouped(input.head.length).toSeq.zipWithIndex.map(x => x._1.zipWithIndex.map(y => {
      y._1.head match {
        case 'S' => 27
        case 'E' =>
          goalPos = (x._2, y._2)
          0
        case _ => 'z' + 1 - y._1.head;
      }
    }))
    println(run(board, Array.ofDim(board.length, board.head.length), goalPos, 27))
    println(run(board, Array.ofDim(board.length, board.head.length), goalPos, 26))
  }

  private def run(board: Seq[Seq[Int]], minPath: Array[Array[Int]], goalPos: (Int, Int), goalValue: Int): Int = {
    run2(0, minPath(goalPos._1)(goalPos._2) + 1, board, minPath, goalPos, goalValue, Integer.MAX_VALUE)
  }

  @tailrec
  def run2(
    option: Int,
    path: Int,
    board: Seq[Seq[Int]],
    minPath: Array[Array[Int]],
    goalPos: (Int, Int),
    goalValue: Int,
    step: Int
  ): Int = {
    if (option > 3) step
    else {
      var step2 = step
      val (x, y) = getXY(option, goalPos)
      if (x >= 0 &&
        x < board.length &&
        y >= 0 &&
        y < board.head.length &&
        board(x)(y) <= board(goalPos._1)(goalPos._2) + 1 &&
        (minPath(x)(y) == 0 || path < minPath(x)(y))
      ) {
        minPath(x)(y) = path
        if (board(x)(y) == goalValue) return path
        step2 = Math.min(step, run(board, minPath, (x, y), goalValue))
      }
      run2(option + 1, path, board, minPath, goalPos, goalValue, step2)
    }
  }

  def getXY(option: Int, goalPos: (Int, Int)): (Int, Int) = {
    option match {
      case 0 => (goalPos._1, goalPos._2 + 1)
      case 1 => (goalPos._1, goalPos._2 - 1)
      case 2 => (goalPos._1 + 1, goalPos._2)
      case 3 => (goalPos._1 - 1, goalPos._2)
    }
  }
}