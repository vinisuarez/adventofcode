package day5

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

object HydrothermalVenture {
  case class Coordinate(x: Int, y: Int)

  case class Direction(init: Coordinate, end: Coordinate)

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day5/input.txt")
    val validDirections = buildValidDirections(str)

    val board = populateBoard(Array.ofDim[Int](1000,1000), validDirections)

    println(countDangerousLines(board))
  }

  private def buildValidDirections(str: BufferedSource): Seq[Direction] = {
    str.getLines()
      .map(line => {
        val directionSplit = line.split("->")
        val initSplit = directionSplit.head.split(",")
        val endSplit = directionSplit.tail.head.split(",")
        Direction(
          Coordinate(initSplit.head.trim.toInt, initSplit.tail.head.trim.toInt),
          Coordinate(endSplit.head.trim.toInt, endSplit.tail.head.trim.toInt)
        )
      })
      .filter(d => d.init.x == d.end.x || d.init.y == d.end.y).toSeq
  }

  @tailrec
  private def populateBoard(board: Array[Array[Int]], directions: Seq[Direction]): Array[Array[Int]] = {
    if (directions.isEmpty) board
    else {
      populateBoard(markTheBoard(board, directions.head), directions.tail)
    }
  }

  @tailrec
  private def markTheBoard(board: Array[Array[Int]], direction: Direction): Array[Array[Int]] = {
    board(direction.init.x)(direction.init.y) = board(direction.init.x)(direction.init.y) + 1
    if (direction.init.x == direction.end.x && direction.init.y == direction.end.y) board
    else {
      if (direction.init.x > direction.end.x) {
        markTheBoard(board, Direction(Coordinate(direction.init.x - 1, direction.init.y), direction.end))
      } else if (direction.init.x < direction.end.x) {
        markTheBoard(board, Direction(Coordinate(direction.init.x + 1, direction.init.y), direction.end))
      } else if (direction.init.y > direction.end.y) {
        markTheBoard(board, Direction(Coordinate(direction.init.x, direction.init.y - 1), direction.end))
      } else {
        markTheBoard(board, Direction(Coordinate(direction.init.x, direction.init.y + 1), direction.end))
      }
    }
  }

  private def countDangerousLines(board: Array[Array[Int]]): Int = {
    board.map(b=> b.count(_ > 1)).sum
  }
}
