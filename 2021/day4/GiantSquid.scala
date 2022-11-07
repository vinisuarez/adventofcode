package day4

import scala.annotation.tailrec
import scala.io.Source

object GiantSquid {
  case class Game(board: Seq[Seq[String]], hitDrawPositions: Seq[String])

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day4/input.txt")
    val input = str.getLines().toSeq

    val drawNumbers = input.head.split(',')
    val games = input.tail.filterNot(_.isEmpty).grouped(5).map(a => a.map(_.split("\\s+").filterNot(_.isEmpty).toSeq)).map(b => Game(b, Seq())).toSeq

    println(run(games, drawNumbers.tail, drawNumbers.head))
  }

  @tailrec
  def run(games: Seq[Game], leftNumbers: Seq[String], drawNumber: String): Int = {
    val updatedGames = games.map(g => {
      val hits = findElement(g.board, drawNumber)
      if (hits.isEmpty) g
      else Game(g.board, g.hitDrawPositions :+ hits)
    })

    val maybeGame = hasWinner(updatedGames)
    if (maybeGame.isDefined) getSumFromNonDraw(maybeGame.get) * drawNumber.toInt
    else run(updatedGames, leftNumbers.tail, leftNumbers.head)
  }

  def findElement(table: Seq[Seq[String]], elem: String): String = {
    val row = table.indexWhere(_.contains(elem))
    if (row > -1) row.toString + table(row).indexOf(elem)
    else ""
  }

  val winSequences: Seq[IndexedSeq[String]] = Range(0, 5).map(i => Range(0, 5).map(y => i.toString + y)) ++
    Range(0, 5).map(i => Range(0, 5).map(y => y.toString + i))

  def hasWinner(games: Seq[Game]): Option[Game] = {
    games.find(g => winSequences.exists(ws => ws.toSet.subsetOf(g.hitDrawPositions.toSet)))
  }

  def getSumFromNonDraw(game: Game): Int = {
    game.board.flatten.map(_.toInt).sum - game.hitDrawPositions.map(p => {
      game.board(p.head.toString.toInt)(p.tail.head.toString.toInt).toInt
    }).sum
  }
}
