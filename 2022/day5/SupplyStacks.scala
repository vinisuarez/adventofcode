package day5

import scala.annotation.tailrec
import scala.io.Source

object SupplyStacks {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day5/input.txt")
    val input = str.getLines().toSeq
    val creates = input.take(8).map(s => s.grouped(4).toArray).toArray.transpose.map(_.dropWhile(c => c.equals("    ") || c.equals("   ")))
    val moves = input.drop(10)
    println(run(creates.clone(), moves, shouldReverse = true))
    println(run(creates.clone(), moves, shouldReverse = false))
  }

  @tailrec
  def run(creates: Array[Array[String]], moves: Seq[String], shouldReverse: Boolean): String = {
    if (moves.isEmpty) creates.map(_.head.charAt(1).toString).mkString
    else {
      val pattern = "move ([0-9]+) from ([0-9]+) to ([0-9]+)".r
      val (quantity, from, to) = moves.head match {
        case pattern(q, f, t) => (q.toInt, f.toInt - 1, t.toInt - 1)
      }
      creates(to) = (if (shouldReverse) creates(from).take(quantity).reverse else creates(from).take(quantity)) ++ creates(to)
      creates(from) = creates(from).drop(quantity)
      run(creates, moves.tail, shouldReverse)
    }
  }
}
