package day5

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object SupplyStacks {

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day5/input.txt")
    val input = str.getLines().toSeq


    val creates = input.take(3).map(s => if (s.startsWith(" ") Array(s.take(3)) ++ s.drop(4).grouped(4).toArray).toArray
    val moves = input.drop(5)

    println(run(creates, moves))
  }



  @tailrec
  def run(creates: Array[Array[String]], moves: Seq[String]): String = {
    if (moves.isEmpty) getTopCreates(creates)
    else {
      val move = moves.head
      val quantity = move.charAt(5).toString.toInt
      val from = move.charAt(12).toString.toInt
      val to = move.charAt(17).toString.toInt

      creates(to) = creates(from).take(quantity).reverse ++ creates(to)
      creates(from) = 
      run(creates, moves.tail)
    }
  }

  def getTopCreates(creates: Array[Array[String]]): String = ???
}
