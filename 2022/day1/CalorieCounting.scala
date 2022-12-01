package day1

import scala.annotation.tailrec
import scala.io.Source

object CalorieCounting {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day1/input.txt")
    val input = str.getLines().toSeq
    println(run(input.tail, new Array[Int](input.size), 0).toSeq.max)
  }

  @tailrec
  def run(next: Seq[String], result: Array[Int], pos: Int): Array[Int] = {
    if (next.isEmpty) result
    else if (next.head.isEmpty) run(next.tail, result, pos + 1)
    else {
      result(pos) = result(pos) + next.head.toInt
      run(next.tail, result, pos)
    }
  }
}
