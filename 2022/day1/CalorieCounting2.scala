package day1

import scala.annotation.tailrec
import scala.io.Source

object CalorieCounting2 {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day1/input.txt")
    val input = str.getLines().toSeq
    println(run(input.tail, new Array[Int](input.size), 0, Seq()).sum)
  }

  @tailrec
  def run(next: Seq[String], result: Array[Int], pos: Int, top3: Seq[Int]): Seq[Int] = {
    if (next.isEmpty) buildTop3(top3, result(pos))
    else if (next.head.isEmpty) run(next.tail, result, pos + 1, buildTop3(top3, result(pos)))
    else {
      result(pos) = result(pos) + next.head.toInt
      run(next.tail, result, pos, top3)
    }
  }

  private def buildTop3(top3: Seq[Int], entry: Int) = {
    (top3 :+ entry).sorted(Ordering.Int.reverse).take(3)
  }
}
