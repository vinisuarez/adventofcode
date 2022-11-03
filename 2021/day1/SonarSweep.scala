package day1

import scala.annotation.tailrec
import scala.io.Source

object SonarSweep {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day1/input.txt")
    val input = str.getLines().map(_.toInt).toList
    println(run(input.tail, input.head, 0))
  }

  @tailrec
  def run(next: Seq[Int], previous: Int, acc: Int): Int = {
    if (next.isEmpty) acc
    else run(next.tail, next.head, if (next.head > previous) acc + 1 else acc)
  }
}
