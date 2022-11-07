package day1

import scala.annotation.tailrec
import scala.io.Source

object SonarSweep2 {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day1/input.txt")
    val input = str.getLines().map(_.toInt).toList
    println(run(input.tail, input.take(3).sum, 0))
  }

  @tailrec
  def run(input: Seq[Int], previous: Int, acc: Int): Int = {
    if (input.size < 3) acc
    else {
      val current = input.take(3).sum
      run(input.tail, current, if (current > previous) acc + 1 else acc)
    }
  }
}
