package day6

import scala.annotation.tailrec
import scala.io.Source

object Lanternfish {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day6/input.txt")
    val input = str.getLines().flatten(v => v.split(',')).map(_.toInt).toSeq

    println(run(input, 0))
  }

  @tailrec
  def run(list: Seq[Int], day: Int): Int =
    if (day == 80) list.size
    else run(list.map(v => if (v == 0) 6 else v - 1) ++ List.fill(list.count(_ == 0))(8), day + 1)
}
