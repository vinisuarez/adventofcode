package day6

import scala.io.Source

object TuningTrouble {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day6/input.txt")
    val input = str.getLines().toSeq.head
    println(input.zipWithIndex.find(i => input.substring(i._2, i._2 + 4).toSet.size == 4).get._2 + 4)
    println(input.zipWithIndex.find(i => input.substring(i._2, i._2 + 14).toSet.size == 14).get._2 + 14)
  }
}
