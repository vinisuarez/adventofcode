package day8

import scala.io.Source

object SevenSegmentSearch {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day8/input.txt")
    val outputs = str.getLines().flatten(_.split('|').tail).toSeq
    val filter = Seq(2, 3, 4, 7)
    val input = outputs.flatten(_.split("\\s+").filter(n => filter.contains(n.length)))

    println(input.size)
  }
}
