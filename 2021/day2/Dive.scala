package day2

import scala.annotation.tailrec
import scala.io.Source

object Dive {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day2/input.txt")
    val input = str.getLines().toList
    println(run(input, 0, 0))
  }

  @tailrec
  def run(list: Seq[String], position: Int, depth: Int): Int = {
    if (list.isEmpty) position * depth
    else {
      val (direction, value) = parseInstruction(list.head);
      direction match {
        case "up" => run(list.tail, position, depth - value)
        case "down" => run(list.tail, position, depth + value)
        case "forward" => run(list.tail, position + value, depth)
      }
    }
  }

  def parseInstruction(instruction: String): (String, Int) = {
    val split = instruction.split("\\s+")
    (split.head, split.tail.head.toInt)
  }
}
