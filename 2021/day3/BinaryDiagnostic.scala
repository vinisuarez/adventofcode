package day3

import scala.annotation.tailrec
import scala.io.Source

object BinaryDiagnostic {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2021/day3/input.txt")
    val input = str.getLines().toSeq
    println(run(input, 0, "", ""))
  }


  @tailrec
  def run(input: Seq[String], pos: Int, gamma: String, epsilon: String): Int = {
    if (pos == input.head.length) Integer.parseInt(gamma, 2) * Integer.parseInt(epsilon, 2)
    else {
      val (nextGamma, nextEpsilon) = countForPosition(input, pos)
      run(input, pos + 1, gamma + nextGamma, epsilon + nextEpsilon)
    }
  }


  def countForPosition(input: Seq[String], pos: Int): (Int, Int) = {
    val oneCount = input.map(_.charAt(pos).toString.toInt).sum
    val zeroCount = input.size - oneCount
    if (oneCount > zeroCount) (1, 0)
    else (0, 1)
  }
}
