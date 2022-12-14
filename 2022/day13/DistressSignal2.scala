package day13

import scala.annotation.tailrec
import scala.io.Source

object DistressSignal2 {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day13/input.txt")
    val input = str.getLines().toSeq.filter(_.startsWith("["))

    val value = input
      .filterNot(_.exists(a => a.isDigit)).sortBy(_.length) ++ input.filter(_.exists(a => a.isDigit))
      .map(_.replaceAll("\\[", "").replaceAll("]", ""))
      .sortWith((a, b) => sorting(a, b))

    val i1 = value.indexWhere(i => if (i.head == '[' || i.head == ',') false else i.head.toString.toInt == 2) + 1
    val i2 = value.indexWhere(i => if (i.head == '[' || i.head == ',') false else i.head.toString.toInt == 6) + 2
    println(i1 * i2)
  }

  @tailrec
  private def sorting(a: String, b: String): Boolean = {
    val aSplit = a.split(",").toSeq
    val bSplit = b.split(",").toSeq
    val f = if (aSplit.head.isEmpty) 0 else aSplit.head.toInt
    val s = if (bSplit.head.isEmpty) 0 else bSplit.head.toInt
    if (f == 0 && s == 0) true
    else if (f == s) sorting(aSplit.drop(1).mkString(","), bSplit.drop(1).mkString(","))
    else f < s
  }
}