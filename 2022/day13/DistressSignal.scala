package day13

import scala.io.Source

object DistressSignal {
  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day13/input.txt")
    val input = str.getLines().toSeq.filter(_.startsWith("[")).grouped(2).toSeq
    println(run(input, 1, 0))
  }

  def run(input: Seq[Seq[String]], index: Int, count: Int): Int = {
    if (input.isEmpty) count
    else {
      val pair = input.head
      val value = compare(pair(0), pair(1))
      if (value == 1) run(input.tail, index + 1, count + index)
      else run(input.tail, index + 1, count)
    }
  }

  def compare(f: String, s: String): Int = {
    if (f(0) != '[') {
      compare("[" + f + "]", s)
    } else if (s(0) != '[') {
      compare(f, "[" + s + "]")
    } else {
      val fCloseIndex = findIndexOfClose(f)
      val sCloseIndex = findIndexOfClose(s)

      val f2 = f.substring(1, fCloseIndex)
      val s2 = s.substring(1, sCloseIndex)

      val fSplit = f2.split(",")
      val sSplit = s2.split(",")

      for ((f3, index) <- fSplit.zipWithIndex) {
        if (sSplit.length == index || f3.equals("[[]]") && s2.equals("[]") || (s2.isEmpty && f3.nonEmpty)) return -1
        if (f3.isEmpty && sSplit(index).nonEmpty || f3.equals("[]") && s2.equals("[[]]")) return 1
        if (f3.isEmpty && sSplit(index).isEmpty || f3.equals("[]") && s2.equals("[]")) return 0
        if (f3.startsWith("[")) {
          val i = f2.indexOf("[")
          val fIndex = findIndexOfClose(f2.substring(i))
          val sIndex = findIndexOfClose(s2.substring(i))
          val first = compare(f2.substring(i, fIndex + i + 1), s2.substring(i, sIndex + i + 1))
          if (first != 0) return first
          else return compare(f2.substring(fIndex + 2), s2.substring(sIndex + 2))
        } else if (s2.startsWith("[")) {
          val fIndex = findIndexOfClose(f2)
          val sIndex = findIndexOfClose(s2)
          val sSub = s2.substring(0, sIndex + 1)
          if (sSub.equals("]")) return -1
          val first = compare(f2.substring(0, fIndex + 1), sSub)
          if (first != 0) return first
          if (f2.length < fIndex + 2) return 1
          else compare(f2.substring(fIndex + 2), s2.substring(sIndex + 2))
        } else if (sSplit(index).startsWith("[")) {
          val i = s2.indexOf("[")
          val fIndex = findIndexOfClose(f2.substring(i))
          val sIndex = findIndexOfClose(s2.substring(i))
          val fSub = f3.substring(0, fIndex + 1)
          if (fSub.equals("]")) return 1
          val first = compare(fSub, s2.substring(i, sIndex + i+1))
          if (first != 0) return first
          else compare(f2.substring(fIndex + 2), s2.substring(sIndex + 2))
        }
        else {
          val fInt = f3.toInt
          val sInt = sSplit(index).toInt
          if (fInt < sInt) return 1
          if (fInt > sInt) return -1
        }
      }
      if (f2.length < s2.length) 1
      else 0
    }
  }

  def findIndexOfClose(string: String): Int = {
    var counter = 0
    for((c,index) <- string.zipWithIndex) {
      if (c == '[') counter = counter + 1
      if (c == ']') counter = counter - 1
      if (counter == 0) return index
    }
    -1
  }
}