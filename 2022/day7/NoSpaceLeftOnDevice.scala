package day7

import scala.annotation.tailrec
import scala.io.Source

object NoSpaceLeftOnDevice {
  case class Node(path: String, var value: Int, var childrenNode: Seq[String])

  def main(args: Array[String]): Unit = {
    val str = Source.fromFile(System.getProperty("user.dir") + "/2022/day7/input.txt")
    val input = str.getLines().toSeq

    run(input, Map("/" -> Node("/", 0, Seq())), "")
  }

  @tailrec
  def run(input: Seq[String], nodes: Map[String, Node], currentPath: String): Any = {
    if (input.isEmpty) {
      val root = nodes("/")
      val directorySizes = buildAllDirectoriesSize(nodes, root)
      //  part 1
      println(directorySizes.filter(_ < 100000).sum)
      // part 2
      val total = buildAllDirectoriesSize(nodes, Node("", 0, Seq(""))).max
      val freeSpace = 70000000 - total
      val neededSpace = 30000000 - freeSpace
      println(directorySizes.sorted.find(_ > neededSpace).get)
    }
    else {
      val current = input.head
      if (current.startsWith("$")) {
        if (current.startsWith("$ cd")) {
          val dir = current.split("\\s+")(2)
          if (dir.equals(".."))
            run(input.tail, nodes, currentPath.substring(0, currentPath.dropRight(1).lastIndexOf("/") + 1))
          else if (dir.equals("/"))
            run(input.tail, nodes, "/")
          else
            run(input.tail, nodes, currentPath + dir + "/")
        } else
          run(input.tail, nodes, currentPath)
      } else {
        if (current.startsWith("dir")) {
          val directory = current.split("\\s+")(1)
          val path = currentPath + directory + "/"
          val maybeNode = nodes.getOrElse(currentPath, Node(currentPath, 0, Seq()))
          maybeNode.childrenNode = maybeNode.childrenNode :+ directory
          run(input.tail, nodes ++ Map(path -> Node(path, 0, Seq())), currentPath)
        } else {
          val fileSize = current.split("\\s+")(0)
          val maybeNode = nodes.getOrElse(currentPath, new Node(currentPath, 0, Seq()))
          maybeNode.value = maybeNode.value + fileSize.toInt
          run(input.tail, nodes ++ Map(currentPath -> maybeNode), currentPath)
        }
      }
    }
  }

  def buildAllDirectoriesSize(nodes: Map[String, Node], node: Node): Seq[Int] = {
    node.childrenNode.map(p => getValue(nodes, nodes(node.path + p + "/"))) ++
      node.childrenNode.flatMap(p => buildAllDirectoriesSize(nodes, nodes(node.path + p + "/")))
  }

  def getValue(nodes: Map[String, Node], node: Node): Int = {
    node.value + node.childrenNode.map(p => getValue(nodes, nodes(node.path + p + "/"))).sum
  }
}
