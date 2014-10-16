package eu.edssystem.candidatetask

import eu.edssystem.candidatetask.model.Node

object NodeFactory {

  private val map =
    """___________/
      ___*_*_*___/
      ____0_1____/
      _*_*_*_*_*_/
      __2_3_4_5__/
      _*_*_*_*_*_/
      __6_7_8_9__/
      _*_*_*_*_*_/
      ____A_B____/
      ___*_*_*___/
      ___________""".replaceAll("\\s", "")


  def buildNodes: Seq[Node] = {
    val elementMap: Array[Array[Char]] =
      for {
        line <- map.split("/")
      } yield line.toCharArray

    val nodes=for {(line, i) <- elementMap.zipWithIndex
         (char, j) <- line.zipWithIndex
         if char == '*'
    } yield getNode(i, j, elementMap)
    nodes.to[List]

  }

  private def getNode(i: Int, j: Int, elementMap: Array[Array[Char]]): Node = {

    def charToSqIndex(ch: Char) = Character.digit(ch, 16)
    def getSquareRef(ch: Char): Option[Int] = {
      ch match {
        case '_' => None
        case ch: Char => Some(charToSqIndex(ch))
      }
    }
    val upperLeft = getSquareRef(elementMap(i - 1)(j - 1))
    val upperRight = getSquareRef(elementMap(i - 1)(j + 1))
    val bottomLeft = getSquareRef(elementMap(i + 1)(j - 1))
    val bottomRight = getSquareRef(elementMap(i + 1)(j + 1))

    Node(upperLeft, upperRight, bottomLeft, bottomRight)

  }


}
