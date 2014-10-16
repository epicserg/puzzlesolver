package eu.edssystem.candidatetask

import eu.edssystem.candidatetask.model.{Square, Node}

object CombinationVerifier {

  private val nodes: Seq[Node] = NodeFactory.buildNodes


  def verify(squareSeq: Seq[Square]): Boolean = {
    val squares: Vector[Square] = squareSeq.to[Vector]
    for (node: Node <- nodes) {
      val nodeConjunctionSum: Int = getConjunctionSumm(node, squares)
      if (nodeConjunctionSum > 10) {
        return false
      }
      if (node.containsFourAttachments && allAttachmentsPresent(node, squares) && nodeConjunctionSum != 10) {
        return false
      }
    }
    return true
  }

  private def getConjunctionSumm(node: Node, squares: Vector[Square]): Int = {

    val squareSize = squares.size

    def isSquareAbsent(number: Option[Int]) = number.isEmpty || number.head > squareSize - 1

    val topLeftSquareValue: Int =
      if (isSquareAbsent(node.topLeft)) 0
      else
        squares(node.topLeft.head).buttomRight

    val topRightSquareValue: Int =
      if (isSquareAbsent(node.topRight)) 0
      else
        squares(node.topRight.head).buttomLeft

    val bottomRightSquareValue: Int =
      if (isSquareAbsent(node.bottomRight)) 0
      else
        squares(node.bottomRight.head).topLeft

    val bottomLeftSquareValue: Int =
      if (isSquareAbsent(node.bottomLeft)) 0
      else
        squares(node.bottomLeft.head).topRight

    return topLeftSquareValue + topRightSquareValue + bottomLeftSquareValue + bottomRightSquareValue
  }

  private def allAttachmentsPresent(node: Node, squares: Vector[Square]): Boolean = {
    val squareSize = squares.size
      node.topLeft.head < squareSize &&
      node.topRight.head < squareSize &&
      node.bottomLeft.head < squareSize &&
      node.bottomRight.head < squareSize
  }

}
