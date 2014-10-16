package eu.edssystem.candidatetask.model

case class Node(topLeft: Option[Int],
                topRight: Option[Int],
                bottomLeft: Option[Int],
                bottomRight: Option[Int]) {

  def containsFourAttachments: Boolean =
    !topLeft.isEmpty && !topRight.isEmpty && !bottomLeft.isEmpty && !bottomRight.isEmpty
}
