package eu.edssystem.candidatetask.model

case class Square(topLeft: Int, topRight: Int, buttomLeft: Int, buttomRight: Int , id : Int) {
  def formattedString: String =topLeft+" "+topRight+" "+buttomLeft+" "+buttomRight+System.lineSeparator()
};
