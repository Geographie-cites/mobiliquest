package mobiliquest

import java.io.File

object CSV {

  type Header = String
  type Field = String
  type Line = Array[String]
  type Column = Seq[String]

  case class Content(header: Seq[String], fields: Seq[Line])
  def empty = Content(Seq(), Seq())

  implicit class ContentDecorator(content: Content) {
    def column(columnName: String) = CSV.column(columnName, content)
  }

  def read(path: File): CSV.Content = read(io.Source.fromFile(path).getLines())

  def read(rawLines: Iterator[String]) = {
    val lines = rawLines.map{_.split(",").map{_.trim}}.toSeq

    lines.size match {
      case 0 => CSV.empty
      case _ => CSV.Content(lines.head, lines.tail)
    }
  }

  def column(columnName: String, content: Content): Column = {
    content.header.indexOf(columnName) match {
      case -1 => Seq()
      case index: Int => content.fields.transpose.toSeq(index)
    }
  }
}
