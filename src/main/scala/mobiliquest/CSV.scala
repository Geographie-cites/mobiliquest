package mobiliquest

import java.io.File

object CSV {

  type Header = String
  type Line = Seq[String]
  type Column = Seq[String]

  case class Content(header: Seq[String], fields: Seq[Line])
  def empty = Content(Seq(), Seq())

  def read(path: File): Content = read(io.Source.fromFile(path).getLines())

  def read(rawLines: Iterator[String]) = {
    val lines = rawLines.map{_.split(";").map{x=> x.replaceAll("\"","").trim}}.toSeq

    lines.size match {
      case 0 => empty
      case _ => Content(lines.head, lines.tail.map{_.toSeq})
    }
  }

  def columnIndex(columnName: String, content: Content) = content.header.indexOf(columnName)

  def column(columnName: String, content: Content): Column = {
    columnIndex(columnName, content) match {
      case -1 => Seq()
      case index: Int => content.fields.transpose.toSeq(index)
    }
  }

  def columnIndexes(columnNames: Seq[String], content: Content) = columnNames.map{cn=>
    columnIndex(cn, content)
  }

  def columns(columnNames: Seq[String], content: Content): Seq[Column] = {
    val indexes = columnIndexes(columnNames, content)
   content.fields.transpose.zipWithIndex.filter { case (col,ind)=>
      indexes.contains(ind)
    }.map{_._1}
  }

  def linesWhere(columnNames: Seq[String], content: Content): Seq[Seq[String]] = columns(columnNames, content).transpose
}
