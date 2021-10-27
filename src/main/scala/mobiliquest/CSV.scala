package mobiliquest

import java.io.File

object CSV {

  type Header = String
  type Column = Seq[String | Int | Double]
  type Line = Seq[Column]

  case class Content(headers: IndexedSeq[String], columns: Seq[Column])

  def read(path: File): Content = read(io.Source.fromFile(path).getLines())

  def read(rawLines: Iterator[String]) = {
    val lines = rawLines.map {
      _.split(";").map {
        _.replaceAll("\"", "").trim
      }
        .toIndexedSeq
    }.toSeq

    val headers: IndexedSeq[Header] = lines.headOption.map {
      _.toIndexedSeq
    }.getOrElse(IndexedSeq[Header]())

    val rawColumns = lines.size match {
      case 0 => IndexedSeq()
      case _ => lines.tail.transpose.map {
        _.toSeq
      }
    }

    val columns: Seq[Column] = if (rawColumns.headOption.map {
      _.size
    }.getOrElse(0) > 0) {
      rawColumns.map { rc =>
        rc.head match {
          case v if Seq("AGE").contains(v) => rc.map {
            _.toInt
          }
          case _ => rc
        }
      }
    }
    else Seq()

    Content(lines.head, columns)
  }

  def columnIndex(columnName: String, content: Content) = content.headers.indexOf(columnName)

  def column(columnName: String, content: Content): Column = {
    columnIndex(columnName, content) match {
      case -1 => Seq()
      case index: Int => content.columns.toSeq(index)
    }
  }

  def columnIndexes(columnNames: Seq[String], content: Content) = columnNames.map { cn =>
    columnIndex(cn, content)
  }

    def columns(columnNames: Seq[String], content: Content): Seq[Column] = {
  //    content.headers.zip(content.columns).filter { case (head, col) =>
  //      columnNames.contains(head)
  //    }.map {
  //      _._2
  //    }
      columnNames.map { cn =>
        CSV.column(cn, content)
      }
    }

  def linesWhere(columnNames: Seq[String], selectedLineIndexes: Seq[Int], selectedContent: Content): Seq[Seq[String | Int | Double]] = {
    val requestedColumns = columns(columnNames, selectedContent)
    // Filter selected indexed lines on requested columns
    selectedLineIndexes.flatMap(requestedColumns.transpose.lift)

  }

}
