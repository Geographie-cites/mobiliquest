package mobiliquest

import mobiliquest.CSV.Header
import scala.annotation.targetName

object Quest {

  sealed trait Rule

  case class SuchAsInt(operator: Int => Boolean) extends Rule

  case class SuchAsDouble(operator: Double => Boolean) extends Rule

  case class IsIn(values: Seq[String]) extends Rule

  case object NoRule extends Rule

  case class Where(header: CSV.Header, request: Request)

  case class RuleOnColumn(header: CSV.Header, rule: Rule)

  case class Request(content: CSV.Content, selected: Seq[CSV.Header], rulesOnColumns: Seq[RuleOnColumn])

  implicit class WhereDecorator(w: Where) {
    def suchAs(x: Int => Boolean): Request = w.request.copy(rulesOnColumns = w.request.rulesOnColumns :+ RuleOnColumn(w.header, SuchAsInt(i => x(i))))

    @targetName("suchAsDouble")
    def suchAs(x: Double => Boolean): Request = w.request.copy(rulesOnColumns = w.request.rulesOnColumns :+ RuleOnColumn(w.header, SuchAsDouble(i => x(i))))

    def isIn(values: String*) =
      w.request.copy(rulesOnColumns = w.request.rulesOnColumns :+ RuleOnColumn(w.header, IsIn(values)))
  }

  implicit class RuleOnColumnDecorator(ruleOnColumn: RuleOnColumn) {
    def compute(column: CSV.Column): Seq[Int] = {
        column.zipWithIndex.filter {case (el,ind)=>
          ruleOnColumn.rule match {
            case sa: SuchAsInt=> sa.operator(el.toString.toInt)
            case sa: SuchAsDouble=> sa.operator(el.toString.toDouble)
            case iin: IsIn=> iin.values.contains(el)
            case _=> true
          }
        }.map{_._2}
    }
  }

  implicit class ContentDecorator(content: CSV.Content) {
    def column(columnName: String) = CSV.column(columnName, content)

    def select(headers: CSV.Header*) = Request(content, headers, Seq())

    def where(header: Header) = Request(content, content.headers, Seq()).where(header)
  }

  implicit class RequestDecorator(request: Request) {

    def where(header: CSV.Header) = Where(header, request)

    def quest = {
      def quest0(selectedContent: CSV.Content, rulesToBeApplied: Seq[RuleOnColumn]): CSV.Content = {
        println("content size " + selectedContent.columns.head.size)
        if (rulesToBeApplied.isEmpty) selectedContent
        else {
          val ruleOnColumn = rulesToBeApplied.head

          // Get column indexes after filtering
          val newRuleResult = ruleOnColumn.compute(CSV.column(ruleOnColumn.header, selectedContent))
          
          // Produce new content where filter is applied on each column
          val newContent = CSV.linesWhere(newRuleResult, selectedContent)
          quest0(newContent, rulesToBeApplied.tail)
        }
      }

      // Gest only columns of interest (selected in 'select' instruction or used in request filters
      val selection = doSelect((request.selected ++ request.rulesOnColumns.map {
        _.header
      }).distinct, request.content)

        quest0(selection, request.rulesOnColumns)
    }
  }

  def doSelect(headers: Seq[CSV.Header], content: CSV.Content) = {
    CSV.Content(headers.toIndexedSeq, CSV.columns(headers, content))
  }

}
