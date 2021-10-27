package mobiliquest

import mobiliquest.CSV.Header
import scala.annotation.targetName

object Quest {

  sealed trait Rule
  case class SuchAsInt(operator: Int=> Boolean) extends Rule
  case class SuchAsDouble(operator: Double=> Boolean) extends Rule
  case class IsIn(values: Seq[String]) extends Rule
  case object NoRule extends Rule

  case class Where(header: CSV.Header, request: Request)
  case class RuleOnColumn(header: CSV.Header, rule: Rule)
  case class Request(content: CSV.Content, selected: Seq[CSV.Header], rulesOnColumns: Seq[RuleOnColumn])

  implicit class WhereDecorator(w: Where) {
    def suchAs(x: Int=> Boolean): Request =  w.request.copy(rulesOnColumns = w.request.rulesOnColumns :+ RuleOnColumn(w.header, SuchAsInt(i => x(i))))
    @targetName("suchAsDouble")
    def suchAs(x: Double=> Boolean):Request = w.request.copy(rulesOnColumns = w.request.rulesOnColumns :+ RuleOnColumn(w.header, SuchAsDouble(i => x(i))))

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
      def quest0(selectedContent: CSV.Content, selectedLineIndexes: Seq[Int], rulesToBeApplied: Seq[RuleOnColumn]): Seq[Int] = {
        println("content size " + selectedContent.columns.head.size)
        if (rulesToBeApplied.isEmpty) selectedLineIndexes
        else {
          val ruleOnColumn = rulesToBeApplied.head

          val newRuleResult =  ruleOnColumn.compute(CSV.column(ruleOnColumn.header, selectedContent))
          val newLineSelection = {
            if (selectedLineIndexes.isEmpty) newRuleResult
            else selectedLineIndexes intersect newRuleResult
          }
          quest0(selectedContent, newLineSelection, rulesToBeApplied.tail)
        }
      }

      val selection = doSelect((request.selected ++ request.rulesOnColumns.map{_.header}).distinct, request.content)

      val quested = quest0(selection, 0 to selection.columns.headOption.map{_.length}.getOrElse(0), request.rulesOnColumns)

      CSV.linesWhere(request.selected, quested, selection)
    }
  }

  def doSelect(headers: Seq[CSV.Header], content: CSV.Content) = {
    CSV.Content(headers.toIndexedSeq, CSV.columns(headers, content))
  }

}
