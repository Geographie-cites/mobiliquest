package mobiliquest

object Quest {

  sealed trait Rule
  case class SuchAs(operator: String=> Boolean) extends Rule
  case class IsIn(values: Seq[String]) extends Rule
  case object NoRule extends Rule

  case class RuleOnColumn(header: CSV.Header, rule: Rule)
  case class Request(content: CSV.Content, selected: Seq[CSV.Header], rulesOnColumns: Seq[RuleOnColumn])

  implicit class RuleOnColumnDecorator(ruleOnColumn: RuleOnColumn) {
    def compute(content: CSV.Content) = {
      val headerIndex = CSV.columnIndex(ruleOnColumn.header, content)
      content.copy(fields =
        content.fields.filter {line=>
          val valueToBeTested = line(headerIndex)
          ruleOnColumn.rule match {
            case sa: SuchAs=>  sa.operator(valueToBeTested)
            case iin: IsIn=> iin.values.contains(valueToBeTested)
            case _=> true
          }
        }
      )
    }
  }

  implicit class ContentDecorator(content: CSV.Content) {
    def column(columnName: String) = CSV.column(columnName, content)
    def select(headers: CSV.Header*) = Request(content, headers, Seq())
  }

  implicit class RequestDecorator(request: Request) {

    def where(header: CSV.Header, rule: Rule) = request.copy(rulesOnColumns = request.rulesOnColumns :+ RuleOnColumn(header, rule))
    def quest = {
      def quest0(content: CSV.Content, rulesToBeApplied: Seq[RuleOnColumn]): CSV.Content = {
        if (rulesToBeApplied.isEmpty) content
        else quest0(rulesToBeApplied.head.compute(content), rulesToBeApplied.tail)
      }

      CSV.linesWhere(request.selected, quest0(request.content, request.rulesOnColumns))
    }
  }

}
