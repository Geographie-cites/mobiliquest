package client

import org.scalajs.dom
import shared.Utils
import shared.data.*

//import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import scaladget.bootstrapnative.bsn._

import com.raquo.laminar.api.L._
import client.RequestForm._
//import shared.data

@JSExportTopLevel(name = "mobiliquest")
@JSExportAll
object App {

  //def main(args: Array[String]): Unit = {

  def gui() = {

    def indicatorsUI(study: Study, requestType: RequestType) = RequestForm.indicatorUIs(study, requestType)

    val studiesUI = RequestForm.studyUI
    val requestType: Var[RequestType] = Var(SubPop())
    val requestStatus: Var[RequestStatus] = Var(shared.data.Off)

    def indicatorsUIToRequest(indicatorsUI: Seq[IndicatorUI]) =
      indicatorsUI.map { i =>
        i.indicatorAndModalities._1 -> Utils.flatten(i.indicatorAndModalities._2)
      }

    val subPopState = ToggleState("Sup population", btn_primary_string, () => requestType.set(SubPop()))
    val perimeterState = ToggleState("Perimter", btn_primary_string, () => requestType.set(Perimeter()))
    val requestSelector = exclusiveRadio(Seq(subPopState, perimeterState), btn_secondary_string, subPopState)

    val content =
      div(
        h1("Mobiliquest !"),
        div(display.flex, flexDirection.row, justifyContent.spaceAround, width := "1000",
          margin := "10",
          studiesUI.selector,
          requestSelector,
          button("Run", btn_primary_outline, onClick --> { _ =>
            import scala.concurrent.ExecutionContext.Implicits.global

            requestStatus.set(shared.data.Running)
            val request = shared.data.Request(RequestForm.currentStudy.now(), indicatorsUIToRequest(currentIndicatorsUI.now()), requestType.now())

            APIClient.rExecution(request).future.onComplete {
              _.foreach(rr => requestStatus.set(Done(rr)))
            }
          },
            disabled <-- requestStatus.signal.map {
              _ match {
                case Running => true
                case _ => false
              }
            }
          ),
          children <-- requestStatus.signal.map { x =>
            x match {
              case shared.data.Done(r) =>
                val filterURL = r.filterURL.getOrElse("?")
                Seq(span("FIXME: nbRec", margin := "0 10 0 10"), a("filters.json", href := filterURL, target := "_blank"))
              case _ => Seq()
            }
          }
        ),
        child <-- RequestForm.currentStudy.signal.combineWith(requestType.signal).map { case (cs, rt) =>
          val newIndUI = indicatorsUI(cs, rt).toSeq
          currentIndicatorsUI.set(newIndUI)
          div(rowFlex, margin := "30px",
            newIndUI.map { iui => iui.content }
          )
        }
      )

    val containerNode = dom.document.querySelector("#mobiliquest-content")
    render(containerNode, content)
  }
}
