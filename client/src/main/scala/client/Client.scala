package client

import java.nio.ByteBuffer
import org.scalajs.dom

import scala.concurrent.Future
import boopickle.Default._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import scaladget.bootstrapnative.bsn._
import com.raquo.laminar.api.L._

import scala.concurrent.ExecutionContext.Implicits.global
import autowire._
import boopickle.Default._
import client.RequestForm._
import shared.data
import shared.data._

@JSExportTopLevel(name = "mobiliquest")
@JSExportAll
object App {

  //def main(args: Array[String]): Unit = {

  def gui() = {

    def indicatorsUI(study: Study, requestType: RequestType) = RequestForm.indicatorUIs(study, requestType)

    val studiesUI = RequestForm.studyUI
    val requestType: Var[RequestType] = Var(SubPop())
    val requestStatus: Var[data.RequestStatus] = Var(data.Off)

    def indicatorsUIToRequest(indicatorsUI: Seq[IndicatorUI]) = {
      indicatorsUI.map {
        _.indicatorAndModalities
      }.toMap
    }

    val subPopState = ToggleState("Sup population", btn_primary_string, () => requestType.set(SubPop()))
    val perimeterState = ToggleState("Perimeter", btn_primary_string, () => requestType.set(Perimeter()))
    val requestSelector = exclusiveRadio(Seq(subPopState, perimeterState), btn_secondary_string, subPopState)

    val content =
      div(
        h1("Mobiliquest !"),
        div(display.flex, flexDirection.row, justifyContent.spaceAround, width := "1000",
          margin := "10",
          studiesUI.selector,
          requestSelector,
          button("Run", btn_primary_outline, onClick --> { _ =>
            requestStatus.set(data.Running)
            val request = data.Request(RequestForm.currentStudy.now(), indicatorsUIToRequest(currentIndicatorsUI.now()), requestType.now())
            Post[shared.Api].run(request).call().foreach { x =>
              requestStatus.set(data.Done(x))
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
              case data.Done(r) =>
                val filterURL = r.filterURL.getOrElse("?")
                Seq(span("FIXME: nbRec", margin := "0 10 0 10"), a("filters.json", href := filterURL, target := "_blank"))
              case _ => Seq()
            }
          }
        ),
        child <-- RequestForm.currentStudy.signal.combineWith(requestType.signal).map { case (cs, rt) =>
          val newIndUI = indicatorsUI(cs, rt)
          currentIndicatorsUI.set(newIndUI)
          div(rowFlex, margin := "30px",
            newIndUI.map { iui => iui.content }
          )
        }
      )

    val containerNode = dom.document.querySelector("#mobiliquest-content")


    RequestForm
    render(containerNode, content)
  }
}

object Post extends autowire.Client[ByteBuffer, Pickler, Pickler] {

  override def doCall(req: Request): Future[ByteBuffer] = {
    dom.ext.Ajax.post(
      url = req.path.mkString("/"),
      data = Pickle.intoBytes(req.args),
      responseType = "arraybuffer",
      headers = Map("Content-Type" -> "application/octet-stream")
    ).map(r => TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer]))
  }

  override def read[Result: Pickler](p: ByteBuffer) = Unpickle[Result].fromBytes(p)

  override def write[Result: Pickler](r: Result) = Pickle.intoBytes(r)

}
