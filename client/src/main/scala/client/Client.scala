package client

import java.nio.ByteBuffer
import org.scalajs.dom

import scala.concurrent.Future
import boopickle.Default._
import org.scalajs

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import scaladget.bootstrapnative.bsn._
import scaladget.tools._
import com.raquo.laminar.api.L._

import scala.concurrent.ExecutionContext.Implicits.global
import autowire._
import boopickle.Default._
import client.RequestForm.{IndicatorUI, rowFlex}
import shared.data
import shared.data.{Running, Study}

@JSExportTopLevel(name = "mobiliquest")
@JSExportAll
object App {

  //def main(args: Array[String]): Unit = {

  def gui() = {

    def indicatorsUI(study: Study) = RequestForm.indicatorUIs(study)

    val studiesUI = RequestForm.studyUI
    val currentIndicatorsUI: Var[Seq[RequestForm.IndicatorUI]] = Var(Seq())
    val nbRequestedRecords: Var[data.RequestStatus] = Var(data.Off)

    def indicatorsUIToRequest(indicatorsUI: Seq[IndicatorUI]) = {
      val (perim, all) = indicatorsUI.partition(x => x.indicatorAndModalities._1 == data.Indicators.perimetre)
      (all.map { iUI =>
        iUI.indicatorAndModalities
      }.toMap, perim.head.indicatorAndModalities._2)
    }

    val content =
      div(
        margin := "10",
        h1("Mobiliquest !"),
        studiesUI.selector,
        child <-- RequestForm.currentStudy.signal.map { cs =>
          val newIndUI = indicatorsUI(cs)
          currentIndicatorsUI.set(newIndUI)
          div(rowFlex, marginTop := "30px",
            newIndUI.map { iui => iui.content }
          )
        },
        button("Run", btn_primary_outline, onClick --> { _ =>
          nbRequestedRecords.set(data.Running)
          val (all, perimModalities) = indicatorsUIToRequest(currentIndicatorsUI.now())
          val request = data.Request(RequestForm.currentStudy.now(), perimModalities, all)
          Post[shared.Api].run(request).call().foreach { x =>
            nbRequestedRecords.set(data.Done(x))
          }
        },
          disabled <-- nbRequestedRecords.signal.map {
            _ match {
              case Running => true
              case _ => false
            }
          }
        ),
        child <-- nbRequestedRecords.signal.map { x =>
          div(
            x match {
              case data.Done(r) => r.toString
              case _ => ""
            },
            marginLeft := "10"
          )
        })

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
