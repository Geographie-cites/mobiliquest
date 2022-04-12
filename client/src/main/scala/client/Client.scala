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
import shared.data.Study

@JSExportTopLevel(name = "mobiliquest")
@JSExportAll
object App {

  //def main(args: Array[String]): Unit = {

  def gui() = {

    def indicatorsUI(study: Study) = RequestForm.indicatorUIs(study)

    val studiesUI = RequestForm.studyUI
    val currentIndicatorsUI: Var[Seq[RequestForm.IndicatorUI]] = Var(Seq())

    def indicatorsUIToRequest(indicatorsUI: Seq[IndicatorUI]) = {
      indicatorsUI.map{iUI=>
        iUI.indicatorAndModalities
      }.toMap
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
          val request = data.Request(RequestForm.currentStudy.now(), indicatorsUIToRequest(currentIndicatorsUI.now()))
          Post[shared.Api].run(request).call().foreach { x =>
            println("X " + x)

          }
        })
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
