package client

import java.nio.ByteBuffer
import org.scalajs.dom

import scala.concurrent.Future
import boopickle.Default._
import org.scalajs

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import com.raquo.laminar.api.L._

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel (name="mobiliquest")
@JSExportAll
object App {

  //def main(args: Array[String]): Unit = {

  def gui() = {
    val content = h1("Mobiliquest !")

    val containerNode = dom.document.querySelector("#mobiliquest-content")

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
