package mobiliquest

import org.http4s.HttpRoutes
import scalatags.Text.all._
import scalatags.Text.{all => tags}
import cats.effect._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`

object RootPage {

  lazy val routes: HttpRoutes[IO] = HttpRoutes.of {
    case GET -> Root =>
      val ht =
        tags.html(
          tags.head(
            tags.meta(tags.httpEquiv := "Content-Type", tags.content := "text/html; charset=UTF-8"),
            tags.link(tags.rel := "stylesheet", tags.`type` := "text/css", href := "css/styleMobiliquest.css"),
            tags.link(tags.rel := "stylesheet", tags.`type` := "text/css", href := "css/bootstrap.css"),
            tags.script(tags.`type` := "text/javascript", tags.src := "js/demo.js")
          ),
          body(
            tags.div(id := "mobiliquest-content"),
            tags.script("mobiliquest.gui();")
          )
        )
      Ok.apply(ht.render).map(_.withContentType(`Content-Type`(MediaType.text.html)))
    case request@GET -> Root / "js" / path =>
      import fs2.io.file.Path
      StaticFile.fromPath(Path(s"server/target/webapp/js/$path"), Some(request)).getOrElseF(NotFound())
    case request@GET -> Root / "css" / path =>
      import fs2.io.file.Path
      StaticFile.fromPath(Path(s"server/target/webapp/css/$path"), Some(request)).getOrElseF(NotFound())
  }
}

