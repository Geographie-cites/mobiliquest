package mobiliquest

import cats.effect._
import cats.syntax.all.*
import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.Router
import cats.effect.unsafe.implicits.global


import scala.concurrent.ExecutionContext.global

object Main extends IOApp {

  lazy val httpApp = Router("/" -> RootPage.routes, "/" -> APIServer.routes /*, "/" -> DocumentationServer.routes*/).orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
      .bindHttp(8080, "localhost")
      .withHttpApp(httpApp)
      .resource
      .useForever
      .as(ExitCode.Success)
}

