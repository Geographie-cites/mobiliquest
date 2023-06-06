package mobiliquest

import cats.effect.*
import cats.syntax.all.*
import org.http4s.server.blaze.*
import org.http4s.implicits.*
import org.http4s.server.Router
import cats.effect.unsafe.implicits.global

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration.Duration

object Main extends IOApp {

  lazy val httpApp = Router("/" -> RootPage.routes, "/" -> APIServer.routes /*, "/" -> DocumentationServer.routes*/).orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](scala.concurrent.ExecutionContext.global)
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(httpApp)
      .withIdleTimeout(Duration.Inf)
      .resource
      .useForever
      .as(ExitCode.Success)
}

