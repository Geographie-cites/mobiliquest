package mobiliquest


import cats.effect.*
import endpoints4s.http4s.server
import org.http4s.*
import shared.data.RequestResponse

/** Defines a Play router (and reverse router) for the endpoints described
 * in the `CounterEndpoints` trait.
 */
object APIServer
  extends server.Endpoints[IO]
    with shared.APIEndPoint
    with server.JsonEntitiesFromCodecs {

  lazy val rExecutionRoute = rExecution.implementedBy(request => ApiImpl.run(request))

  lazy val routes: HttpRoutes[IO] = HttpRoutes.of(
    routesFromEndpoints(rExecutionRoute)
  )

}
