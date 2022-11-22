package shared

import shared.data.RequestResponse
import endpoints4s.{algebra, circe}
import io.circe._
import io.circe.generic.auto.*

trait APIEndPoint
  extends algebra.Endpoints
    with algebra.circe.JsonEntitiesFromCodecs
    with circe.JsonSchemas {

  val rExecution: Endpoint[data.Request, data.RequestResponse] =
    endpoint(post(path / "r-execution", jsonRequest[data.Request]), ok(jsonResponse[data.RequestResponse]))
//
//  val test: Endpoint[data.Request, RequestResponse] =
//    endpoint(post(path / "test", jsonRequest[data.Request]), ok(jsonResponse[RequestResponse]))
}
