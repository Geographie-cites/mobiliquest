package mobiliquest


import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import shared.data._

import better.files._

object Serializer {

  def toJson(request: Request, target: File) = {

    val json =
      ("request" ->
        ("study" -> request.study) ~
          ("perimeter" -> Utils.flatten(request.perimModalities)) ~
          ("filters" -> request.filters.map { case (ind, mod) => ind.RName -> Utils.flatten(mod) })
        )

    target.overwrite(pretty(render(json)))
  }

}