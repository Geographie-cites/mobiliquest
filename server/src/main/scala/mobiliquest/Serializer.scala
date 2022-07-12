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
          (request.requestType.name -> request.filters.map { case (ind, mod) => ind.RName -> Utils.flatten(mod) })
        )

    val jsonText = pretty(render(json))
    target.overwrite(jsonText)
    Utils.hash(jsonText)
  }

}