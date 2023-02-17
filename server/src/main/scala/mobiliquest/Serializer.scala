package mobiliquest


import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import shared.data._
import better.files._

object Serializer {

  def toJson(request: Request, target: File, fileName: String) = {

    val json =
      ("request" ->
        ("study" -> request.study) ~
          (request.requestType.name -> request.filters.map { case (ind, mod) => ind.RName -> mod })
        )

    val jsonText = pretty(render(json))
    val hash = Utils.hash(jsonText)
    val outPath = s"$target/$hash".toFile

    outPath.toJava.mkdir
    s"$outPath/$fileName".toFile.overwrite(jsonText)
    hash
  }

}