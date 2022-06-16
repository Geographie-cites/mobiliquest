package mobiliquest

import mobiliquest.R.filterFullModalities
import shared.data
import shared.data.{Indicators, RequestResponse, emptyResponse}
import better.files._

import java.nio.file.Files
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits._

object ApiImpl extends shared.Api {

  def run(request: data.Request): RequestResponse = {
    val cleanRequest = request.copy(
      filters = request.filters.map { case (i, m) => i -> Seq(Right(filterFullModalities(request.study, i, Utils.flatten(m)))) },
      perimModalities = Seq(Right(filterFullModalities(request.study, Indicators.perimetre, Utils.flatten(request.perimModalities))))
    )
    val cleanNameStudy = request.study.toLowerCase

    FileService.withPresenceUtile[RequestResponse] { inputDir =>
      FileService.withTmpDir[RequestResponse] { outputDir =>
        val rFuture = ThreadService.submit(() => R.computeAll(cleanRequest, inputDir.pathAsString, outputDir.pathAsString))
        val nbRecords = Await.result(rFuture, 1 hour)

        if (nbRecords > 0) {
          Serializer.toJson(cleanRequest, (s"$outputDir/filters.json").toFile)
          //FIXME: replace request.study by hash
          FileService.uploadFiles(outputDir.listRecursively.toSeq, outputDir.pathAsString, cleanNameStudy)
          RequestResponse(Some(nbRecords), Some(FileService.getURL(cleanNameStudy, "filters.json")))
        } else emptyResponse
      }
    }
  }

}
