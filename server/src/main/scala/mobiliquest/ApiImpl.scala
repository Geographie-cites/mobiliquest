package mobiliquest

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
      filters = request.filters.map { case (i, m) => i -> Seq(Right(R.filterFullModalities(request.study, i, Utils.flatten(m)))) }
    )

    FileService.withPresenceUtile[RequestResponse] { inputDir =>
      FileService.withTmpDir[RequestResponse] { outputDir =>
        val hash = Serializer.toJson(cleanRequest, (s"$outputDir/filters.json").toFile)

        if (FileService.exists(hash)) {
          RequestResponse(None, Some(FileService.getURL(hash, "filters.json")))
        }
        else {
          val rFuture = ThreadService.submit(() => R.computeAll(cleanRequest, inputDir.pathAsString, outputDir.pathAsString))
          val nbRecords = Await.result(rFuture, 1 hour)

          if (nbRecords > 0) {
            FileService.uploadFiles(outputDir.listRecursively.toSeq, outputDir.pathAsString, hash)
            RequestResponse(Some(nbRecords), Some(FileService.getURL(hash, "filters.json")))
          } else emptyResponse
        }
      }
    }
  }

}
