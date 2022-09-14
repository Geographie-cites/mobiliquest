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

    FileService.withSourceData[RequestResponse] { inputDir =>
      FileService.withTmpDir[RequestResponse] { outputDir =>
        val hash = Serializer.toJson(cleanRequest, outputDir, "filters.json")
        val outputDirWithHash = s"$outputDir/$hash".toFile

        if (FileService.existsDir(FileService.outData,hash)) {
          val url = FileService.getURL(FileService.outData, s"$hash/filters.json")
          RequestResponse(Some(FileService.getURL(FileService.outData, s"$hash/filters.json")), None/*Some(FileService.getURL(hash, "stat.json"))*/)
        }
        else {
          val rFuture = ThreadService.submit(() => R.computeAll(cleanRequest, inputDir.pathAsString, outputDirWithHash.pathAsString))
          val valid = Await.result(rFuture, 1 hour)

          if (valid) {
            FileService.uploadOutData(outputDirWithHash.listRecursively.toSeq, outputDirWithHash.pathAsString, hash)
            RequestResponse(Some(FileService.getURL(FileService.outData,s"$hash/filters.json")), None/*Some(FileService.getURL(FileService.outData,s"$hash/"stat.json"))*/)
          } else emptyResponse
        }
      }
    }
  }

}
