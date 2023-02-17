package mobiliquest

import shared.data
import shared.data.{Indicators, RequestResponse, emptyResponse}
import better.files.*
import org.http4s.StaticFile

import java.nio.file.Files
import scala.concurrent.duration.*
import scala.language.postfixOps
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.*

object ApiImpl {

  def run(request: data.Request): RequestResponse = {
    println("RUN " + request.study)
    val cleanRequest = request.copy(
      filters = request.filters.map { case (i, m) => i -> R.filterFullModalities(request.study, i, m) }
    )

    println("RUN0 ")
    FileService.withSourceData[RequestResponse] { inputDir =>

      println("RUN01 ")
      FileService.withTmpDir[RequestResponse] { outputDir =>
        val hash = Serializer.toJson(cleanRequest, outputDir, "filters.json")
        val outputDirWithHash = s"$outputDir/$hash".toFile

        println("RUN02 ")
        if (FileService.existsDir(FileService.outData, hash)) {
          val url = FileService.getURL(FileService.outData, s"$hash/filters.json")
          RequestResponse(Some(FileService.getURL(FileService.outData, s"$hash/filters.json")), None /*Some(FileService.getURL(hash, "stat.json"))*/)
        }
        else {
          val rFuture = ThreadService.submit(() => R.computeAll(cleanRequest, inputDir.pathAsString, outputDirWithHash.pathAsString))
          val valid = Await.result(rFuture, 1 hour)

          println("RUN03 ")
          if (valid) {
            FileService.uploadOutData(outputDirWithHash.listRecursively.toSeq, outputDirWithHash.pathAsString, hash)
            RequestResponse(Some(FileService.getURL(FileService.outData, s"$hash/filters.json")), None /*Some(FileService.getURL(FileService.outData,s"$hash/"stat.json"))*/)
          } else emptyResponse
        }
      }
    }
  }

}
