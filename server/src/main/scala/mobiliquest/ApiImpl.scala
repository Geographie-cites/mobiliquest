package mobiliquest

import mobiliquest.R.filterFullModalities
import shared.data
import shared.data.Indicators
import better.files._

object ApiImpl extends shared.Api {

  val inputDir = "/home/mathieu/Documents/Geocite/Mobiliscope/data/BD_presence_utile"
  val outputDir = "/tmp/mobiliquest/out"

  def run(request: data.Request): Int = {
    val cleanRequest = request.copy(
      filters = request.filters.map { case (i, m) => i -> Seq(Right(filterFullModalities(request.study, i, Utils.flatten(m)))) },
      perimModalities = Seq(Right(filterFullModalities(request.study, Indicators.perimetre, Utils.flatten(request.perimModalities))))
    )

    val nbRecords = R.computeAll(cleanRequest)(inputDir, outputDir)
    if (nbRecords > 0) {
      Serializer.toJson(cleanRequest, (s"$outputDir/${request.study}/filters.json").toFile)
    }
    nbRecords
  }

}
