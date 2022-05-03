package mobiliquest

import shared.data

object ApiImpl extends shared.Api {

  val inputDir = "/home/mathieu/Documents/Geocite/Mobiliscope/data/BD_presence_utile"
  val outputDir = "/tmp/mobiliquest/out"

  def run(request: data.Request): Int = {
    val nbRecords = R.computeAll(request)(inputDir, outputDir)
    if (nbRecords > 0) {
      Serializer.toJson(request, new java.io.File(outputDir + "/request.json"))
    }
    nbRecords
  }
}
