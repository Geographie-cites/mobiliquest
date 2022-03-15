package mobiliquest

import shared.Data._
import shared.Data

object ApiImpl extends shared.Api {

  val inputDir = "/home/mathieu/Documents/Geocite/Mobiliscope/data"
  val outputDir = "/tmp/mobiliquest/out"

  def run(request: Data.Request): String = {
    R.computeAll(request.study)(inputDir, outputDir)
    "Finished"
  }
}
