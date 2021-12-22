package mobiliquest

object R {

  type Study = String
  type Directory = String

  val R = org.ddahl.rscala.RClient()

  val api = io.Source.fromResource("routines/api.R").getLines().mkString("\n")


  def computeAll(study: Study)(implicit inputDirectory: Directory, outputDirectory: Directory) = {
    val prezLongPath = s""""$inputDirectory/prezLong_$study.csv""""
    val sfSecPath = s""""$inputDirectory/sec_$study.shp""""
    val outputPath = s""""$outputDirectory/$study""""

    R.eval(api + s"""\ncomputeAll(4, "$study", $prezLongPath, $sfSecPath, $outputPath)""")
  }
}
