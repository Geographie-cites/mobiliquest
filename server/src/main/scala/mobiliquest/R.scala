package mobiliquest

import java.io.File
import shared.Data._

object R {
  val R = org.ddahl.rscala.RClient()

  //FIXME: should be given in args
  val inputDir = "/home/mathieu/Documents/Geocite/Mobiliscope/data"
  val outputDir = "/tmp/mobiliquest/out/"

  new File(inputDir).mkdirs()
  new File(outputDir).mkdirs()

  val api = io.Source.fromResource("routines/p2m_fct_mobiQuest.R").getLines().mkString("\n")

  def computeAll(study: Study)(implicit inputDirectory: Directory, outputDirectory: Directory) = {
    R.eval(api + s"""\np2m("$study", c(3,2), c("SEX", "2"), "$inputDir", "$outputDir")""")
  }
}
