package mobiliquest

import java.io.File
import shared.data._

object R {
  val R = org.ddahl.rscala.RClient()

  //FIXME: should be given in args
  val inputDir = "/home/mathieu/Documents/Geocite/Mobiliscope/data/BD_presence_utile"
  val outputDir = "/tmp/mobiliquest/out/"

  new File(inputDir).mkdirs()
  new File(outputDir).mkdirs()

  val api = io.Source.fromResource("routines/p2m_fct_mobiQuest.R").getLines().mkString("\n")

  def computeAll(request: Request)(implicit inputDirectory: Directory, outputDirectory: Directory) = {

    def c(modalities: Modalities) = {
      if (modalities.isEmpty) "c()"
      else s""" c(${
        modalities.map {
          _ match {
            case Left(m: Modality) => Seq(m)
            case Right(sm: Seq[Modality])=> sm
          }
        }.flatten.mkString(", ")
      }
      )"""
    }

    val Rfilters = request.filters.map{case (indicator, modalities)=>
      s""" "${indicator.RName}" = ${c(modalities)} """
    }.mkString(",")

    val oo = s"""\np2m("${request.study}", ${c(request.perimModalities)}, list($Rfilters), "$inputDir", "$outputDir")"""
    println("R call :: " + oo)
    R.evalI0(api + oo)
  }
}
