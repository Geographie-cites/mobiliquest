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

  def flatten(modalities: Modalities): Seq[Modality] =
    modalities.map {
      _ match {
        case Left(m: Modality) => Seq(m)
        case Right(sm: Seq[Modality]) => sm
      }
    }.flatten.distinct

  def filterFullModalities(study: Study, indicator: Indicator, flattenModalities: Seq[Modality]) =
    if (flattenModalities.length == flatten(Indicators.availableIndicatorsAndModalities(study)(indicator)).length) Seq()
    else flattenModalities

  def computeAll(request: Request)(implicit inputDirectory: Directory, outputDirectory: Directory) = {

    val Rfilters = request.filters.map { case (i, m) => i -> filterFullModalities(request.study, i, flatten(m)) }.filterNot {
      _._2.isEmpty
    }.map { case (indicator, modalities) =>
      // An indicator with all modalities should not be consider as a filter
      s""" "${indicator.RName}" = c(${modalities.mkString(", ")}) """
    }.mkString(",")

    val call = s"""\np2m("${request.study}", c(${flatten(request.perimModalities).mkString(",")}), list($Rfilters), "$inputDir", "$outputDir")"""
    println("R call :: " + call)
    R.evalI0(api + call)
  }
}
