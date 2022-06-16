package mobiliquest

import shared.data._

object R {
  val R = org.ddahl.rscala.RClient()

  val api = scala.io.Source.fromResource("routines/p2m_fct_mobiQuest.R").getLines().mkString("\n")

  def filterFullModalities(study: Study, indicator: Indicator, flattenModalities: Seq[Modality]) =
    if (flattenModalities.length == Utils.flatten(Indicators.availableIndicatorsAndModalities(study)(indicator)).length) Seq()
    else flattenModalities

  def computeAll(request: Request, inputDir: Directory, outputDir: Directory) = {

    val Rfilters = request.filters.map {
      case (indicator, modalities) =>
        // An indicator with all modalities should not be consider as a filter
        s""" "${indicator.RName}" = c(${Utils.flatten(modalities).mkString(", ")}) """
    }.mkString(",")

    val call = s"""\np2m("${request.study}", c(${Utils.flatten(request.perimModalities).mkString(",")}), list($Rfilters), "$inputDir", "$outputDir")"""
    println("R call :: " + call)
    R.evalI0(api + call)
  }
}
