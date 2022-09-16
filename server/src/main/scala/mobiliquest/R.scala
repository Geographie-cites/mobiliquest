package mobiliquest

import shared.data._

object R {
  val R = org.ddahl.rscala.RClient()

  val api = scala.io.Source.fromResource("routines/p2m_fct.R").getLines().mkString("\n")

  def filterFullModalities(study: Study, indicator: Indicator, flattenModalities: Seq[Modality]) =
    if (flattenModalities.length == Utils.flatten(Indicators.availableIndicatorsAndModalities(study)(indicator)).length) Seq()
    else flattenModalities

  def computeAll(request: Request, inputDir: Directory, outputDir: Directory) = {

    val Rfilters = "list(" + request.filters.map {
      case (indicator, modalities) =>
        // An indicator with all modalities should not be consider as a filter
        s""" "${indicator.RName}" = c(${Utils.flatten(modalities).mkString(", ")}) """
    }.mkString(",") + ")"

    def indicatorList(rType: RequestType) =
      if (rType == request.requestType) Rfilters
      else "list()"

    val call = s"""\np2m("${request.study}", ${indicatorList(Perimeter())}, ${indicatorList(SubPop())}, "$inputDir", "$outputDir")"""
    R.evalL0(api + call)
  }
}
