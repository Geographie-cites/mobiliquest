package mobiliquest

import org.http4s.StaticFile
import shared.data.*
import org.http4s._
import org.http4s.dsl.io._

object R {
  lazy val R = org.ddahl.rscala.RClient()
  lazy val api = scala.io.Source.fromResource("routines/p2m_fct.R").getLines().mkString("\n")

  def filterFullModalities(study: Study, indicator: Indicator, flattenModalities: Seq[Modality]) =
    if (flattenModalities.length == shared.Utils.flatten(Indicators.availableIndicatorsAndModalities(study)(indicator)).length) Seq()
    else flattenModalities

  def computeAll(request: shared.data.Request, inputDir: Directory, outputDir: Directory) = {

    println("compute all " + request.study)
    val Rfilters = "list(" + request.filters.map {
      case (indicator, modalities) =>
        // An indicator with all modalities should not be consider as a filter
        s""" "${indicator.RName}" = c(${modalities.mkString(", ")}) """
    }.mkString(",") + ")"

    def indicatorList(rType: RequestType) =
      if (rType == request.requestType) Rfilters
      else "list()"

    val call = s"""\np2m("${request.study}", ${indicatorList(Perimeter())}, ${indicatorList(SubPop())}, "$inputDir", "$outputDir")"""
    println("call :  " + call)
    R.evalL0(api + call)
  }
}
