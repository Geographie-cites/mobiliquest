package client

import shared.data
import shared.data._
import scaladget.bootstrapnative.bsn._
import com.raquo.laminar.api.L._
import scaladget.bootstrapnative.Selector.Options

object RequestForm {

  val studies = data.Indicators.availableIndicatorsAndModalities.keys.toSeq.sorted
  val currentStudy: Var[Study] = Var(studies.head)
  val currentIndicatorsAndModalities: Var[IndicatorAndModalities] = Var(Map())
  val rowFlex = Seq(display.flex, flexDirection.row)
  val columnFlex = Seq(display.flex, flexDirection.column)


  def unactivateState(modalityName: String) = ToggleState(modalityName, btn_outline_secondary_string)

  def toggleOn(modalityName: String, onColor: String) = ToggleState(modalityName, onColor)

  def modalityButton(modalityName: String): ToggleButtonState = toggle(toggleOn(modalityName, btn_primary_string), true, unactivateState(modalityName), withCaret = false)

  class IndicatorUI(study: Study, indicator: Indicator, availableModalities: Modalities) {

    private val modMap = indicator.modalityDescriptions.toMap

    private val toggleButtonStates = availableModalities.map {
      _ match {
        case Left(m: Modality) => modalityButton(modMap(m))
        case Right(sOfM: Seq[Modality]) => modalityButton(sOfM.map {
          modMap(_)
        }.mkString(" et "))
      }
    }

    def content =
      div(columnFlex, alignItems.center,
        div(indicator.RName, cls := "indicatorName"),
        div(indicator.description, cls := "indicatorDescription"),
        div(columnFlex, toggleButtonStates.map {
          _.element.amend(cls := "indicatorButton")
        }
        )
      )

    def indicatorAndModalities: (Indicator, Modalities) = {
      val selectedModalities = toggleButtonStates.zip(availableModalities).filter { case (tbs, _) =>
        tbs.toggled.now()
      }.map {
        _._2
      }
      (indicator, selectedModalities)
    }
  }

  def indicatorUIs(study: Study, requestType: RequestType) = {
    val indicatorAndModalities = data.Indicators.availableIndicatorsAndModalities(study).filter { ii =>
      ii._1.requestType == requestType
    }
    indicatorAndModalities.map { iam => new IndicatorUI(study, iam._1, iam._2) }.toSeq
  }

  lazy val studyUI: Options[Study] = studies.options(
    key = btn_danger,
    naming = (s: Study) => s,
    onclose = () => currentStudy.set(studyUI.content.now().getOrElse(studies.head))
  )

}
