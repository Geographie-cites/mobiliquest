package client

import shared.data
import shared.data.{Indicator, IndicatorAndModalities, Modality, Study}
import scaladget.bootstrapnative.bsn._
import com.raquo.laminar.api.L._
import scaladget.bootstrapnative.Selector.Options

object RequestForm {

  val studies = data.Indicators.availableIndicatorsAndModalities.keys.toSeq.sorted
  val currentStudy: Var[Study] = Var(studies.head)
  val rowFlex = Seq(display.flex, flexDirection.row)
  val columnFlex = Seq(display.flex, flexDirection.column)


  def unactivateState(modalityName: String) = ToggleState(modalityName, btn_outline_secondary_string)

  def toggleOn(modalityName: String, onColor: String) = ToggleState(modalityName, onColor)

  def modalityButton(modalityName: String): ToggleButtonState = toggle(toggleOn(modalityName, btn_primary_string), false, unactivateState(modalityName), withCaret = false)
  
  class IndicatorUI(indicator: Indicator, availableModalities: Seq[Modality]) {

    private val modMap = indicator.modalityDescriptions.toMap

    private val toggleButtonStates = availableModalities.map { am =>
      modalityButton(modMap(am))
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

    def indicatorAndModalities: (Indicator, Seq[Modality]) = {
      val modalities: Seq[Modality] = toggleButtonStates.zipWithIndex.filter { case (tbs, ind) =>
        tbs.toggled.now()
      }.map {
        _._2
      }
      (indicator, modalities)
    }
  }

  def indicatorUIs(indicatorAndModalities: IndicatorAndModalities) =
    indicatorAndModalities.map { iam => new IndicatorUI(iam._1, iam._2) }.toSeq

  lazy val studyUI: Options[Study] = studies.options(
    key = btn_danger,
    naming = (s: Study) => s,
    onclose = () => currentStudy.set(studyUI.content.now().getOrElse(studies.head))
  )

}
