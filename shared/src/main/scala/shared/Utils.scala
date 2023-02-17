package shared

import shared.data.{Modalities, Modality}

object Utils {
  def flatten(modalities: Modalities): Seq[Modality] =
    modalities.map {
      _ match {
        case Left(m: Modality) => Seq(m)
        case Right(sm: Seq[Modality]) => sm
      }
    }.flatten.distinct
}
