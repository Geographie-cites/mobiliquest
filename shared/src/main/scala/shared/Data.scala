package shared

import scala.scalajs.js.|


object data {

  type Study = String
  type Directory = String
  type Modality = Int
  type RequestID = String
  type Modalities = Seq[Either[Modality, Seq[Modality]]]

  sealed trait RequestType {
    def name: String
  }

  case class SubPop() extends RequestType {
    def name = "subpop"
  }

  case class Perimeter() extends RequestType {
    def name = "perimeter"
  }

  case class Indicator(RName: String, description: String, modalityDescriptions: Seq[(Modality, String)], requestType: RequestType)

  type IndicatorAndModalities = Map[Indicator, Modalities]

  implicit def intToLeftModality(m: Int): Left[Modality, Seq[Modality]] = Left(m)

  implicit def seqOfIntToLeftModality(ms: Seq[Int]): Right[Modality, Seq[Modality]] = Right(ms)

  // implicit def ModalityToLeftModality(m: Seq[Modality]): Seq[Left[Modality, Seq[Modality]]] = m.map{Left(_)}

  implicit def ModalityToRightModality(m: Seq[Seq[Modality]]): Seq[Right[Modality, Seq[Modality]]] = m.map{Right(_)}

  case class Request(study: Study, filters: IndicatorAndModalities, requestType: RequestType)

  case class RequestResponse(filterURL: Option[String], statURL: Option[String])

  val emptyResponse = RequestResponse(None, None)

  sealed trait RequestStatus

  case object Off extends RequestStatus

  case object Running extends RequestStatus

  case class Done(requestResponse: RequestResponse) extends RequestStatus

  object Indicators {

    val perimetre = Indicator(
      "PERIM",
      "Perimeter according to urban area zoning",
      Seq(
        1 -> "Peripheral areas",
        2 -> "Urban areas",
        3 -> "Inner city",
        4 -> "Distant periphery",
        5 -> "Close periphery",
        6 -> "Pericenter",
        7 -> "Center"
      ),
      Perimeter()
    )

    val kAge = Indicator(
      "KAGE",
      "Age groups",
      Seq(
        0 -> "15 and less",
        1 -> "16-24",
        2 -> "25-34",
        3 -> "35-64",
        4 -> "65 and more"
      ),
      SubPop()
    )

    val sex = Indicator(
      "SEX",
      "Sex",
      Seq(
        1 -> "Male",
        2 -> "Female"
      ),
      SubPop()
    )

    val strM_fr = Indicator(
      "STRM",
      "Household composition",
      Seq(
        1 -> "Single-person household",
        2 -> "Couple without children",
        3 -> "Household (excluding couple) without children",
        4 -> "Household with children"
      ),
      SubPop()
    )

    val strM_ca = Indicator(
      "STRM",
      "Household composition",
      Seq(
        1 -> "Single-person household",
        2 -> "Household without children",
        3 -> "Household with children"
      ),
      SubPop()
    )

    val strM_al = Indicator(
      "STRM",
      "Household composition",
      Seq(
        1 -> "Single-person household",
        2 -> "Family without children",
        3 -> "Complex household without children",
        4 -> "Family with children",
        5 -> "Complex household with children"
      ),
      SubPop()
    )

    val educ = Indicator(
      "EDUC",
      "Educational level (individual)",
      Seq(
        1 -> "Low",
        2 -> "Intermediate",
        3 -> "High",
        4 -> "Very high"
      ),
      SubPop()
    )

    val educMen = Indicator(
      "EDUCMEN",
      "Educational level (household)",
      Seq(
        1 -> "Low",
        2 -> "Intermediate",
        3 -> "High",
        4 -> "Very high"
      ),
      SubPop()
    )

    val rev = Indicator(
      "REV",
      "Revenu du ménage",
      Seq(
        1 -> "Faible",
        2 -> "Intermédiaire - Tranche inférieure",
        3 -> "Intermédiaire - Tranche supérieure",
        4 -> "Élevé",
        5 -> "Inconnu"
      ),
      SubPop()
    )

    val rev_al = Indicator(
      "REV",
      "Household income",
      Seq(
        1 -> "Very low",
        2 -> "Low",
        3 -> "Intermediate",
        4 -> "High",
        5 -> "Very high"
      ),
      SubPop()
    )

    val cso = Indicator(
      "CSO",
      "Socioprofessional status",
      Seq(
        1 -> "Unskilled workers",
        2 -> "Skilled workers",
        3 -> "Self-employed",
        4 -> "Executives and professionals"
      ),
      SubPop()
    )

    val inf = Indicator(
      "INFORMAL",
      "Professional informality",
      Seq(
        1 -> "Formal workers",
        2 -> "Informal workers",
      ),
      SubPop()
    )

    val sse = Indicator(
      "SSE",
      "Socio-economic stratum of residence",
      Seq(
        1 -> "Stratum 1 or not stratified",
        2 -> "Stratum 2",
        3 -> "Stratum 3",
        4 -> "Stratum 4, 5 or 6"
      ),
      SubPop()
    )

    val log = Indicator(
      "LOG",
      "Housing tenure",
      Seq(
        1 -> "Rent-free",
        2 -> "Tenants",
        3 -> "Owners"
      ),
      SubPop()
    )

    val csp = Indicator(
      "CSP",
      "Socioprofessional status (individual)",
      Seq(
        1 -> "Inactive",
        2 -> "Workers",
        3 -> "Employees",
        4 -> "Intermediate occupants",
        5 -> "Managers and intellectual professionals"
      ),
      SubPop()
    )

    val cspMen = Indicator(
      "CSPMEN",
      "Socioprofessional status (household)",
      Seq(
        1 -> "Inactive",
        2 -> "Workers",
        3 -> "Employees",
        4 -> "Intermediate occupants",
        5 -> "Managers and intellectual professionals"
      ),
      SubPop()
    )

    val occ = Indicator(
      "OCC",
      "Occupational status",
      Seq(
        1 -> "Active",
        2 -> "Student",
        3 -> "Unemployed",
        4 -> "Retired",
        5 -> "Inactive"
      ),
      SubPop()
    )

    val dep = Indicator(
      "DEP",
      "Département de résidence",
      Seq(
        1 -> "Paris",
        2 -> "Seine-Saint-Denis",
        3 -> "Val-de-Marne",
        4 -> "Haut-de-Seine",
        5 -> "Greater Paris"
      ),
      SubPop()
    )

    val zonage = Indicator(
      "ZONAGE",
      "Residential location in the urban/peripheral rings",
      Seq(
        1 -> "Peripheral areas",
        2 -> "Urban areas",
        3 -> "Inner city",
        4 -> "Distant periphery",
        5 -> "Close periphery",
        6 -> "Pericenter",
        7 -> "Center"
      ),
      SubPop()
    )

    val qpv = Indicator(
      "QPV",
      "Residential location in/outside 'Poverty Areas' (QPV)",
      Seq(
        1 -> "Inside QPV",
        2 -> "Outside QPV"
      ),
      SubPop()
    )

    val basic: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_fr -> Seq(1, 2, 3, 4),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      csp -> Seq(1, 2, 3, 4, 5),
      cspMen -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3),
      qpv -> Seq(1, 2)
    )

    val annecy: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_fr -> Seq(1, 2, 3, 4),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      csp -> Seq(1, 2, 3, 4, 5),
      cspMen -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3)
    )

    val besCarc: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_fr -> Seq(1, 2, 3, 4),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      csp -> Seq(1, 2, 3, 4, 5),
      cspMen -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 3),
      qpv -> Seq(1, 2)
    )

    val idf: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_fr -> Seq(1, 2, 3, 4),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev -> Seq(1, 2, 3, 4),
      csp -> Seq(1, 2, 3, 4, 5),
      cspMen -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5),
      dep -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3),
      qpv -> Seq(1, 2)
    )

    val bogota: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_al -> Seq(1, 2, 3, 4, 5),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev_al -> Seq(1, 2, 3, 4, 5),
      cso -> Seq(1, 2, 3, 4),
      inf -> Seq(1, 2),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(4, 5, 6, 7),
      sse -> Seq(1, 2, 3, 4),
      log -> Seq(1, 2, 3)
    )

    val santiago: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_al -> Seq(1, 2, 3, 4, 5),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev_al -> Seq(1, 2, 3, 4, 5),
      cso -> Seq(1, 2, 3, 4),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(4, 5, 6, 7),
      log -> Seq(1, 2, 3)
    )

    val saopaulo: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_al -> Seq(1, 2, 3, 4, 5),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev_al -> Seq(1, 2, 3, 4, 5),
      cso -> Seq(1, 2, 3, 4),
      inf -> Seq(1, 2),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(4, 5, 6, 7),
      log -> Seq(1, 2, 3)
    )


    val canadians: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM_ca -> Seq(1, 2, 3),
      rev -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5)
    )

    val spatialFR: IndicatorAndModalities = Map(perimetre -> Seq(Seq(3, 2, 1), Seq(3, 2), Seq(3)))
    val spatialCaBe: IndicatorAndModalities = Map(perimetre -> Seq(Seq(3, 1), Seq(3)))
    val spatialAL: IndicatorAndModalities = Map(perimetre -> Seq(Seq(7, 6, 5, 4), Seq(7, 6, 5), Seq(7, 6)))
    val spatialNone: IndicatorAndModalities = Map(perimetre -> Seq())

    val availableIndicatorsAndModalities: Map[Study, IndicatorAndModalities] = Map(
      "albi" -> (basic ++ spatialFR),
      "alencon" -> (basic ++ spatialFR),
      "amiens" -> (basic ++ spatialFR),
      "annecy" -> (annecy ++ spatialFR),
      "angers" -> (basic ++ spatialFR),
      "angouleme" -> (basic ++ spatialFR),
      "annemasse" -> (basic ++ spatialFR),
      "bayonne" -> (basic ++ spatialFR),
      "besancon" -> (besCarc ++ spatialCaBe),
      "beziers" -> (basic ++ spatialFR),
      "bogota" -> (bogota ++ spatialAL),
      "bordeaux" -> (basic ++ spatialFR),
      "brest" -> (basic ++ spatialFR),
      "caen" -> (basic ++ spatialFR),
      "carcassonne" -> (besCarc ++ spatialCaBe),
      "cherbourg" -> (basic ++ spatialFR),
      "clermont-ferrand" -> (basic ++ spatialFR),
      "creil" -> (basic ++ spatialFR),
      "dijon" -> (basic ++ spatialFR),
      "douai" -> (basic ++ spatialFR),
      "dunkerque" -> (basic ++ spatialFR),
      "grenoble" -> (basic ++ spatialFR),
      "idf" -> (idf ++ spatialFR),
      "la-reunion" -> (basic ++ spatialFR),
      "la-rochelle" -> (basic ++ spatialFR),
      "le-havre" -> (basic ++ spatialFR),
      "lille" -> (basic ++ spatialFR),
      "longwy" -> (basic ++ spatialFR),
      "lyon" -> (basic ++ spatialFR),
      "marseille" -> (basic ++ spatialFR),
      "martinique" -> (basic ++ spatialFR),
      "metz" -> (basic ++ spatialFR),
      "montpellier" -> (basic ++ spatialFR),
      "montreal" -> (canadians ++ spatialFR),
      "nancy" -> (basic ++ spatialFR),
      "nantes" -> (basic ++ spatialFR),
      "nice" -> (basic ++ spatialFR),
      "nimes" -> (basic ++ spatialFR),
      "niort" -> (basic ++ spatialFR),
      "ottawa-gatineau" -> (canadians ++ spatialNone),
      "poitiers" -> (basic ++ spatialFR),
      "quebec" -> (basic ++ spatialFR),
      "quimper" -> (basic ++ spatialFR),
      "rennes" -> (basic ++ spatialFR),
      "rouen" -> (basic ++ spatialFR),
      "saguenay" -> (canadians ++ spatialNone),
      "saint-brieuc" -> (basic ++ spatialFR),
      "saint-etienne" -> (basic ++ spatialFR),
      "santiago" -> (santiago ++ spatialAL),
      "sao-paulo" -> (saopaulo ++ spatialAL),
      "sherbrook" -> (canadians ++ spatialFR),
      "strasbourg" -> (basic ++ spatialFR),
      "thionville" -> (basic ++ spatialFR),
      "toulouse" -> (basic ++ spatialFR),
      "tours" -> (basic ++ spatialFR),
      "trois-riviere" -> (canadians ++ spatialNone),
      "valence" -> (basic ++ spatialFR),
      "valenciennes" -> (basic ++ spatialFR)
    )
  }

}
