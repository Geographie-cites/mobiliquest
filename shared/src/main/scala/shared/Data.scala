package shared


object data {

  type Study = String
  type Directory = String
  type Modality = Int
  type RequestID = String

  case class Indicator(RName: String, description: String, modalityDescriptions: Seq[(Modality, String)])

  type IndicatorAndModalities = Map[Indicator, Seq[Modality]]


  case class Request(study: Study, filters: IndicatorAndModalities)


  object Indicators {

    val kAge = Indicator(
      "KAGE",
      "Classe d'âge",
      Seq(
        0 -> "moins de 16 ans",
        1 -> "16-24 ans",
        2 -> "25-34 ans",
        3 -> "35-64 ans",
        4 -> "65 ans et plus"
      )
    )

    val sex = Indicator(
      "SEX",
      "Sexe",
      Seq(
        1 -> "Homme",
        2 -> "Femme"
      )
    )

    val strM = Indicator(
      "STRM",
      "Structure du ménage",
      Seq(
        1 -> "Ménage d'une personne",
        2 -> "Couple sans enfant",
        3 -> "Ménage (hors couple) sans enfant",
        4 -> "Ménage avec enfant"
      )
    )

    val educ = Indicator(
      "EDUC",
      "Niveau d'éducation",
      Seq(
        1 -> "Faible niveau",
        2 -> "Niveau intermédiaire",
        3 -> "Niveau élevé",
        4 -> "Niveau très élevé"
      )
    )

    val educMen = Indicator(
      "EDUCMEN",
      "Niveau d'éducation le plus bas des adultes du ménage",
      Seq(
        1 -> "Faible niveau",
        2 -> "Niveau intermédiaire",
        3 -> "Niveau élevé",
        4 -> "Niveau très élevé"
      )
    )

    val rev = Indicator(
      "REV",
      "Revenus du ménage",
      Seq(
        1 -> "Faible revenu",
        2 -> "Revenu intermédiaire - tranche inférieure",
        3 -> "Revenu intermédiaire - tranche supérieure",
        4 -> "Revenu élevé"
      )
    )

    val csp = Indicator(
      "CSP",
      "Classe socio-professionnelle",
      Seq(
        1 -> "Inactif",
        2 -> "Ouvrier",
        3 -> "Employé",
        4 -> "Intermédiaire",
        5 -> "Cadres et professions intellectuelles"
      )
    )

    val cspMen = Indicator(
      "CSPMEN",
      "Classe socio-professionnelle la plus basse des adultes du ménage",
      Seq(
        1 -> "Inactif",
        2 -> "Ouvrier",
        3 -> "Employé",
        4 -> "Intermédiaire",
        5 -> "Cadres et professions intellectuelles"
      )
    )

    val occ = Indicator(
      "OCC",
      "Statut d'occupation",
      Seq(
        1 -> "Actif",
        2 -> "Étudiant",
        3 -> "Sans emploi",
        4 -> "Retraité",
        5 -> "Inactif"
      )
    )

    val dep = Indicator(
      "DEP",
      "Département",
      Seq(
        1 -> "Paris",
        2 -> "Seine-Saint-Denis",
        3 -> "Val-de-Marne",
        4 -> "Haut-de-Seine",
        5 -> "Grande couronne"
      )
    )

    val zonage = Indicator(
      "ZONAGE",
      "Zone du secteur de résidence",
      Seq(
        1 -> "Résident dans la ville centre",
        2 -> "Résident en zone urbaine",
        3 -> "Résident en zone périphérique"
      )
    )

    val qpv = Indicator(
      "QPV",
      "Zone fine de résidence",
      Seq(
        0 -> "Hors QPV",
        1 -> "en QPV"
      )
    )

    val basic: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM -> Seq(1, 2, 3, 4),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      csp -> Seq(1, 2, 3, 4, 5),
      cspMen -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3),
      qpv -> Seq(0, 1)
    )

    val annecy: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM -> Seq(1, 2, 3, 4),
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
      strM -> Seq(1, 2, 3, 4),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      csp -> Seq(1, 2, 3, 4, 5),
      cspMen -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2),
      qpv -> Seq(0, 1)
    )

    val idf: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM -> Seq(1, 2, 3, 4),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev -> Seq(1, 2, 3, 4),
      csp -> Seq(1, 2, 3, 4, 5),
      cspMen -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5),
      dep -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3),
      qpv -> Seq(0, 1)
    )


    val availableIndicatorsAndModalities: Map[Study, IndicatorAndModalities] = Map(
      "ALBI" -> basic,
      "ALENCON" -> basic,
      "AMIENS" -> basic,
      "ANNECY" -> annecy,
      "ANGERS" -> basic,
      "ANGOULEME" -> basic,
      "ANNEMASSE" -> basic,
      "BAYONNE" -> basic,
      "BESANCON" -> besCarc,
      "BEZIERS" -> basic,
      "BORDEAUX" -> basic,
      "BREST" -> basic,
      "CAEN" -> basic,
      "CARCASSONNE" -> besCarc,
      "CHERBOURG" -> basic,
      "CLERMONT FERRAND" -> basic,
      "CREIL" -> basic,
      "DIJON" -> basic,
      "DOUAI" -> basic,
      "DUNKERQUE" -> basic,
      "GRENOBLE" -> basic,
      "IDF" -> idf,
      "LA REUNION" -> basic,
      "LA ROCHELLE" -> basic,
      "LE HAVRE" -> basic,
      "LILLE" -> basic,
      "LONGWY" -> basic,
      "LYON" -> basic,
      "MARSEILLE" -> basic,
      "MARTINIQUE" -> basic,
      "METZ" -> basic,
      "MONTPELLIER" -> basic,
      "NANCY" -> basic,
      "NANTES" -> basic,
      "NICE" -> basic,
      "NIMES" -> basic,
      "NIORT" -> basic,
      "POITIERS" -> basic,
      "QUEBEC" -> basic,
      "QUIMPER" -> basic,
      "RENNES" -> basic,
      "ROUEN" -> basic,
      "SAINT BRIEUC" -> basic,
      "SAINT ETIENNE" -> basic,
      "STRASBOURG" -> basic,
      "THIONVILLE" -> basic,
      "TOULOUSE" -> basic,
      "TOURS" -> basic,
      "VALENCE" -> basic,
      "VALENCIENNES" -> basic
    )
  }

}