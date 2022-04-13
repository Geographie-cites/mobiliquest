package shared


object data {

  type Study = String
  type Directory = String
  type Modality = Int
  type RequestID = String

  case class Indicator(RName: String, description: String, modalityDescriptions: Seq[(Modality, String)])

  type IndicatorAndModalities = Map[Indicator, Seq[Modality]]


  case class Request(study: Study, perimModalities: Seq[Modality], filters: IndicatorAndModalities)


  object Indicators {

    val perimetre = Indicator(
      "PERIM",
      "Zone du secteur de résidence",
      Seq(
        1 -> "Couronne des pôles urbains",
        2 -> "Pôles urbains",
        3 -> "Centre ville",
        4 -> "XXX"
      )
    )

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
        4 -> "Ménage avec enfant",
        5 -> "ménage complexe avec enfant"
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
        1 -> "Faible",
        2 -> "Intermédiaire - Tranche inférieure",
        3 -> "Intermédiaire - Tranche supérieure",
        4 -> "Élevé",
        5 -> "Inconnu"
      )
    )

    val cso = Indicator(
      "CSO",
      "",
      Seq(
        1 -> "Travailleurs non qualifiés",
        2 -> "Travailleurs qualifiés",
        3 -> "Indépendants",
        4 -> "Cadres et professions intellectuelles"
      )
    )

    val inf = Indicator(
      "INF",
      "Informalité professionnelle des actifs",
      Seq(
        1 -> "Actifs avec emploi formel",
        2 -> "Actifs avec emploi informel",
      )
    )

    val sse = Indicator(
      "SSE",
      "Strate socio économique",
      Seq(
        1 -> "Résident strate 1 ou non stratifiée",
        2 -> "Résident strate 2",
        3 -> "Résident strate 2",
        4 -> "Résident strate 4, 5 ou 6"
      )
    )

    val log = Indicator(
      "LOG",
      "Statut d'occupation du logement",
      Seq(
        1 -> "Personnes hébergées",
        2 -> "Locataires",
        3 -> "Propriétaires"
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
        1 -> "Résident de périphérie lointaine",
        2 -> "Résident de périphérie proche",
        3 -> "Résident du péricentre",
        4 -> "Résidents du centre"
      )
    )

    val qpv = Indicator(
      "QPV",
      "Zone fine de résidence",
      Seq(
        1 -> "Hors QPV",
        2 -> "en QPV"
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
      qpv -> Seq(1,2)
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
      qpv -> Seq(1,2)
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
      qpv -> Seq(1, 2)
    )

    val bogota: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM -> Seq(1, 2, 3, 4, 5),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev -> Seq(1, 2, 3, 4, 5),
      cso -> Seq(1, 2, 3, 4),
      inf-> Seq(1,2),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3, 4),
      sse -> Seq(1,2,3,4),
      log-> Seq(1,2,3)
    )

    val santiago: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM -> Seq(1, 2, 3, 4, 5),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev -> Seq(1, 2, 3, 4, 5),
      cso -> Seq(1, 2, 3, 4),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3, 4),
      log-> Seq(1,2,3)
    )

    val saopaulo: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM -> Seq(1, 2, 3, 4, 5),
      educ -> Seq(1, 2, 3, 4),
      educMen -> Seq(1, 2, 3, 4),
      rev -> Seq(1, 2, 3, 4, 5),
      cso -> Seq(1, 2, 3, 4),
      inf-> Seq(1,2),
      occ -> Seq(1, 2, 3, 4, 5),
      zonage -> Seq(1, 2, 3, 4),
      log-> Seq(1,2,3)
    )


    val canadians: IndicatorAndModalities = Map(
      kAge -> Seq(1, 2, 3, 4),
      sex -> Seq(1, 2),
      strM -> Seq(1, 2, 3),
      rev -> Seq(1, 2, 3, 4, 5),
      occ -> Seq(1, 2, 3, 4, 5)
    )

    val spatialOnly2: IndicatorAndModalities = Map(perimetre -> Seq(2))
    val spatialOnly3: IndicatorAndModalities = Map(perimetre -> Seq(3))
    val spatialOnly4: IndicatorAndModalities = Map(perimetre -> Seq(4))
    val spatial2And3: IndicatorAndModalities = Map(perimetre -> Seq(2, 3))
    val spatial3And4: IndicatorAndModalities = Map(perimetre -> Seq(3, 4))
    val spatial2And3And4: IndicatorAndModalities = Map(perimetre -> Seq(2, 3, 4))
    val spatialNone: IndicatorAndModalities = Map(perimetre -> Seq())


    val availableIndicatorsAndModalities: Map[Study, IndicatorAndModalities] = Map(
      "ALBI" -> (basic ++ spatialOnly3),
      "ALENCON" -> (basic ++ spatial2And3),
      "AMIENS" -> (basic ++ spatial2And3),
      "ANNECY" -> (annecy ++ spatial2And3),
      "ANGERS" -> (basic ++ spatial2And3),
      "ANGOULEME" -> (basic ++ spatial2And3),
      "ANNEMASSE" -> (basic ++ spatial2And3),
      "BAYONNE" -> (basic ++ spatial2And3),
      "BESANCON" -> (besCarc ++ spatialOnly2),
      "BEZIERS" -> (basic ++ spatial2And3),
      "BOGOTA" -> (bogota ++ spatialOnly4),
      "BORDEAUX" -> (basic ++ spatial2And3),
      "BREST" -> (basic ++ spatial2And3),
      "CAEN" -> (basic ++ spatial2And3),
      "CARCASSONNE" -> (besCarc ++ spatialOnly2),
      "CHERBOURG" -> (basic ++ spatial2And3),
      "CLERMONT FERRAND" -> (basic ++ spatial2And3),
      "CREIL" -> (basic ++ spatial2And3),
      "DIJON" -> (basic ++ spatial2And3),
      "DOUAI" -> (basic ++ spatial2And3),
      "DUNKERQUE" -> (basic ++ spatial2And3),
      "GRENOBLE" -> (basic ++ spatial2And3),
      "IDF" -> (idf ++ spatial2And3),
      "LA REUNION" -> (basic ++ spatial2And3),
      "LA ROCHELLE" -> (basic ++ spatial2And3),
      "LE HAVRE" -> (basic ++ spatial2And3),
      "LILLE" -> (basic ++ spatial2And3),
      "LONGWY" -> (basic ++ spatial2And3),
      "LYON" -> (basic ++ spatial2And3),
      "MARSEILLE" -> (basic ++ spatial2And3),
      "MARTINIQUE" -> (basic ++ spatial2And3),
      "METZ" -> (basic ++ spatial2And3),
      "MONTPELLIER" -> (basic ++ spatial2And3),
      "MONTREAL" -> (canadians ++ spatialNone),
      "NANCY" -> (basic ++ spatial2And3),
      "NANTES" -> (basic ++ spatial2And3),
      "NICE" -> (basic ++ spatial2And3),
      "NIMES" -> (basic ++ spatial2And3),
      "NIORT" -> (basic ++ spatial2And3),
      "OTTAWA GATINEAU" -> (canadians ++ spatialNone),
      "POITIERS" -> (basic ++ spatial2And3),
      "QUEBEC" -> (basic ++ spatial2And3),
      "QUIMPER" -> (basic ++ spatial2And3),
      "RENNES" -> (basic ++ spatial2And3),
      "ROUEN" -> (basic ++ spatial2And3),
      "SAGUENAY" -> (canadians ++ spatialNone),
      "SAINT BRIEUC" -> (basic ++ spatial2And3),
      "SAINT ETIENNE" -> (basic ++ spatial2And3),
      "SANTIAGO" -> (santiago ++ spatial3And4),
      "SAO PAULO" -> (saopaulo ++ spatial2And3And4),
      "SHERBROOK" -> (canadians ++ spatialNone),
      "STRASBOURG" -> (basic ++ spatial2And3),
      "THIONVILLE" -> (basic ++ spatial2And3),
      "TOULOUSE" -> (basic ++ spatial2And3),
      "TOURS" -> (basic ++ spatial2And3),
      "TROIS RIVIERE" -> (canadians ++ spatialNone),
      "VALENCE" -> (basic ++ spatial2And3),
      "VALENCIENNES" -> (basic ++ spatial2And3)
    )
  }

}