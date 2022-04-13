# ================================================================================#
#                                    MOBILIQUEST
#             Préparation des indicateurs à intégrer au Mobiliscope
#                                      fonctions 
# ================================================================================#

# library
library(tidylog)
library(tidyverse)
library(sf)
library(geojsonio)
library(geojsonsf)
library(OasisR) # Duncan
library(spdep) # Moran

#==== GLOBAL FUNCTIONS ====

# 1. Indicateur POPULATION GLOBALE ----
createPopFiles <- function(nomEnq, prez_long, sfSec, seuil, cheminOut){

  # 1a. CONSTRUCTION DES DONNEES POUR la carte en cercle proportionnelle - pop0_prop : 
  # nombre estimé de personnes présentes par secteur et par heure
  
  dir.create(paste0(cheminOut, nomEnq,"/pop0_prop"))
  dir.create(paste0(cheminOut, nomEnq,"/pop0_prop/data"))
  dir.create(paste0(cheminOut, nomEnq,"/pop0_prop/geo"))
  
  ## Comptage des présences par secteur et par heure : 
  ## POPULATION PRESENTE (STOCK PONDERE)
  pvs <- prez_long %>% 
    group_by(HOUR, CODE_SEC) %>% 
    summarise(popSec = round(sum(W_IND, na.rm = TRUE),2),
              popSecB = length(ID_IND)) %>% 
    ungroup() %>% 
    arrange(CODE_SEC, HOUR)
  
  ## Application d'un seuil - replace petits effectifs bruts par 0
  if(!is.na(seuil)){
    pvs <- pvs %>% 
      mutate(popSec = case_when(popSecB < seuil & popSecB > 0 ~ NaN,
                                TRUE ~ popSec))
  }
  
  ## Préparation de la table à joindre au geojson
  dataShpProp <- pvs %>% 
    select(HOUR, CODE_SEC, popSec) %>% 
    pivot_wider(names_from = HOUR, 
                values_from = popSec, 
                names_prefix = "pop0_") %>% 
    rename(Secteur_EM = CODE_SEC)
  
  ## Jointure 
  shpProp <- left_join(sfSec, dataShpProp, by = "Secteur_EM")
  
  ### Export
  shpProp <- sf_geojson(shpProp)
  geojson_write(shpProp,
                file = paste0(cheminOut, nomEnq,"/pop0_prop/geo/secteursData.geojson"),
                layer_options = "ENCODING=UTF-8")
  
  
  
  ## Préparation de la table pour le graphique simple (dataSect)
  oldH <- unique(as.character(pvs$HOUR))
  newH <- c("4am", "5am", "6am", "7am", 
            "8am", "9am", "10am", "11am", 
            "12am", "1pm", "2pm", "3pm", 
            "4pm", "5pm", "6pm", "7pm", 
            "8pm", "9pm", "10pm", "11pm", 
            "12pm", "1am", "2am", "3am")
  
  ## mise en forme csv STOCK
  dfProp <- pvs %>% 
    select(-popSecB) %>% 
    pivot_wider(names_from = CODE_SEC, values_from = popSec) %>% 
    mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>% 
    relocate(hour) %>% 
    select(-HOUR)
  
  
  ### Export
  write.csv(dfProp, 
             paste0(cheminOut, nomEnq,"/pop0_prop/data/dataSect.csv"), 
             row.names = FALSE)   
  
  
  
  # 1b. CONSTRUCTION DES DONNEES POUR la carte de densité - pop0_choro : 
  # Densité de personnes (km2) présentes par secteur et par heure
  
  dir.create(paste0(cheminOut, nomEnq,"/pop0_choro"))
  dir.create(paste0(cheminOut, nomEnq,"/pop0_choro/data"))
  dir.create(paste0(cheminOut, nomEnq,"/pop0_choro/geo"))
  
  ### joindre la variable 'AREA' de la couche des secteurs à la table de présence et calcul
  options(scipen = 999)
  pvs2 <- pvs %>% 
    left_join(., select(sfSec, CODE_SEC = Secteur_EM, AREA)) %>% 
    select(-geometry) %>% 
    mutate(AREA_KM = AREA/1e6,
           dens = round(popSec/AREA_KM,2))
  
  ## Préparation de la table à joindre au geojson
  dataShpChoro <- pvs2 %>% 
    select(HOUR, CODE_SEC, dens) %>% 
    pivot_wider(names_from = HOUR, 
                values_from = dens, 
                names_prefix = "pop0_") %>% 
    rename(Secteur_EM = CODE_SEC)
  
  ## Jointure avec le fichier shp
  shpChoro <- left_join(sfSec, dataShpChoro, by = "Secteur_EM")
  
  ### Export
  shpChoro <- sf_geojson(shpChoro)
  geojson_write(shpChoro,
                file = paste0(cheminOut, nomEnq,"/pop0_choro/geo/secteursData.geojson"),
                layer_options = "ENCODING=UTF-8")

  ## mise en forme csv DENS
  dfChoro <- pvs2 %>% 
    select(HOUR, CODE_SEC, dens) %>% 
    pivot_wider(names_from = CODE_SEC, values_from = dens) %>% 
    mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>% 
    relocate(hour) %>% 
    select(-HOUR)
  
  
  ### Export
  write.csv(dfChoro, 
             paste0(cheminOut, nomEnq,"/pop0_choro/data/dataSect.csv"), 
             row.names = FALSE) 
  
  
  
  
  # 1c. CONSTRUCTION DES DONNEES POUR LES CARTES EN OURSINS - pop0_flow : 
  # nombre estimé de personnes non résidentes par secteur et par heure + flux OD
  
  # Création des dossiers   
  dir.create(paste0(cheminOut, nomEnq,"/pop0_flow"))
  dir.create(paste0(cheminOut, nomEnq,"/pop0_flow/data"))
  dir.create(paste0(cheminOut, nomEnq,"/pop0_flow/geo"))
  

  ## Calcul des flux OD (origine = secteur de résidence - RES_SEC, destination = secteur de présence - CODE_SEC)
  ## seuil de population brute = 6 individus par secteur (en deçà on ne diffuse pas l'info)
  flowdata <- prez_long %>% 
    select(ID_IND, W_IND, HOUR, CODE_SEC, RES_SEC) %>% 
    group_by(HOUR, CODE_SEC, RES_SEC) %>% 
    summarise(W_IND = round(sum(W_IND, na.rm = TRUE),2),
              n = length(ID_IND)) %>% 
    ungroup() %>% 
    filter(CODE_SEC != RES_SEC & n >= 6) %>%   ### seuil toujours appliqué
    select(-n) %>% 
    arrange(CODE_SEC, HOUR)
  
  ## Export de flowdata
  write.csv2(flowdata, 
             paste0(cheminOut,nomEnq,"/pop0_flow/geo/flowData.csv"), 
             row.names = FALSE)  
  
  
  ## data stock NR
  pvs3 <- prez_long %>% 
    filter(CODE_SEC != RES_SEC) %>% 
    group_by(HOUR, CODE_SEC) %>% 
    summarise(popSec = round(sum(W_IND, na.rm = TRUE),2),
              popSecB = length(ID_IND)) %>%    
    ungroup() %>% 
    arrange(CODE_SEC, HOUR)
  
  ## Application d'un seuil - replace petits effectifs bruts par 0
  if(!is.na(seuil)){
    pvs3 <- pvs3 %>% 
      mutate(popSec = case_when(popSecB < seuil & popSecB > 0 ~ NaN,
                                TRUE ~ popSec))
  }
  
      
  ## Préparation de la table à joindre au geojson  
  dataShpChoroNR <- pvs3 %>% 
    select(-popSecB) %>% 
    pivot_wider(names_from = HOUR, 
                values_from = popSec, 
                names_sort = TRUE,
                names_prefix = "pop0_") %>% 
    rename(Secteur_EM = CODE_SEC) %>% 
    arrange(Secteur_EM) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  ## Jointure avec le geojson
  shpChoroNR <- left_join(sfSec, dataShpChoroNR, by = "Secteur_EM")
  
  ## Export
  shpChoroNR <- sf_geojson(shpChoroNR)
  geojson_write(shpChoroNR,
                file = paste0(cheminOut, nomEnq, "/pop0_flow/geo/secteursData.geojson"),
                layer_options = "ENCODING=UTF-8")
  
  ## mise en forme csv STOCK NR
  dfChoroNR <- pvs3 %>% 
    select(-popSecB) %>% 
    pivot_wider(names_from = CODE_SEC, values_from = popSec) %>% 
    mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>% 
    relocate(hour) %>% 
    select(-HOUR) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  
  ## Export des tables
  write.csv(dfChoroNR, 
             paste0(cheminOut, nomEnq,"/pop0_flow/data/dataSect.csv"), 
             row.names = FALSE) 
}

# 2. Autres indicateurs : ----

#~ calcul des pourcentages ----
calculPart <- function(dataStock){
  data.frame(dataStock[ , 1:3], apply(dataStock[ , 4:length(dataStock)], MARGIN = 2, 
                                      FUN = function(x){(x * 100) / dataStock$popSec}))
}

#~ comptage des présences par heure/secteur et par modalités d'indicateur ----
prepPVS <- function(nomEnq, prez_long, nomIndic, nomVar, seuil){
  
  result <- list()
  
  ## Construction de la table de présence par secteur et par heure : STOCKS
  if(is.na(seuil)){
    pvs <- prez_long %>% 
      select(HOUR, CODE_SEC, W_IND, all_of(nomVar)) %>%
      mutate(nomVar = as.numeric(get(nomVar))) %>%
      filter(!is.na(nomVar)) %>% 
      filter(nomVar>0) %>% 
      arrange(nomVar) %>% 
      pivot_wider(id_cols = c(HOUR, CODE_SEC), 
                  names_from = nomVar, 
                  names_prefix = nomIndic,
                  names_sort = TRUE,
                  values_from = W_IND,
                  values_fn = sum) %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
      mutate_if(is.numeric, ~round(.,2)) %>%
      group_by(HOUR, CODE_SEC) %>%  
      mutate(popSec = sum(c_across(where(is.numeric)))) %>% 
      relocate(popSec, .after = CODE_SEC) %>% 
      ungroup() %>% 
      arrange(CODE_SEC, HOUR)
  }
  
  
  ### Calcul avec application d'un seuil de répondants bruts
  if(!is.na(seuil)){
    pvs <- prez_long %>% 
      select(HOUR, CODE_SEC, W_IND, all_of(nomVar)) %>%
      mutate(nomVar = as.numeric(get(nomVar))) %>%
      filter(!is.na(nomVar)) %>% 
      filter(nomVar>0) %>% 
      arrange(nomVar) %>% 
      mutate(N_IND = 1) %>% 
      pivot_wider(id_cols = c(HOUR, CODE_SEC), 
                  names_from = nomVar, 
                  names_prefix = nomIndic,
                  names_sort = TRUE,
                  values_from = c(W_IND, N_IND),
                  values_fn = sum) %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
      mutate_if(is.numeric, ~round(.,2)) %>%
    
    # Application du filtre
    pvs <- pvs %>% 
      mutate(across(all_of(starts_with("W")), 
                    ~ case_when(get(str_replace(cur_column(), "W_IND", "N_IND")) < seuil &
                                  get(str_replace(cur_column(), "W_IND", "N_IND")) > 0 ~ NaN,
                                TRUE ~ .))) %>% 
      select(-c(starts_with("N_IND"))) %>% 
      rename_with(., ~ str_replace(., "W_IND_", ""), starts_with("W")) %>% 
      group_by(HOUR, CODE_SEC) %>%
      mutate(popSec = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
      relocate(popSec, .after = CODE_SEC) %>%
      ungroup() %>%
      arrange(CODE_SEC, HOUR)
  }
  
  
  ## Construction de la table de présence par secteur et par heure : PARTS
  pvs2 <- calculPart(dataStock = pvs)
  
  ## Construction de la table de présence par secteur et par heure : 
  ## POPULATION NON RESIDENTE (STOCKS)
  if(is.na(seuil)){
    pvs3 <- prez_long %>%
      filter(CODE_SEC != RES_SEC) %>%
      select(HOUR, CODE_SEC, W_IND, all_of(nomVar)) %>%
      mutate(nomVar = as.numeric(get(nomVar))) %>%
      filter(!is.na(nomVar)) %>% 
      filter(nomVar>0) %>% 
      arrange(nomVar) %>% 
      pivot_wider(id_cols = c(HOUR, CODE_SEC), 
                  names_from = nomVar, 
                  names_prefix = nomIndic,
                  names_sort = TRUE,
                  values_from = W_IND,
                  values_fn = sum) %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
      mutate_if(is.numeric, ~round(.,2)) %>%
      group_by(HOUR, CODE_SEC) %>%  
      mutate(popSec = sum(c_across(where(is.numeric)))) %>% 
      relocate(popSec, .after = CODE_SEC) %>% 
      ungroup() %>% 
      arrange(CODE_SEC, HOUR)
  }
  
  ### Calcul avec application d'un seuil de répondants bruts
  if(!is.na(seuil)){
    pvs3 <- prez_long %>% 
      filter(CODE_SEC != RES_SEC) %>%
      select(HOUR, CODE_SEC, W_IND, all_of(nomVar)) %>%
      mutate(nomVar = as.numeric(get(nomVar))) %>%
      filter(!is.na(nomVar)) %>% 
      filter(nomVar>0) %>% 
      arrange(nomVar) %>% 
      mutate(N_IND = 1) %>% 
      pivot_wider(id_cols = c(HOUR, CODE_SEC), 
                  names_from = nomVar, 
                  names_prefix = nomIndic,
                  names_sort = TRUE,
                  values_from = c(W_IND, N_IND),
                  values_fn = sum) %>% 
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
      mutate_if(is.numeric, ~round(.,2)) %>%
    
    # Application du filtre
    pvs3 <- pvs3 %>% 
      mutate(across(all_of(starts_with("W")), 
                    ~ case_when(get(str_replace(cur_column(), "W_IND", "N_IND")) < seuil & 
                                  get(str_replace(cur_column(), "W_IND", "N_IND")) > 0 ~ NaN,
                                TRUE ~ .))) %>% 
      select(-c(starts_with("N_IND"))) %>% 
      rename_with(., ~ str_replace(., "W_IND_", ""), starts_with("W")) %>% 
      group_by(HOUR, CODE_SEC) %>%
      mutate(popSec = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
      relocate(popSec, .after = CODE_SEC) %>%
      ungroup() %>%
      arrange(CODE_SEC, HOUR)
    
  }
  
  if(nomVar=="MOTIF"){
    pvs3 <- pvs3 %>% mutate(act1 = 0)
  }
  
  mod <- prez_long %>% 
    select(HOUR, CODE_SEC, W_IND, all_of(nomVar)) %>%
    mutate(nomVar = as.numeric(get(nomVar))) %>%
    filter(!is.na(nomVar)) %>% 
    filter(nomVar>0) %>% 
    arrange(nomVar) %>% 
    pull()
  mod <- unique(mod)
  
  
  result[["pvs"]] <- pvs
  result[["pvs2"]] <- pvs2
  result[["pvs3"]] <- pvs3
  result[["mod"]] <- mod
  return(result)
  
  
}

#~ création des geojson et des csv pour le graphique "simple" ----
createFiles <- function(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut){

  # mini tables de présences de l'indicateur
  pvs <- data[["pvs"]]
  pvs2 <- data[["pvs2"]]
  if(nomIndic != "res"){
    pvs3 <- data[["pvs3"]]
  }
  # valeur des modalités
  mod <- data[["mod"]]
  
  
  # pour chaque modalité :
  for(i in 1:length(mod)){
    
    indic <- colnames(pvs)[3 + as.numeric(i)]
    
    # Création des répertoires
    ## Répertoires parents (2 par indicateur)
    dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_prop"))
    dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_choro"))
    
    ## Répertoires enfants (2 par répertoire parent)
    dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_prop/geo"))
    dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_choro/geo"))
    
    dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_prop/data"))
    dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_choro/data"))
    
    
    
    ## Préparation du tableau de données à joindre au geojson
    ## data stock
    dataShpProp <- pvs %>% 
      select(HOUR, CODE_SEC, all_of(indic)) %>%
      pivot_wider(names_from = HOUR, 
                  values_from = indic, 
                  names_sort = TRUE,
                  names_prefix = paste0(indic, "_")) %>% 
      rename(Secteur_EM = CODE_SEC)
    
    ### Jointure des données au fond de carte
    shpProp <- left_join(sfSec, dataShpProp, by = "Secteur_EM")
    
    ### Export des données spatiales
    shpProp <- sf_geojson(shpProp)
    geojson_write(shpProp,
                  file = paste0(cheminOut, nomEnq, "/", indic ,"_prop/geo/secteursData.geojson"),
                  layer_options = "ENCODING=UTF-8")
    
    
    ## data part
    dataShpChoro <- pvs2 %>% 
      select(HOUR, CODE_SEC, indic) %>%
      pivot_wider(names_from = HOUR, 
                  values_from = indic, 
                  names_sort = TRUE,
                  names_prefix = paste0(indic, "_")) %>% 
      rename(Secteur_EM = CODE_SEC)
    
    ### Jointure des données au fond de carte
    shpChoro <- left_join(sfSec, dataShpChoro, by = "Secteur_EM")
    
    ### Export des données spatiales
    shpChoro <- sf_geojson(shpChoro)
    geojson_write(shpChoro,
                  file = paste0(cheminOut, nomEnq, "/", indic ,"_choro/geo/secteursData.geojson"),
                  layer_options = "ENCODING=UTF-8")
    
    
    ## Création des données pour les cartes en oursins
    if(!nomIndic %in% c("res")){
      
      ## Répertoires parents (2 par indicateur)
      dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_flow"))
      
      ## Répertoires enfants (2 par répertoire parent)
      dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_flow/geo"))
      dir.create(paste0(cheminOut, nomEnq, "/", indic ,"_flow/data"))
      
      ### Flowdata : csv des flux OD avec seuil à 6
      flowdata <- prez_long %>% 
        select(ID_IND, W_IND, HOUR, CODE_SEC, RES_SEC, nomVar) %>%
        filter(nomVar == i) %>% 
        group_by(HOUR, CODE_SEC, RES_SEC) %>% 
        summarise(W_IND = round(sum(W_IND, na.rm = TRUE),2),
                  n = length(ID_IND)) %>% 
        filter(CODE_SEC != RES_SEC & n  >= 6) %>% ## application du seuil
        select(-n) %>% 
        arrange(CODE_SEC, HOUR)
      
      ### data stock NR
      dataShpChoroNR <- pvs3 %>% 
        select(HOUR, CODE_SEC, all_of(indic)) %>%
        pivot_wider(names_from = HOUR, 
                    values_from = indic, 
                    names_sort = TRUE,
                    names_prefix = paste0(indic, "_")) %>% 
        rename(Secteur_EM = CODE_SEC) %>% 
        arrange(Secteur_EM) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0))
      
      ### Jointure des données au fond de carte
      shpChoroNR <- left_join(sfSec, dataShpChoroNR, by = "Secteur_EM")
      
      ### Export des données spatiales
      shpChoroNR <- sf_geojson(shpChoroNR)
      geojson_write(shpChoroNR,
                    file = paste0(cheminOut, nomEnq, "/", indic ,"_flow/geo/secteursData.geojson"),
                    layer_options = "ENCODING=UTF-8")
      write.csv2(flowdata, 
                 paste0(cheminOut, nomEnq, "/", indic ,"_flow/geo/flowData.csv"), 
                 row.names = FALSE)
    }
    
    
    
    # Création des tables pour le graphique simple
    oldH <- unique(as.character(pvs$HOUR))
    newH <- c("4am", "5am", "6am", "7am", 
              "8am", "9am", "10am", "11am", 
              "12am", "1pm", "2pm", "3pm", 
              "4pm", "5pm", "6pm", "7pm", 
              "8pm", "9pm", "10pm", "11pm", 
              "12pm", "1am", "2am", "3am")
    
    ## mise en forme csv STOCK
    dfProp <- pvs %>% 
      select(HOUR, CODE_SEC, all_of(indic)) %>% 
      pivot_wider(names_from = CODE_SEC, values_from = indic) %>% 
      mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>% 
      relocate(hour) %>% 
      select(-HOUR)
    
    
    ### Export des données
    write.csv(dfProp, 
              paste0(cheminOut, nomEnq, "/", indic ,"_prop/data/dataSect.csv"), 
              row.names = FALSE)
    
    
    ## mise en forme csv PART
    dfChoro <- pvs2 %>% 
      select(HOUR, CODE_SEC, all_of(indic)) %>% 
      pivot_wider(names_from = CODE_SEC, values_from = indic) %>% 
      mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>%
      relocate(hour) %>% 
      select(-HOUR)
    
    
    ### Export des données
    write.csv(dfChoro, 
              paste0(cheminOut, nomEnq, "/", indic ,"_choro/data/dataSect.csv"), 
              row.names = FALSE)
    
    ## Oursins
    if(nomIndic != "res"){
      
      ## mise en forme csv STOCK NR
      dfChoroNR <- pvs3 %>% 
        select(HOUR, CODE_SEC, all_of(indic)) %>% 
        pivot_wider(names_from = CODE_SEC, values_from = indic) %>% 
        mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>%
        relocate(hour) %>% 
        select(-HOUR) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0))
      
      ## Export des données
      write.csv(dfChoroNR, 
                paste0(cheminOut, nomEnq, "/", indic ,"_flow/data/dataSect.csv"), 
                row.names = FALSE)
    }
    
      
  }
  
}  

#~ création des indices de ségrégation et csv ----
createISeg <- function(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut){
  
  # mini tables de présences de l'indicateur
  pvs <- data[["pvs"]]
  pvs2 <- data[["pvs2"]]
  # valeur des modalités
  mod <- data[["mod"]]
  
  # DUNCAN
  ## Init table
  duncan <- data.frame("hour" = unique(pvs$HOUR))
  
  ## Calcul de l'indice
  for (i in unique(pvs$HOUR)){
    
    duncan <- bind_rows(duncan, as.data.frame(t(ISDuncan(pvs[pvs$HOUR == i , 4:length(pvs)]))))
    
  }
  
  ## mise en forme 
  newH <- c("4am", "5am", "6am", "7am", 
            "8am", "9am", "10am", "11am", 
            "12am", "1pm", "2pm", "3pm", 
            "4pm", "5pm", "6pm", "7pm", 
            "8pm", "9pm", "10pm", "11pm", 
            "12pm", "1am", "2am", "3am")
  
  duncan <- duncan %>% 
    filter(!is.na(V1)) %>% 
    mutate(hour = newH) %>% # !! pvs$HOUR doit être factor with 24 levels from h4 to h27
    mutate_if(is.numeric, ~replace(., is.na(.), 0))
  
  ## Rename variables
  colnames(duncan)[2:length(duncan)] <- colnames(pvs[ , 4:length(pvs)])
  
  ## Pour le Canada rev5 en 2eme col
  if(ctry == "CA" & nomIndic == "rev") {
    duncan <- duncan %>% relocate(c("hour", "rev5"))
  }
  ## Pour Bogota mode4 en 2eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    duncan <- duncan %>% relocate(c("hour", "mode4"))
  }
  
  
  # MORAN
  
  ## projection 
  if (ctry == "FR") {
    if (!nomEnq %in% c("LA REUNION", "MARTINIQUE")) {
      shpSectMoran <- sfSec %>% 
        st_transform(crs = 2154)
    }
    if (nomEnq == "LA REUNION") {
      shpSectMoran <- sfSec %>% 
        st_transform(crs = 2975)
    }
    if (nomEnq == "MARTINIQUE") {
      shpSectMoran <- sfSec %>% 
        st_transform(crs = 5490)
    }
  }
  
  if (ctry == "CA") {
    shpSectMoran <- sfSec %>% 
      st_transform(crs = 3978)
  }
  
  if (nomEnq == "BOGOTA") {
    shpSectMoran <- sfSec %>% 
      st_transform(crs = 21897) %>% 
      # supression des secteurs non contigüs
      filter(!Secteur_EM %in% c("UTAM700", "UTAM650", "UTAM680", 
                                "UTAM563", "UTAM660", "UTAM690",
                                "UTAM670", "UTAM640", "UTAM540",
                                "UTAM500", "UTAM520", "UTAM580",
                                "UTAM620", "UTAM600", "UTAM590",
                                "UTAM630", "UTAM610", "UTAM89"))
  }
  
  if (nomEnq == "SAO PAULO") {
    shpSectMoran <- sfSec %>% 
      st_transform(crs = 22523)
  }
  
  if (nomEnq == "SANTIAGO") {
    shpSectMoran <- sfSec %>% 
      st_transform(crs = 32719)
  }
  
  
  ## Init table
  moran <- data.frame("hour" = '', "var" = '', "moran" = numeric(24))
  
  for (i in unique(pvs2$HOUR)){
    
    # Trier les données
    dataMoran <- filter(pvs2 %>% select(-popSec), HOUR == i)
    dataMoran <- dataMoran %>%
      rename(Secteur_EM = CODE_SEC)
    
    # Joindre avec le shp
    dataMoran <- left_join(shpSectMoran, dataMoran, by = "Secteur_EM")
    
    # Calcul des paramètres
    nbSecteurs <- poly2nb(pl = dataMoran,
                          row.names = dataMoran$Secteur_EM,
                          snap = 50,
                          queen = TRUE)
    
    dataMoran <- dataMoran %>%
      st_drop_geometry()
    
    # replace hour na with i and indic na with zero
    dataMoran <- dataMoran %>% 
      mutate_if(is.factor, ~replace(., is.na(.), i)) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0))
    
    # Calcul de l'indice de Moran
    # n mod +1 car jointure de perim 
    for (j in colnames(dataMoran[ , (length(dataMoran)-length(mod)+1):length(dataMoran)])){
      
      Moran <- moran.mc(x = dataMoran[[j]],
                        listw = nb2listw(nbSecteurs), nsim=1000)
      
      moran <- rbind(moran, data.frame("hour" = i,
                                       "var" = j,
                                       "moran" = Moran$statistic,
                                       stringsAsFactors = FALSE))
    }
    
  }
  
  
  ## Mise en forme
  moran <- moran %>% 
    filter(hour != '') %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    pivot_wider(names_from = var, values_from = moran) %>% 
    mutate(hour = newH) # !! pvs$HOUR doit être factor with 24 levels from h4 to h27
  
  ## Pour le Canada rev5 en 2eme col
  if(ctry == "CA" & nomIndic == "rev") {
    moran <- moran %>% relocate(c("hour", "rev5"))
  }
  ## Pour Bogota mode4 en 2eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    moran <- moran %>% relocate(c("hour", "mode4"))
  }
  
  
  ## EXPORT 
  write.csv(duncan, 
              paste0(cheminOut, nomEnq, "/indice_segreg/", nomIndic,"_Duncan.csv"), 
              row.names = FALSE)
  write.csv(moran, 
              paste0(cheminOut, nomEnq, "/indice_segreg/", nomIndic,"_Moran.csv"), 
              row.names = FALSE)
}  

#~ création des csv pour graphique en aires empilées (stacked) ----
createStacked <- function(nomIndic, nomEnq, ctry, data, cheminOut){
  
  # mini tables de présences de l'indicateur
  pvs <- data[["pvs"]]
  pvs2 <- data[["pvs2"]]
  pvs3 <- data[["pvs3"]]
  #valeur des modalités
  mod <- data[["mod"]]
  
  # hour vec
  oldH <- unique(as.character(pvs$HOUR))
  newH <- c("4am", "5am", "6am", "7am", 
            "8am", "9am", "10am", "11am", 
            "12am", "1pm", "2pm", "3pm", 
            "4pm", "5pm", "6pm", "7pm", 
            "8pm", "9pm", "10pm", "11pm", 
            "12pm", "1am", "2am", "3am")
  
  # 1. Cartes choro
  df <- pvs2 %>% 
    mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>% 
    rename(district = CODE_SEC) %>% 
    relocate(district, hour) %>% 
    select(-HOUR, -popSec)
  
  ## Pour le Canada rev5 en 3eme col
  if(ctry == "CA" & nomIndic == "rev") {
    df <- df %>% relocate(c("hour", "district", "rev5"))
  }
  ## Pour Bogota mode4 en 3eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    df <- df %>% relocate(c("hour", "district", "mode4"))
  }
  
  ## Export
  write.csv(df, paste0(cheminOut, nomEnq, "/stacked/", nomIndic, "_choro_stacked.csv"), 
              row.names = FALSE)
  
  
  # 2. Cartes Proportionnelles
  
  df <- pvs %>% 
    mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>% 
    rename(district = CODE_SEC) %>% 
    relocate(district, hour) %>% 
    select(-HOUR, -popSec)
  
  
  ## Pour le Canada rev5 en 3eme col
  if(ctry == "CA" & nomIndic == "rev") {
    df <- df %>% relocate(c("hour", "district", "rev5"))
  }
  ## Pour Bogota mode4 en 3eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    df <- df %>% relocate(c("hour", "district", "mode4"))
  }
  
  ## Export
  write.csv(df, 
             paste0(cheminOut, nomEnq, "/stacked/", nomIndic, "_prop_stacked.csv"), 
             row.names = FALSE)
  
  
  # 3. Cartes en oursins
  if(!nomIndic %in% c("res")){
    
    df <- pvs3 %>% 
      mutate(hour = recode(HOUR, !!!setNames(newH, oldH))) %>% 
      rename(district = CODE_SEC) %>% 
      relocate(district, hour) %>% 
      select(-HOUR, -popSec)
    
    ## Pour le Canada rev5 en 3eme col
    if(ctry == "CA" & nomIndic == "rev") {
      df <- df %>% relocate(c("hour", "district", "rev5"))
    }
    ## Pour Bogota mode4 en 3eme col
    if(nomEnq == "BOGOTA" & nomIndic == "mode") {
      df <- df %>% relocate(c("hour", "district", "mode4"))
    }
    
    ## Export
    write.csv(df, 
               paste0(cheminOut, nomEnq, "/stacked/", nomIndic, "_flow_stacked.csv"), 
                row.names = FALSE)
  }
  
}  



#==== ALGO FUNCTION ====

##---- Fonction p2m : de la table de présence aux indicateurs du Mobiliscope ----
p2m <- function(nomEnq, perim, subpop, cheminIn, cheminOut){


  # Création des répertoires de sortie 
  dir.create(paste0(cheminOut, nomEnq))
  dir.create(paste0(cheminOut, nomEnq, "/stacked"))
  dir.create(paste0(cheminOut, nomEnq, "/indice_segreg"))

  
  #~ 0. FILTRES : Enquête, périmètre et sous-population ----
  
  ## couche secteur
  sfSec <- st_read(paste0(cheminIn, "/BDgeo/SEC_59ED_W84.shp"))
  sfSec <- sfSec %>% 
    mutate(ENQUETE = case_when(LIB_ED=="Valenciennes, 2011" ~ "VALENCIENNES2011",
                               TRUE ~ ENQUETE)) %>% 
    filter(ENQUETE == nomEnq) %>% 
    rename(Secteur_EM = CODE_SEC, CENTROID_X = X_W84, CENTROID_Y = Y_W84) 
  
  ### périmètre
  if(length(perim)!=0){
    sfSec <- sfSec %>% 
      filter(ZONAGE_SEC %in% perim)
  }
  
  ## données de présence
  prez_long <- readRDS(paste0(cheminIn, "/presence_utile_", nomEnq, ".RDS"))
  
  ### effectif de départ avant filtrage
  eff_start <- nrow(prez_long)

  ### périmètre
  if(length(perim)!=0){
    prez_long <- prez_long %>% 
      filter(CODE_SEC %in% sfSec$Secteur_EM)
  }

  ### code pays de l'enquête
  ctry <- unique(prez_long$PAYS)
  
  ### sous-population
  subpop <- subpop %>% compact()
    
  if(length(subpop)!=0){
    fns <- imap(subpop, ~ call(if (length(.x) == 1) "==" else "%in%", sym(.y), .x))
    prez_long <- prez_long %>%
      filter(!!!unname(fns))
  }
  
  
  ### SEUIL
  seuil <- NA
  
  if(nrow(prez_long)==0){
    cat("STOP PROCESS: ", "zero population")
  } else if(nrow(prez_long) < (5*eff_start)/100){
    cat("STOP PROCESS: ", "insufficient population (", nrow(prez_long), "presences remain after filtering)")
  } else {
    #~ 1. INDICATEUR "WHOLE POPULATION" ----
    createPopFiles(nomEnq, prez_long, sfSec, seuil, cheminOut)
    
    
    #~ 2. TOUS LES AUTRES INDICATEURS ----
    
    ## POPULATION RESIDENTE/NON RESIDENTE
    nomIndic <- "res"
    nomVar <- "RES"
    
    if(!nomVar%in%names(subpop)){
      data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
      createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
      createStacked(nomIndic, nomEnq, ctry, data, cheminOut)  }
    
    ## SEX 
    nomIndic <- "sex"
    nomVar <- "SEX"
    
    if(!nomVar%in%names(subpop)){
      data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
      createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
      createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
      createStacked(nomIndic, nomEnq, ctry, data, cheminOut)  }
    
    
    
    ## AGE
    nomIndic <- "age"
    nomVar <- "KAGE"
    
    if(!nomVar%in%names(subpop)){
      data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
      createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
      createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
      createStacked(nomIndic, nomEnq, ctry, data, cheminOut)  }
    
    ## OCCUPATION PRINCIPALE
    nomIndic <- "occ"
    nomVar <- "OCC"
    
    if(!nomVar%in%names(subpop)){
      data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
      createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
      createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
      createStacked(nomIndic, nomEnq, ctry, data, cheminOut)  }
    
    
    ## ACTIVITE 
    nomIndic <- "act"
    nomVar <- "MOTIF"
    
    if(!nomVar%in%names(subpop)){
      data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
      createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
      createStacked(nomIndic, nomEnq, ctry, data, cheminOut)  }
    
    
    nomIndic <- "mode"
    nomVar <- "MODE_ARR"
    
    if(!nomVar%in%names(subpop)){
      data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
      createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
      createStacked(nomIndic, nomEnq, ctry, data, cheminOut)  }
    
    if(ctry %in% c("FR", "AS")){
      
      ## NIVEAU D'EDUCATION (INDIVIDUEL)
      nomIndic <- "cleduc"
      nomVar <- "EDUC"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
      
      ## NIVEAU D'EDUCATION (MENAGE) 
      nomIndic <- "educmen"
      nomVar <- "EDUCMEN"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
    }
    
    if(ctry == "FR"){
      ## CSP (INDIVIDUELLE) 
      nomIndic <- "cs"
      nomVar <- "CSP"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
      
      ## CSP (MENAGE) 
      nomIndic <- "cspmen"
      nomVar <- "CSPMEN"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
    }
    
    
    ## ZONE DE RESIDENCE 
    if(ctry == "FR"){
      nomIndic <- "resarea"
      nomVar <- "ZONAGE"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
    }
    
    
    ## QPV 
    if(ctry == "FR" & nomEnq != "ANNECY"){
      
      nomIndic <- "qpv"
      nomVar <- "QPV"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
    }
    
    
    # REVENU (MENAGE) - PARIS, CANADA et Amérique du Sud
    if(nomEnq == "IDF" || ctry %in% c("CA", "AS")){
      
      nomIndic <- "rev"
      nomVar <- "REV"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
    }
    
    
    if(nomEnq == "IDF"){
      
      # DEPARTEMENT DE RESIDENCE 
      nomIndic <- "dep"
      nomVar <- "DEP"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
    }
    
    if(ctry=="AS"){
      
      ## 3o. CSO (des actifs)
      nomIndic <- "cso"
      nomVar <- "CSO"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
      
      ## COMPOSITION DU MENAGE
      nomIndic <- "strm"
      nomVar <- "STRM"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
      
      ## STATUT D'OCCUPATION DU LOGEMENT
      nomIndic <- "log"
      nomVar <- "LOG"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
    }
    
    ## INFORMALITE (des actifs) 
    if(nomEnq %in% c("BOGOTA", "SAO PAULO")){
      
      nomIndic <- "inf"
      nomVar <- "INFORMAL"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
    }
    
    
    
    if(ctry=="AS"){ 
      ## ZONAGE METAL 
      nomIndic <- "zona"
      nomVar <- "ZONAGE"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
      
    }
    
    if(nomEnq == "BOGOTA"){    
      ## SSE
      nomIndic <- "sse"
      nomVar <- "RES_SSE"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
    }
    
    if(ctry=="FR"){
      ## STATUT D'OCCUPATION DU LOGEMENT
      nomIndic <- "strmfr"
      nomVar <- "STRM"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
    }
    
    if(ctry=="CA"){
      ## STATUT D'OCCUPATION DU LOGEMENT
      nomIndic <- "strmqc"
      nomVar <- "STRM"
      
      if(!nomVar%in%names(subpop)){
        data <- prepPVS(nomEnq, prez_long, nomIndic, nomVar, seuil)
        createFiles(nomIndic, nomVar, nomEnq, data, prez_long, sfSec, cheminOut)
        createISeg(nomIndic, nomVar, nomEnq, ctry, data, sfSec, cheminOut)
        createStacked(nomIndic, nomEnq, ctry, data, cheminOut)    }
    }
  }
  
  
  
  
}

