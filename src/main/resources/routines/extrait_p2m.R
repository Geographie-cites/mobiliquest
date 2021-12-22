#===================    Extrait de p2m.R  ==================#


# librairies 
library(tidylog)
library(tidyverse)
library(sf)
library(data.table) # setDT function
library(geojsonio)
library(OasisR) # Duncan
library(spdep) # Moran




# fonctions ----
calculPart <- function(dataStock){
  data.frame(dataStock[ , 1:3], apply(dataStock[ , 4:length(dataStock)], MARGIN = 2, 
                                      FUN = function(x){(x * 100) / dataStock$popSec}))
}

prepAge <- function(nomEnq, prez_long){
  
  result <-  list()
  
  ## Construction de la table de présence par secteur et par heure : STOCKS
  pvs <- prez_long %>% 
    group_by(variable, CODE_SEC) %>% 
    summarise(popSec = sum(W_IND[as.numeric(KAGE) >= 1], na.rm = TRUE),
              age1 = sum(W_IND[as.numeric(KAGE) == 1], na.rm = TRUE),
              age2 = sum(W_IND[as.numeric(KAGE) == 2], na.rm = TRUE),
              age3 = sum(W_IND[as.numeric(KAGE) == 3], na.rm = TRUE),
              age4 = sum(W_IND[as.numeric(KAGE) == 4], na.rm = TRUE))
  
  ## Construction de la table de présence par secteur et par heure : PARTS
  pvs2 <- calculPart(dataStock = pvs)
  
  ## Construction de la table de présence par secteur et par heure : 
  ## POPULATION NON RESIDENTE (STOCKS)
  pvs3 <- prez_long %>% 
    filter(CODE_SEC != RES_SEC) %>% 
    group_by(variable, CODE_SEC) %>% 
    summarise(popsec = sum(W_IND[as.numeric(KAGE) >= 1], na.rm = TRUE),
              age1 = sum(W_IND[as.numeric(KAGE) == 1], na.rm = TRUE),
              age2 = sum(W_IND[as.numeric(KAGE) == 2], na.rm = TRUE),
              age3 = sum(W_IND[as.numeric(KAGE) == 3], na.rm = TRUE),
              age4 = sum(W_IND[as.numeric(KAGE) == 4], na.rm = TRUE))
  
  result[["pvs"]] <- pvs
  result[["pvs2"]] <- pvs2
  result[["pvs3"]] <- pvs3
  return(result)
  
}

createFiles <- function(nbMod, indicateur, nomIndic, nomEnq, data, prez_long, sfSec, outputDir){

  pvs <- data[["pvs"]]
  pvs2 <- data[["pvs2"]]
  if(nomIndic != "res"){
    pvs3 <- data[["pvs3"]]
  }
  
  # Calcul des valeurs min et max pour l'ensemble des modalités
  # Ces valeurs seront utilisées pour déterminer les bornes minimum et maximum des graphiques 
  # "simples" par secteur
  
  ## STOCK (cercle proportionnel)
  vMinS <- 1000000 # init
  vMaxS <- 0 # init
  
  vMinS <- apply(pvs[ , 4:length(pvs)], MARGIN = 2, function(x){
    if(min(x) < vMinS){
      vMinS <-  min(x)
    }
  })
  vMinS <- min(vMinS) # valeur min en stocks
  
  vMaxS <- apply(pvs[ , 4:length(pvs)], MARGIN = 2, function(x){
    if(max(x) > vMaxS){
      vMaxS <-  max(x)
    }
  })
  vMaxS <- max(vMaxS) # valeur max en stocks
  
  dfMinMaxS <- data.frame("min" = numeric(length(unique(prez_long$CODE_SEC))), 
                          "max" = numeric(length(unique(prez_long$CODE_SEC)))) 
  
  dfMinMaxS$min <- vMinS
  dfMinMaxS$max <- vMaxS
  dfMinMaxS <- as.data.frame(t(dfMinMaxS))
  dfMinMaxS <- setDT(dfMinMaxS, keep.rownames = TRUE)[]
  
  ## PART (carte Choro)
  vMinP <- 110 # init
  vMaxP <- 0 # init
  
  
  vMinP <- apply(pvs2[ , 4:length(pvs2)], MARGIN = 2, function(x){
    if(min(x) < vMinP){
      vMinP <-  min(x)
    }
  })
  vMinP <- min(vMinP) # valeur min en parts
  
  vMaxP <- apply(pvs2[ , 4:length(pvs2)], MARGIN = 2, function(x){
    if(max(x) > vMaxP){
      vMaxP <-  max(x)
    }
  })
  vMaxP <- max(vMaxP) # valeur max en parts
  
  dfMinMaxP <- data.frame("min" = numeric(length(unique(prez_long$CODE_SEC))), 
                          "max" = numeric(length(unique(prez_long$CODE_SEC)))) 
  
  dfMinMaxP$min <- vMinP
  dfMinMaxP$max <- vMaxP
  dfMinMaxP <- as.data.frame(t(dfMinMaxP))
  dfMinMaxP <- setDT(dfMinMaxP, keep.rownames = TRUE)[]
  
  ## Carte en oursins
  if(nomIndic != "res"){
    vMinF <- 1000000 # init
    vMaxF <- 0 # init
    
    vMinF <- apply(pvs3[ , 4:length(pvs3)], MARGIN = 2, function(x){
      if(min(x) < vMinF){
        vMinF <-  min(x)
      }
    })
    vMinF <- min(vMinF) # valeur min en stocks
    
    vMaxF <- apply(pvs3[ , 4:length(pvs3)], MARGIN = 2, function(x){
      if(max(x) > vMaxF){
        vMaxF <-  max(x)
      }
    })
    vMaxF <- max(vMaxF) # valeur max en stocks
    
    dfMinMaxF <- data.frame("min" = numeric(length(unique(prez_long$CODE_SEC))), 
                            "max" = numeric(length(unique(prez_long$CODE_SEC)))) 
    
    dfMinMaxF$min <- vMinF
    dfMinMaxF$max <- vMaxF
    dfMinMaxF <- as.data.frame(t(dfMinMaxF))
    dfMinMaxF <- setDT(dfMinMaxF, keep.rownames = TRUE)[]
  }
  
  
  for(i in 1:nbMod){
    
    indic <- colnames(pvs)[3 + i]
    indic2 <- colnames(pvs2)[3 + i]
    
    # Préparation du tableau de données à joindre au shape
    ## data shape stock
    dataShpProp <- pvs %>% 
      select(variable, CODE_SEC, indic) %>%
      pivot_wider(names_from = variable, values_from = indic, names_prefix = paste0(indic, "_")) %>% 
      rename(Secteur_EM = CODE_SEC)
    
    ### Reorder
    dataShpProp <- dataShpProp[ , c(1, 20:25, 2:24)]
    dataShpProp <- dataShpProp[ , 1:25]
    
    ## data shape part
    dataShpChoro <- pvs2 %>% 
      select(variable, CODE_SEC, indic2) %>%
      pivot_wider(names_from = variable, values_from = indic2, names_prefix = paste0(indic2, "_")) %>% 
      rename(Secteur_EM = CODE_SEC)
    
    ### Reorder
    dataShpChoro <- dataShpChoro[ , c(1, 20:25, 2:24)]
    dataShpChoro <- dataShpChoro[ , 1:25]
    
    ## Création des données pour les cartes en oursins
    if(nomIndic != "res"){
      ### Flowdata 
      flowdata <- prez_long %>% 
        filter(as.numeric(indicateur) == i) %>% 
        select(ID_IND, W_IND, variable, CODE_SEC, RES_SEC) %>%
        group_by(variable, CODE_SEC, RES_SEC) %>% 
        summarise(W_IND = sum(W_IND),
                  n = length(ID_IND)) %>% 
        filter(CODE_SEC != RES_SEC & n  >= 6) %>% 
        select(-n)
      
      ### Préparation des données pour les oursins
      flowdatasmp <- prez_long %>% 
        filter(as.numeric(indicateur) == i) %>% 
        select(ID_IND, variable, CODE_SEC, RES_SEC, W_IND) %>% 
        group_by(variable, CODE_SEC)
      
      
      ### Calcul de la population pondérée par secteur et par heure 
      ### (sans seuil à 12 personnes/secteur en non pondéré)
      flowdatasmpT <- flowdatasmp %>% 
        summarise(W_IND = sum(W_IND))
      
      ### Calcul de la population pondérée non résidente par secteur et par heure
      flowdatasmpNR <-  filter(flowdatasmp, CODE_SEC != RES_SEC) 
      flowdatasmpNR <- summarise(flowdatasmpNR, W_IND = sum(W_IND)) 
      
      ## Population pondérée par secteur et par heure totale (x) et non résidente (y)
      flowdatasmp <- left_join(flowdatasmpT, flowdatasmpNR, by = c("variable","CODE_SEC"))
      flowdatasmp <-  right_join(flowdatasmp, select(pvs3, variable, CODE_SEC), by = c("variable", "CODE_SEC")) # add empty sect
      flowdatasmp[is.na(flowdatasmp)] <- 0
      
      ### Wide
      flowdatasmp <- flowdatasmp %>% 
        select(variable, CODE_SEC, W_IND.y) %>% 
        pivot_wider(names_from = variable, values_from = W_IND.y, names_prefix = paste0(indic, "_")) %>% 
        rename(Secteur_EM = CODE_SEC)
      flowdatasmp <- flowdatasmp[ , c(1, 23:25, 13:15, 2:12, 16:22)]
      
      # Jointure des données aux fonds de carte
      shpSectDataFlow <- left_join(sfSec, flowdatasmp, by = "Secteur_EM")
      
      # Création des répertoires
      ## Répertoires parents (2 par indicateur)
      aa <- paste0(outputDir, "/", nomIndic, i ,"_flow")
      print("ICI")
      print(aa)
      dir.create(paste0(outputDir, "/", nomIndic, i ,"_flow"))
      
      ## Répertoires enfants (2 par répertoire parent)
      dir.create(paste0(outputDir, "/", nomIndic, i ,"_flow/geo"))
      dir.create(paste0(outputDir, "/", nomIndic, i ,"_flow/data"))
      
      # Export des données spatiales
      geojson_write(shpSectDataFlow,
                    file = paste0(outputDir, "/", nomIndic, i ,"_flow/geo/secteursData.geojson"))
      write.table(flowdata,
                  paste0(outputDir, "/", nomIndic, i ,"_flow/geo/flowData.csv"),
                  row.names = FALSE, sep = ",", dec = ".")
    }
    
    
    # Jointure des données aux fonds de carte
    shpSectDataProp <- left_join(sfSec, dataShpProp, by = "Secteur_EM")
    shpSectDataChoro <- left_join(sfSec, dataShpChoro, by = "Secteur_EM")
    
    
    # Création des répertoires
    ## Répertoires parents (2 par indicateur)
    dir.create(paste0(outputDir, "/", nomIndic, i ,"_prop"))
    dir.create(paste0(outputDir, "/", nomIndic, i ,"_choro"))
    
    
    ## Répertoires enfants (2 par répertoire parent)
    dir.create(paste0(outputDir, "/", nomIndic, i ,"_prop/geo"))
    dir.create(paste0(outputDir, "/", nomIndic, i ,"_choro/geo"))
    
    dir.create(paste0(outputDir, "/", nomIndic, i ,"_prop/data"))
    dir.create(paste0(outputDir, "/", nomIndic, i ,"_choro/data"))
    
    
    # Export des données spatiales
    # écriture avec geojson_write -> fichier moins lourd
    geojson_write(shpSectDataProp,
                  file = paste0(outputDir, "/", nomIndic, i ,"_prop/geo/secteursData.geojson"))
    geojson_write(shpSectDataChoro,
                  file = paste0(outputDir, "/", nomIndic, i ,"_choro/geo/secteursData.geojson"))
    
    
    
    # Création des tables pour le graphique simple
    ## STOCK
    dataSectProp <- as.data.frame(t(dataShpProp))
    names(dataSectProp) <- as.matrix(dataSectProp[1, ])
    dataSectProp <-  dataSectProp[-1, ]
    dataSectProp <- rownames_to_column(dataSectProp, "hour")
    dataSectProp$hour <- gsub(".*h",'',dataSectProp$hour)
    dataSectProp$hour <- ifelse(as.numeric(dataSectProp$hour) <= 12, paste0(dataSectProp$hour, 'am'), 
                                ifelse(as.numeric(dataSectProp$hour) >= 24, paste0((as.numeric(dataSectProp$hour) - 24), 'am'),
                                       paste0((as.numeric(dataSectProp$hour) - 12), 'pm')))
    dataSectProp$hour[dataSectProp$hour == "0am"] <- "12pm"
    # dataSectProp <- rbind(slice(dataSectProp, 19:24), slice(dataSectProp, 1:18))
    dataSectProp <- rbindlist(list(dataSectProp, dfMinMaxS))
    dataSectProp <- rbind(slice(dataSectProp, 25:26), slice(dataSectProp, 1:24))
    
    ## PART
    dataSectChoro <- as.data.frame(t(dataShpChoro))
    names(dataSectChoro) <- as.matrix(dataSectChoro[1, ])
    dataSectChoro <- dataSectChoro[-1, ]
    dataSectChoro <- rownames_to_column(dataSectChoro, "hour")
    dataSectChoro$hour <- gsub(".*h",'',dataSectChoro$hour)
    dataSectChoro$hour <- ifelse(as.numeric(dataSectChoro$hour) <= 12, paste0(dataSectChoro$hour, 'am'), 
                                 ifelse(as.numeric(dataSectChoro$hour) >= 24, paste0((as.numeric(dataSectChoro$hour) - 24), 'am'),
                                        paste0((as.numeric(dataSectChoro$hour) - 12), 'pm')))
    dataSectChoro$hour[dataSectChoro$hour == "0am"] <- "12pm"
    # dataSectChoro <- rbind(slice(dataSectChoro, 19:24), slice(dataSectChoro, 1:18))
    dataSectChoro <- rbindlist(list(dataSectChoro, dfMinMaxP))
    dataSectChoro <- rbind(slice(dataSectChoro, 25:26), slice(dataSectChoro, 1:24))
    
    ## Oursins
    if(nomIndic != "res"){
      dataSectFlow <- as.data.frame(t(flowdatasmp))
      names(dataSectFlow) <- as.matrix(dataSectFlow[1,])
      dataSectFlow <-  dataSectFlow[-1, ]
      dataSectFlow <- rownames_to_column(dataSectFlow, "hour")
      dataSectFlow$hour <- gsub(".*h",'',dataSectFlow$hour)
      dataSectFlow$hour <- ifelse(as.numeric(dataSectFlow$hour) <= 12, paste0(dataSectFlow$hour, 'am'), 
                                  ifelse(as.numeric(dataSectFlow$hour) >= 24, paste0((as.numeric(dataSectFlow$hour) - 24), 'am'),
                                         paste0((as.numeric(dataSectFlow$hour) - 12), 'pm')))
      dataSectFlow$hour[dataSectFlow$hour == "0am"] <- "12pm"
      dfh <- data.frame(hour = c("4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12am", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm", "12pm", "1am", "2am", "3am")) # au cas o? il  a des heures manquantes (cf shopping)
      dataSectFlow <- left_join(dfh, dataSectFlow, by = "hour")
      
      # dataSectFlow <- rbind(slice(dataSectFlow, 19:24), slice(dataSectFlow, 1:18))
      dfMinMaxF2 <- dfMinMaxF[,1:length(dataSectFlow)]
      dataSectFlow <- rbindlist(list(dataSectFlow, dfMinMaxF2))
      dataSectFlow <- rbind(slice(dataSectFlow, 25:26), slice(dataSectFlow, 1:24))
      # dataSectFlow <- rbind(slice(dataSectFlow, 1:2), slice(dataSectFlow, 9:26), slice(dataSectFlow, 3:8))
      dataSectFlow[is.na(dataSectFlow)] <- 0
      
      # Export des données
      write.csv2(dataSectFlow, 
                 paste0(outputDir, "/", nomIndic, i ,"_flow/data/dataSect.csv"),
                 row.names = FALSE)
    }
    
    
    # Export des données
    write.csv2(dataSectProp, 
               paste0(outputDir, "/", nomIndic, i ,"_prop/data/dataSect.csv"),
               row.names = FALSE)
    write.csv2(dataSectChoro, 
               paste0(outputDir, "/", nomIndic, i ,"_choro/data/dataSect.csv"),
               row.names = FALSE)
    
  }
  
}  

createISeg <- function(nbMod, nomIndic, nomEnq, data, ctry, sfSec, outputDir){

  
  pvs <- data[["pvs"]]
  pvs2 <- data[["pvs2"]]
  
  # DUNCAN
  ## Mise en forme du tableau de base
  tabISeg <- pvs 
  listHour <- unique(tabISeg$variable)
  duncan <- data.frame("hour" = character(24))
  
  for (i in listHour){
    
    duncan <- bind_rows(duncan, as.data.frame(t(ISDuncan(tabISeg[tabISeg$variable == i , 4:length(tabISeg)]))))
    
  }
  
  ## Ajout des heures
  duncan$hour <- listHour
  duncan$hour <- gsub(".*h",'',duncan$hour)
  duncan$hour <- ifelse(as.numeric(duncan$hour) <= 12, paste0(duncan$hour, 'am'), 
                        ifelse(as.numeric(duncan$hour) >= 24, paste0((as.numeric(duncan$hour) - 24), 'am'),
                               paste0((as.numeric(duncan$hour) - 12), 'pm')))
  duncan$hour[duncan$hour == "0am"] <- "12pm"
  
  ## suppression des lignes vides et reorder
  duncan <- filter(duncan[c(43:48,25:42), ]) 
  
  ## Correction si na (à valider)
  duncan[is.na(duncan)] <- 0
  
  ## Correction des noms de variable
  colnames(duncan)[2:length(duncan)] <- colnames(tabISeg[ , 4:length(tabISeg)])
  
  ## Pour le Canada rev5 en 2eme col
  if(ctry == "CA" & nomIndic == "rev") {
    duncan <- duncan %>% relocate(c("hour", "rev5"))
  }
  ## Pour Bogota mode4 en 2eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    duncan <- duncan %>% relocate(c("hour", "mode4"))
  }
  
  # MORAN
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
      filter(!Secteur_EM %in% c("UTAM700", "UTAM650", "UTAM680",
                                "UTAM563", "UTAM660", "UTAM690",
                                "UTAM670", "UTAM640", "UTAM540",
                                "UTAM500", "UTAM520", "UTAM580",
                                "UTAM620", "UTAM600", "UTAM590",
                                "UTAM640", "UTAM630", "UTAM610",
                                "UTAM89"))
  }
  
  if (nomEnq == "SAO PAULO") {
    shpSectMoran <- sfSec %>% 
      st_transform(crs = 22523)
  }
  
  if (nomEnq == "SANTIAGO") {
    shpSectMoran <- sfSec %>% 
      st_transform(crs = 32719)
  }
  
  
  ## Création d'un tableau vide
  moran <- data.frame("hour" = '', "var" = '', "moran" = numeric(24))
  
  for (i in listHour){
    
    # Trier les données
    dataMoran <- filter(pvs2 %>% select(-popSec), variable == i)
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
    
    # Calcul de l'indice de Moran
    for (j in colnames(dataMoran[ , 13:length(dataMoran)])){
      
      Moran <- moran.mc(x = dataMoran[[j]],
                        listw = nb2listw(nbSecteurs), nsim=1000)
      
      moran <- rbind(moran, data.frame("hour" = i,
                                       "var" = j,
                                       "moran" = Moran$statistic,
                                       stringsAsFactors = FALSE))
    }
    
  }
  
  ## Correction si na (à valider)
  moran[is.na(moran)] <- 0
  
  ## Mise en forme
  moran <- filter(moran, hour != '') # suppression des lignes vides
  
  ## mise en forme des heures
  moran$hour <- gsub(".*h",'',moran$hour)
  moran$hour <- ifelse(as.numeric(moran$hour) <= 12, paste0(moran$hour, 'am'), 
                       ifelse(as.numeric(moran$hour) >= 24, paste0((as.numeric(moran$hour) - 24), 'am'),
                              paste0((as.numeric(moran$hour) - 12), 'pm')))
  moran$hour[moran$hour == "0am"] <- "12pm"
  
  ## wide
  moran <- moran %>% 
    pivot_wider(names_from = var, values_from = moran)
  
  ## reorder
  desired_order <- c("4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12am", 
                     "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm", "12pm", 
                     "1am", "2am", "3am")
  moran$hour <- factor( as.character(moran$hour), levels=desired_order )
  moran <- moran[order(moran$hour), ]
  
  ## Pour le Canada rev5 en 2eme col
  if(ctry == "CA" & nomIndic == "rev") {
    moran <- moran %>% relocate(c("hour", "rev5"))
  }
  ## Pour Bogota mode4 en 2eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    moran <- moran %>% relocate(c("hour", "mode4"))
  }
  
  
  ## EXPORT 
  write.table(duncan, 
              paste0(outputDir, "/indice_segreg/", nomIndic,"_Duncan.csv"),
              row.names = FALSE, sep = ',', dec = '.')
  write.table(moran, 
              paste0(outputDir, "/indice_segreg/", nomIndic,"_Moran.csv"),
              row.names = FALSE, sep = ',', dec = '.')
  
  
} 

createStacked <- function(nbMod, nomIndic, nomEnq, ctry, outputDir){
  
  
  
  # 1. Cartes choro
  
  ## Création d'un tableau pour la construction d'histogrammes en barres empilées (stacked bar chart)
  listData <- list()
  
  for (i in 1:nbMod){
    
    assign(paste0("dataSect", i, "_choro"),
           read.csv(paste0(outputDir, "/", nomIndic, i, "_choro/data/dataSect.csv"),
                    sep = ";", dec= ".", stringsAsFactors = FALSE, header = TRUE, check.names = FALSE
           ))
    
    listData[[as.character(i)]] <- eval(parse(text = paste0("dataSect", i, "_choro")))
    
  }
  
  listData <- lapply(listData, function(x){
    
    # Filter out min and max
    x <- filter(x, hour != "min" & hour != "max")
    
    # From wide to long
    x <- x %>% pivot_longer(-hour, names_to = "secteur", values_to = "value")
    
    # Retourne les tableaux
    return(x)
  })
  
  ## On récupère les dataframes
  for (i in 1:nbMod){
    
    assign(paste0("dataSect", i, "_choro"), listData[[as.character(i)]])
    
  }
  
  ## Jointure des dataframes
  tabFin <- Reduce(function(x, y) merge(x, y, by = c("secteur", "hour"), all=TRUE), listData)
  varColNames <- character()
  
  for (i in 1:nbMod){
    varColNames <- c(varColNames, paste0(nomIndic, i))
  }
  
  colnames(tabFin) <- c("district", "hour", varColNames)
  
  ## Sort tableau
  desired_order <- c("4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12am", 
                     "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm", "12pm", 
                     "1am", "2am", "3am")
  tabFin$hour <- factor( as.character(tabFin$hour), levels=desired_order)
  tabFin <- tabFin[order(tabFin$hour), ]
  
  ## Pour le Canada rev5 en 3eme col
  if(ctry == "CA" & nomIndic == "rev") {
    tabFin <- tabFin %>% relocate(c("hour", "district", "rev5"))
  }
  ## Pour Bogota mode4 en 3eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    tabFin <- tabFin %>% relocate(c("hour", "district", "mode4"))
  }
  
  ## Export
  write.table(tabFin, paste0(outputDir, "/stacked/", nomIndic, "_choro_stacked.csv"),
              row.names = FALSE, sep = ",", dec = ".")
  
  # 2. Cartes Proportionnelles
  
  ## Création d'un tableau pour la construction d'histogrammes en barres empilées (stacked bar chart)
  listData <- list()
  
  for (i in 1:nbMod){
    
    assign(paste0("dataSect", i, "_prop"),
           read.csv(paste0(outputDir, "/", nomIndic, i, "_prop/data/dataSect.csv"),
                    sep = ";", dec= ".", stringsAsFactors = FALSE, header = TRUE, check.names = FALSE
           ))
    
    listData[[as.character(i)]] <- eval(parse(text = paste0("dataSect", i, "_prop")))
    
  }
  
  listData <- lapply(listData, function(x){
    
    # Filter out min and max
    x <- filter(x, hour != "min" & hour != "max")
    
    # From wide to long
    x <- x %>% pivot_longer(-hour, names_to = "secteur", values_to = "value")
    
    # Retourne les tableaux
    return(x)
    
  })
  
  ## On récupère les dataframes
  for (i in 1:nbMod){
    
    assign(paste0("dataSect", i, "_prop"), listData[[as.character(i)]])
    
  }
  
  ## Jointure des dataframes
  tabFin <- Reduce(function(x, y) merge(x, y, by = c("secteur", "hour"), all=TRUE), listData)
  varColNames <- character()
  
  for (i in 1:nbMod){
    varColNames <- c(varColNames, paste0(nomIndic, i))
  }
  
  colnames(tabFin) <- c("district", "hour", varColNames)
  
  ## Sort tableau
  desired_order <- c("4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12am", 
                     "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm", "12pm", 
                     "1am", "2am", "3am")
  tabFin$hour <- factor(as.character(tabFin$hour), levels=desired_order)
  tabFin <- tabFin[order(tabFin$hour), ]
  
  ## Pour le Canada rev5 en 3eme col
  if(ctry == "CA" & nomIndic == "rev") {
    tabFin <- tabFin %>% relocate(c("hour", "district", "rev5"))
  }
  ## Pour Bogota mode4 en 3eme col
  if(nomEnq == "BOGOTA" & nomIndic == "mode") {
    tabFin <- tabFin %>% relocate(c("hour", "district", "mode4"))
  }
  
  ## Export
  write.table(tabFin, paste0(outputDir, "/stacked/", nomIndic, "_prop_stacked.csv"),
              row.names = FALSE, sep = ",", dec = ".")
  
  # 3. Cartes en oursins
  if(nomIndic != "res"){
    
    ## Création d'un tableau pour la construction d'histogrammes en barres empilées (stacked bar chart)
    listData <- list()
    
    for (i in 1:nbMod){
      
      assign(paste0("dataSect", i, "_flow"),
             read.csv(paste0(outputDir, "/", nomIndic, i, "_flow/data/dataSect.csv"),
                      sep = ";", dec= ".", stringsAsFactors = FALSE, header = TRUE, check.names = FALSE
             ))
      
      listData[[as.character(i)]] <- eval(parse(text = paste0("dataSect", i, "_flow")))
      
    }
    
    listData <- lapply(listData, function(x){
      
      # Filter out min and max
      x <- filter(x, hour != "min" & hour != "max")
      
      # From wide to long
      x <- x %>% pivot_longer(-hour, names_to = "secteur", values_to = "value")
      
      # Retourne les tableaux
      return(x)
      
    })
    
    ## On récupère les dataframes
    for (i in 1:nbMod){
      
      assign(paste0("dataSect", i, "_flow"), listData[[as.character(i)]])
      
    }
    
    ## Jointure des dataframes
    tabFin <- Reduce(function(x, y) merge(x, y, by = c("secteur", "hour"), all=TRUE), listData)
    varColNames <- character()
    
    for (i in 1:nbMod){
      varColNames <- c(varColNames, paste0(nomIndic, i))
    }
    
    colnames(tabFin) <- c("district", "hour", varColNames)
    
    ## Sort tableau
    desired_order <- c("4am", "5am", "6am", "7am", "8am", "9am", "10am", "11am", "12am", 
                       "1pm", "2pm", "3pm", "4pm", "5pm", "6pm", "7pm", "8pm", "9pm", "10pm", "11pm", "12pm", 
                       "1am", "2am", "3am")
    tabFin$hour <- factor(as.character(tabFin$hour), levels=desired_order)
    tabFin <- tabFin[order(tabFin$hour), ]
    tabFin[is.na(tabFin)] <- 0 
    
    ## Pour le Canada rev5 en 2eme col
    if(ctry == "CA" & nomIndic == "rev") {
      tabFin <- tabFin %>% relocate(c("hour", "district", "rev5"))
    }
    ## Pour Bogota mode4 en 3eme col
    if(nomEnq == "BOGOTA" & nomIndic == "mode") {
      tabFin <- tabFin %>% relocate(c("hour", "district", "mode4"))
    }
    
    ## Export
    write.table(tabFin, paste0(outputDir, "/stacked/", nomIndic, "_flow_stacked.csv"),
                row.names = FALSE, sep = ",", dec = ".")
  }
  
  
}



# créa des dossiers de sortie
# dir.create(paste0("output/", nomEnq))
# dir.create(paste0("output/", nomEnq, "/indice_segreg"))
# dir.create(paste0("output/", nomEnq, "/stacked"))
