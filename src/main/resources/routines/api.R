source(file = "/home/mathieu/work/cogit/mobiliquest/src/main/resources/routines/extrait_p2m.R")


computeAll <- function(nbMod, nomEnq, longFilePath, secFilePath, outputDir){

    prez_long <- read.csv2(longFilePath,
                                       colClasses = c(rep("character", 5), rep("numeric", 2),
                                                      rep("character", 5), "numeric",
                                                      rep("character", 6), "numeric",
                                                      rep("character", 19)))

    sfSec <- st_read(secFilePath)

    ageIndicators(4, nomEnq, prez_long, sfSec, outputDir)
}


# Préparation des données de l'indicateur AGE ----
ageIndicators <- function(nbMod, study, prez_long, sfSec, outputDir){

    #nbMod <- 4
    nomIndic <- "age"
    indicateur <- prez_long$KAGE

    dir.create(outputDir, recursive = TRUE)
    dir.create(paste0(outputDir, "/indice_segreg"))
    dir.create(paste0(outputDir, "/stacked"))

    ## prep data
    data <- prepAge(nomEnq = study, prez_long)

    # CREATION DES FICHIERS POUR LA CARTOGRAPHIE ET LE GRAPHIQUE "SIMPLE" :
    createFiles(nbMod, indicateur, nomIndic, nomEnq = study, data, prez_long, sfSec, outputDir)

    # CREATION DES FICHIERS POUR LE GRAPHIQUE SEGREGATION (INDICES DE DUNCAN ET DE MORAN)
    createISeg(nbMod, nomIndic, nomEnq = study , data, ctry = "FR", sfSec, outputDir)

    # CREATION DES FICHIERS POUR LE GRAPHIQUE "STACKED"
    createStacked(nbMod, nomIndic, nomEnq = study, ctry = "FR", outputDir)
}