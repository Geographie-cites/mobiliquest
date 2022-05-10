# MobiQuest
# Adaptation p2m pour requêtes
# AD - février 2022

setwd("~/00_MobiQuest/git/mobiliquest/server/src/main/resources/routines")

# dossier d'entrée
cheminIn <- "../../../../../../../data/BD_presence_utile"
# dossier de sortie
cheminOut <- "../../../../../../../data_web/data/"

# choix des params

## Ville
nomEnq = "IDF"

## requête : changement du périmètre observé, exemple :
perim <- c(1, 2, 3)
perim <- c()
## requête : sous-population avec n filtre, exemple :
# subpop <- list("SEX" = "2", "EDUC" = "4", "STRM" = "1")
# subpop <- list("SEX" = "2", "KAGE" = c("1","2"))
subpop <- list("OCC" = c(1, 2, 3, 4, 5) , "CSPMEN" =  c(1, 2, 3, 4, 5)  , "QPV" = c(1, 2) , 
               "CSP" = c(1, 2, 3, 4, 5) , "SEX" =  c(1, 2)  , "ZONAGE" =  c(1, 2, 3) , 
               "KAGE" =  c(1, 2, 3, 4), "EDUC" =  c(1, 2, 3, 4), "EDUCMEN" =  c(1, 2, 3, 4), 
               "STRM" =  c(1, 2, 3, 4), "REV" = c(), "DEP" = c(1, 2, 3, 4, 5))

# load p2m functions
suppressWarnings(source("p2m_fct_mobiQuest.R"))



# une ED
T1<-Sys.time()
p2m(nomEnq, perim, subpop, cheminIn, cheminOut)
T2<-Sys.time()
(Tdiff= difftime(T2, T1))






