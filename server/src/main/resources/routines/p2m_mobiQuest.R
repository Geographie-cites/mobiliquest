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
nomEnq = "ALBI"

## requête : changement du périmètre observé, exemple :
perim <- c()
## requête : sous-population avec n filtre, exemple :
# subpop <- list("SEX" = "2", "EDUC" = "4", "STRM" = "1")
subpop <- list("SEX" = "2", "KAGE" = c("1","2"))
subpop <- list("OCC" = c() , "CSPMEN" =  c()  , "QPV" = c() , 
               "CSP" = c() , "SEX" =  c(2)  , "ZONAGE" =  c(1,2) , "KAGE" =  c()  ,
               "EDUC" =  c(4)  , "EDUCMEN" =  c(3)  , "STRM" =  c())

# load p2m functions
suppressWarnings(source("p2m_fct_mobiQuest.R"))



# une ED
T1<-Sys.time()
p2m(nomEnq, perim, subpop, cheminIn, cheminOut)
T2<-Sys.time()
(Tdiff= difftime(T2, T1))






