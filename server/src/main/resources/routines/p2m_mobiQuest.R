# MobiQuest
# Adaptation p2m pour requêtes
# AD - février 2022

setwd("~/00_MobiQuest/git/mobiliquest/server/src/main/resources/routines")



# choix des params

## Ville
nomEnq = "ALBI"

## requête : changement du périmètre observé, exemple :
perim <- c(1, 2, 3)
perim <- c(6, 7)
perim <- c()
## requête : sous-population avec n filtre, exemple :
subpop <- list( "ZONAGE" = c() , "QPV" = c(2) , "SEX" = c() , "EDUCMEN" = c() , "CSP" = c() , "EDUC" = c() , "CSPMEN" = c() , "STRM" = c() , "OCC" = c() , "KAGE" = c() )
subpop <- list( "ZONAGE" = c() , "QPV" = c() , "SEX" = c() , "EDUCMEN" = c(2, 3, 4) , "CSP" = c() , "EDUC" = c() , "CSPMEN" = c() , "STRM" = c() , "OCC" = c() , "KAGE" = c() )

# dossier d'entrée
cheminIn <- "../../../../../../../data/BD_presence_utile"
# dossier de sortie
cheminOut <- paste0("../../../../../../../data_web/data/", nomEnq, "/")


# load p2m functions
suppressWarnings(source("p2m_fct_mobiQuest.R"))

p2m("ALBI", 
    c(), list( "ZONAGE" = c() , "QPV" = c() , "SEX" = c() , "EDUCMEN" = c(2, 3, 4) , "CSP" = c() , "EDUC" = c() , "CSPMEN" = c() , "STRM" = c() , "OCC" = c() , "KAGE" = c() ),
    cheminIn, cheminOut)

# une ED
T1<-Sys.time()
p2m(nomEnq, perim, subpop, cheminIn, cheminOut)
T2<-Sys.time()
(Tdiff= difftime(T2, T1))






