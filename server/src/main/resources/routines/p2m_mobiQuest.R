# MobiQuest
# Adaptation p2m pour requêtes
# AD - février 2022

# load p2m functions
suppressWarnings(source("p2m_fct_mobiQuest.R"))

# dossier d'entrée
cheminIn <- "../../../../../../../data/BD_presence_utile"
# dossier de sortie
cheminOut <- "../../../../../../../data_web/data/"

# choix des params

## Ville
nomEnq = "ALBI"

## requête : changement du périmètre observé, exemple :
perim <- c(3, 1)
## requête : sous-population avec n filtre, exemple :
# subpop <- list("SEX" = "2", "EDUC" = "4", "STRM" = "1")
subpop <- list("SEX" = "2", "KAGE" = c("1","2"))

# une ED
T1<-Sys.time()
p2m(nomEnq, perim, subpop, cheminIn, cheminOut)
T2<-Sys.time()
Tdiff= difftime(T2, T1)








