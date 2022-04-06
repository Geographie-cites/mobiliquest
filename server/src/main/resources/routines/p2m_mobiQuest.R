# MobiQuest
# Adaptation p2m pour requêtes
# AD - février 2022

# dossier de sortie

# choix des params

## Ville
nomEnq = "ALBI"

## requête : changement du périmètre observé, exemple :
perim <- c(3,1)
## requête : sous-population avec n filtre, exemple :
subpop <- list("SEX" = "2", "EDUC" = "4", "STRM" = "1")

# une ED
T1<-Sys.time()
p2m(nomEnq, perim, subpop, cheminIn, cheminOut)
T2<-Sys.time()
Tdiff= difftime(T2, T1)








