# MobiQuest
# Adaptation p2m pour requêtes
# AD - février 2022

# dossier de sortie

# choix des params

## Ville
nomEnq = "ALBI"

## requête : changement du périmètre observé
# filter secteurs selon requête : 
# ville centre = 3
# ville centre + zone urbaine = c(3, 2)
# all = c(3, 2, 1)
perim <- c(3,1)
subpop <- c("SEX", "2")

# une ED
T1<-Sys.time()
p2m(nomEnq, perim, subpop, cheminIn, cheminOut)
T2<-Sys.time()
Tdiff= difftime(T2, T1)








