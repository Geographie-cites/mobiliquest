# MobiQuest
# Adaptation p2m pour requêtes
# AD - février 2022


# directory
setwd("~/R/mobiliscope/")

# library
library(tidylog)
library(tidyverse)
library(sf)
library(geojsonio)
library(geojsonsf)
library(OasisR) # Duncan
library(spdep) # Moran



# load p2m functions
suppressWarnings(source("prepa_data/scriptsr/p2m_fct_mobiQuest.R"))

# dossier de sortie
chemin <- "prepa_data/data/data_web/data/"
dir.create(chemin)

# choix des params

## Ville
nomEnq = "ALBI"

## requête : changement du périmètre observé
# filter secteurs selon requête : 
# ville centre = 3
# ville centre + zone urbaine = c(3, 2)
# all = c(3, 2, 1)
perim <- c(4, 3)
subpop <- c("SEX", "2")

# une ED
T1<-Sys.time()
p2m(nomEnq, perim, subpop, chemin)
T2<-Sys.time()
Tdiff= difftime(T2, T1)








