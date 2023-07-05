library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
#library(plotKML)
library(ggtree)
library(phylobase)
library(ggpubr)
library(heatmaply)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
setDTthreads(20)
#source("commons/diverging_map.r")
polygon<-readRDS("../Figures/Movie2.Example/polygon.rda")
world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

year<-0


diversity_final<-read_sf("../Shape/IUCN_Richness/Birds/richness.shp")
diversity_final$geometry<-NULL
diversity_final<-data.table(diversity_final)
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_final<-merge(diversity_final, ll, by="global_id", all=F)
diversity_final[is.na(richness)]$richness<-0

diversity_final$N_SPECIES<-diversity_final$richness
#diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
#  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
p1<-create_fig(diversity_final, "Birds")
hist(diversity_final$N_SPECIES)
p1
ggsave(p1, filename="../Figures/Figure6.Species.Richness/species.richness.bird.png", bg="white", width=8, height=4)
ggsave(p1, filename="../Figures/Figure6.Species.Richness/species.richness.bird.pdf", bg="white", width=8, height=4)

diversity_final<-read_sf("../Shape/IUCN_Richness/Mammals/richness.shp")
diversity_final$geometry<-NULL
diversity_final<-data.table(diversity_final)
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_final<-merge(diversity_final, ll, by="global_id", all=F)
diversity_final[is.na(richness)]$richness<-0

diversity_final$N_SPECIES<-diversity_final$richness
#diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
#  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
p2<-create_fig(diversity_final, "Mammals")
ggsave(p2, filename="../Figures/Figure6.Species.Richness/species.richness.mammal.png", bg="white", width=8, height=4)
ggsave(p2, filename="../Figures/Figure6.Species.Richness/species.richness.mammal.pdf", bg="white", width=8, height=4)
p<-ggarrange(p1, p2, nrow=2, ncol=1)
ggsave(p, filename="../Figures/Figure6.Species.Richness/species.richness.iucn.png", bg="white", width=8, height=8)
ggsave(p, filename="../Figures/Figure6.Species.Richness/species.richness.iucn.pdf", bg="white", width=8, height=8)
