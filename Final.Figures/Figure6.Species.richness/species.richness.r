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
library(plotKML)
library(ggtree)
library(phylobase)
library(ggpubr)
library(heatmaply)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
setDTthreads(20)
source("commons/functions.r")
polygon<-readRDS("../Figures/Movie2.Example/polygon.rda")
#diversity<-readRDS("../Data/diversity.rda")
nb<-"BROAD"
da<-"GOOD"
#diversity<-readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", nb, da))
world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

year<-0

item_y<-diversity_final[group=="conservatism"]
label<-"conservatism"


diversity_final<-readRDS("../Data/diversity/diversity_bootstrap.rda")
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_final<-merge(diversity_final, ll, by="global_id", all=T)
diversity_final[is.na(mean_N_SPECIES)]$mean_N_SPECIES<-0
diversity_final[is.na(medium_N_SPECIES)]$medium_N_SPECIES<-0

diversity_final
diversity_final[group=="conservatism" & medium_N_SPECIES==max(diversity_final[group=="conservatism"]$medium_N_SPECIES)]
diversity_final$N_SPECIES<-round(diversity_final$mean_N_SPECIES)
#diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
#  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
p1<-create_fig(diversity_final[group=="conservatism"], "Niche conservatism", polygon=polygon, 
               legend_label = "species number")
p1
ggsave(p1, filename="../Figures/Figure6.Species.Richness/richness.conservatism.png", 
       bg="white", width=8, height=4)

p2_2<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.1], 
                 "shift-directional (0.1)", polygon=polygon)
p2_3<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.5], 
                 "shift-directional (0.5)", polygon=polygon)


p3_2<-create_fig(diversity_final[group=="expansion-directional" & directional_speed==0.1], 
                 "expansion-directional (0.1)", polygon=polygon)
p3_3<-create_fig(diversity_final[group=="expansion-directional" & directional_speed==0.5], 
                 "expansion-directional (0.5)", polygon=polygon)


p4_2<-create_fig(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.1], 
                 "expansion-omnidirectional (0.1)", polygon=polygon)
p4_3<-create_fig(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.5], 
                 "expansion-omnidirectional (0.5)", polygon=polygon)
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
p5_1<-create_fig(diversity_final[group=="random" & species_evo_type==5], 
                 "random-central", polygon=polygon)
p5_2<-create_fig(diversity_final[group=="random" & species_evo_type==6],
                 "random-symmetrical", polygon=polygon)
p5_3<-create_fig(diversity_final[group=="random" & species_evo_type==7],
                 "random-asymmetrical", polygon=polygon)

p2<-ggarrange(plotlist=list(p2_2, p3_2, p4_2), nrow=1)
p3<-ggarrange(plotlist=list(p2_3, p3_3, p4_3), nrow=1)

p5<-ggarrange(plotlist=list(p5_1, p5_2, p5_3), nrow=1)
pp<-ggarrange(plotlist=list(p1, p2, p3, p5), nrow=4, heights = c(2, 1,1,1))
ggsave(pp, filename="../Figures/Figure6.Species.Richness/Species.richness.full.png", 
       width=12, height=12, bg="white")


