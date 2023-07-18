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
p1<-create_fig_map(diversity_final[group=="conservatism"], label=evo_types_label_line[1], polygon=NULL, 
               legend_label = "species number")
p1
ggsave(p1, filename="../Figures/Figure6.Species.Richness/richness.conservatism.map.png", 
       bg="white", width=8, height=4)

p2_2<-create_fig_map(diversity_final[group=="shift-directional" & directional_speed==0.1], 
                     label=evo_types_label_line[2], polygon=NULL)
p2_3<-create_fig_map(diversity_final[group=="shift-directional" & directional_speed==0.5], 
                     label=evo_types_label_line[3], polygon=NULL)


p3_2<-create_fig_map(diversity_final[group=="expansion-directional" & directional_speed==0.1], 
                     label=evo_types_label_line[7], polygon=NULL)
p3_3<-create_fig_map(diversity_final[group=="expansion-directional" & directional_speed==0.5], 
                     label=evo_types_label_line[8], polygon=NULL)


p4_2<-create_fig_map(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.1], 
                     label=evo_types_label_line[9], polygon=NULL)
p4_3<-create_fig_map(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.5], 
                     label=evo_types_label_line[10], polygon=NULL)
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
p5_1<-create_fig_map(diversity_final[group=="random" & species_evo_type==5], 
                 evo_types_label_line[4], polygon=NULL)
p5_2<-create_fig_map(diversity_final[group=="random" & species_evo_type==6],
                 evo_types_label_line[5], polygon=NULL)
p5_3<-create_fig_map(diversity_final[group=="random" & species_evo_type==7],
                 evo_types_label_line[6], polygon=NULL)

p2<-ggarrange(plotlist=list(p2_2, p3_2, p4_2), nrow=1)
p3<-ggarrange(plotlist=list(p2_3, p3_3, p4_3), nrow=1)

p5<-ggarrange(plotlist=list(p5_1, p5_2, p5_3), nrow=1)
pp<-ggarrange(plotlist=list(p1, p2, p3, p5), nrow=4, heights = c(2, 1,1,1))
ggsave(pp, filename="../Figures/Figure6.Species.Richness/Species.richness.full.map.png", 
       width=10, height=12, bg="white")

ggsave(pp, filename="../Figures/Figure6.Species.Richness/Species.richness.full.map.pdf", 
       width=10, height=12, bg="white")
