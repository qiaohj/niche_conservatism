library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
#library(ggspatial)
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
p1<-create_fig_map(diversity_final[group=="conservatism"], label=evo_types_label_line[1], polygon=NULL)
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


diversity_bird<-read_sf("../Shape/IUCN_Richness/Birds/richness.shp")
diversity_bird$geometry<-NULL
diversity_bird<-data.table(diversity_bird)
#diversity_bird<-merge(diversity_bird, ll, by="global_id", all=F)
diversity_bird[is.na(richness)]$richness<-0
diversity_bird$N_SPECIES<-diversity_bird$richness


p_bird<-create_fig_map(diversity_bird, 
                     "Birds", polygon=NULL, with_label=T)
p_bird
  
diversity_mammal<-read_sf("../Shape/IUCN_Richness/Mammals/richness.shp")
diversity_mammal$geometry<-NULL
diversity_mammal<-data.table(diversity_mammal)
#diversity_mammal<-merge(diversity_mammal, ll, by="global_id", all=F)
diversity_mammal[is.na(richness)]$richness<-0
diversity_mammal$N_SPECIES<-diversity_mammal$richness


p_mammal<-create_fig_map(diversity_mammal, 
                       label="Mammals", polygon=NULL, with_label=T)
p_mammal

ggsave(p_bird, filename="../Figures/Figure6.Species.Richness/species.richness.bird.map.png")
ggsave(p_mammal, filename="../Figures/Figure6.Species.Richness/species.richness.mammal.map.png")

pp1<-ggarrange(plotlist=list(p1, p_bird, p_mammal), nrow=1)
p2<-ggarrange(plotlist=list(p2_2, p3_2, p4_2), nrow=1)
p3<-ggarrange(plotlist=list(p2_3, p3_3, p4_3), nrow=1)

p5<-ggarrange(plotlist=list(p5_1, p5_2, p5_3), nrow=1)
pp<-ggarrange(plotlist=list(pp1,  NULL, 
                            p2, p3, p5), nrow=5, heights = c(1, -0.05, 1,1,1))
ggsave(pp, filename="../Figures/Figure6.Species.Richness/Species.richness.full.map.png", 
       width=16, height=10, bg="white")

ggsave(pp, filename="../Figures/Figure6.Species.Richness/Species.richness.full.map.pdf", 
       width=16, height=10, bg="white")
