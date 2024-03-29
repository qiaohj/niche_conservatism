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
#source("commons/diverging_map.r")
polygon<-readRDS("../Figures/Example/polygon.rda")
#diversity<-readRDS("../Data/diversity.rda")
nb<-"BROAD"
da<-"GOOD"
#diversity<-readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", nb, da))
world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

year<-0


if (F){
  diversity<-readRDS("../Data/diversity.rda")
  
  diversity_final<-diversity[year==0]
  diversity_final$group<-"random"
  diversity_final[species_evo_type==1]$group<-"conservatism"
  diversity_final[species_evo_type==2]$group<-"shift-directional"
  diversity_final[species_evo_type==3]$group<-"expansion-directional"
  diversity_final[species_evo_type==4]$group<-"expansion-omnidirectional"
  saveRDS(diversity_final, "../Data/diversity_last_year.rda")
}
diversity_final<-readRDS("../Data/diversity_last_year.rda")

item_y<-diversity_final[group=="conservatism"]
label<-"conservatism"
barwidth=10
get_quantile<-function(values, checked_value){
  match(checked_value, sort(values)) / (length(values) + 1) * 100
}
get_quantile(item_y$N_SPECIES, max(item_y$N_SPECIES))
get_quantile(item_y$N_SPECIES, min(item_y$N_SPECIES))
get_quantile(item_y$N_SPECIES, median(item_y$N_SPECIES))
hist(get_quantile(item_y$N_SPECIES, item_y$N_SPECIES))



p1<-create_fig(diversity_final[group=="conservatism"], "conservatism", legend_position="bottom")
p2_1<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.01], 
                 "shift-directional (0.01)")
p2_2<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.1], 
                 "shift-directional (0.1)")
p2_3<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.5], 
                 "shift-directional (0.5)")

p3_1<-create_fig(diversity_final[group=="expansion-directional" & directional_speed==0.01], 
                 "expansion-directional (0.01)")
p3_2<-create_fig(diversity_final[group=="expansion-directional" & directional_speed==0.1], 
                 "expansion-directional (0.1)")
p3_3<-create_fig(diversity_final[group=="expansion-directional" & directional_speed==0.5], 
                 "expansion-directional (0.5)")

p4_1<-create_fig(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.01], 
                 "expansion-omnidirectional (0.01)")
p4_2<-create_fig(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.1], 
                 "expansion-omnidirectional (0.1)")
p4_3<-create_fig(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.5], 
                 "expansion-omnidirectional (0.5)")
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
p5_1<-create_fig(diversity_final[group=="random" & species_evo_type==5], 
                 "random-central")
p5_2<-create_fig(diversity_final[group=="random" & species_evo_type==6],
                 "random-symmetrical")
p5_3<-create_fig(diversity_final[group=="random" & species_evo_type==7],
                 "random-asymmetrical")

p2<-ggarrange(plotlist=list(p2_1, p2_2, p2_3), nrow=1)
p3<-ggarrange(plotlist=list(p3_1, p3_2, p3_3), nrow=1)
p4<-ggarrange(plotlist=list(p4_1, p4_2, p4_3), nrow=1)
p5<-ggarrange(plotlist=list(p5_1, p5_2, p5_3), nrow=1)
pp<-ggarrange(plotlist=list(p1, p2, p3, p4, p5), nrow=5, heights = c(2, 1,1,1,1))
ggsave(pp, filename="../Figures/FigureS1.Diversity/fig.s1.png", width=12, height=16, bg="white")

