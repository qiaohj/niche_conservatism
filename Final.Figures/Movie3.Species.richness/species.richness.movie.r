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

#diversity<-readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", nb, da))
world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_last<-readRDS(sprintf("../Data/diversity/diversity_bootstrap_years/%d.rda", 0))
diversity_max<-diversity_last[,.(max_species=round(max(mean_N_SPECIES))), by=list(species_evo_type, directional_speed)]
y=1198
for (y in c(1199:0)){
  print(y)
  targetxxx<-sprintf("../Figures/Movie3.Species.Richness/years/%d.png", y)
  if (file.exists(targetxxx)){
    next()
  }
  saveRDS(NULL, targetxxx)
  if (y==1199){
    diversity_final<-readRDS("../Data/seeds.rda")
    outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
    length(unique(outliers$global_id))
    diversity_final<-diversity_final[!(global_id %in% outliers$global_id)]
    diversity_final$mean_N_SPECIES<-1
    diversity_final$medium_N_SPECIES<-1
    diversity_final$ci<-0
    diversity_final$sd_N_SPECIES<-0
    diversity_final$year<-1199
    bootstrap_seeds_all<-readRDS("../Data/diversity/bootstrap_seeds.rda")
    xx<-bootstrap_seeds_all[, .(N=.N), by=list(global_id, rep)]
    xx<-xx[, .(N=mean(N)), by=list(global_id)]
    diversity_final<-merge(diversity_final, xx, by="global_id", all=T)
    diversity_final$mean_N_SPECIES<-diversity_final$N
    diversity_final$N<-NULL
    
    coms<-data.table(species_evo_type=c(1, 2, 2, 3, 3, 4, 4, 5, 6, 7),
                     directional_speed=c(0, 0.1, 0.5, 0.1, 0.5, 0.1, 0.5, 0.01, 0.01, 0.01),
                     group=c("conservatism", "shift-directional", "shift-directional", 
                             "expansion-directional", "expansion-directional",
                             "expansion-omnidirectional", "expansion-omnidirectional",
                             "random", "random", "random"))
    diversity_final_list<-list()
    for (j in c(1:nrow(coms))){
      diversity_final$species_evo_type<-coms[j]$species_evo_type
      diversity_final$directional_speed<-coms[j]$directional_speed
      diversity_final$group<-coms[j]$group
      
      diversity_final_list[[length(diversity_final_list)+1]]<-diversity_final
    }
    diversity_final<-rbindlist(diversity_final_list)
    diversity_final$lon<-NULL
    diversity_final$lat<-NULL
    
    
    is_uniform<-F
  }else{
    diversity_final<-readRDS(sprintf("../Data/diversity/diversity_bootstrap_years/%d.rda", y))
    
    is_uniform<-F
  }
  diversity_final<-merge(diversity_final, ll, by="global_id", all=T)
  diversity_final<-merge(diversity_final, diversity_max, by=c("directional_speed", "species_evo_type"))
  diversity_final[is.na(mean_N_SPECIES)]$mean_N_SPECIES<-0
  diversity_final[is.na(medium_N_SPECIES)]$medium_N_SPECIES<-0
  
  #diversity_final
  #diversity_final[group=="conservatism" & medium_N_SPECIES==max(diversity_final[group=="conservatism"]$medium_N_SPECIES)]
  diversity_final$N_SPECIES<-round(diversity_final$mean_N_SPECIES)
  #diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
  #  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
  p1<-create_fig(diversity_final[group=="conservatism"], evo_types_label_line[1], polygon=polygon, 
                 legend_label = "species number", is_uniform = is_uniform)
  #p1
  
  p2_2<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.1], 
                   evo_types_label_line[2], polygon=polygon, is_uniform = is_uniform)
  p2_3<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.5], 
                   evo_types_label_line[3], polygon=polygon, is_uniform = is_uniform)
  
  
  p3_2<-create_fig(diversity_final[group=="expansion-directional" & directional_speed==0.1], 
                   evo_types_label_line[7], polygon=polygon, is_uniform = is_uniform)
  p3_3<-create_fig(diversity_final[group=="expansion-directional" & directional_speed==0.5], 
                   evo_types_label_line[8], polygon=polygon, is_uniform = is_uniform)
  
  
  p4_2<-create_fig(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.1], 
                   evo_types_label_line[9], polygon=polygon, is_uniform = is_uniform)
  p4_3<-create_fig(diversity_final[group=="expansion-omnidirectional" & directional_speed==0.5], 
                   evo_types_label_line[10], polygon=polygon, is_uniform = is_uniform)
  #5: random-central
  #6: random-symmetrical
  #7: random-asymmetrical
  p5_1<-create_fig(diversity_final[group=="random" & species_evo_type==5], 
                   evo_types_label_line[4], polygon=polygon, is_uniform = is_uniform)
  p5_2<-create_fig(diversity_final[group=="random" & species_evo_type==6],
                   evo_types_label_line[5], polygon=polygon, is_uniform = is_uniform)
  p5_3<-create_fig(diversity_final[group=="random" & species_evo_type==7],
                   evo_types_label_line[6], polygon=polygon, is_uniform = is_uniform)
  
  p2<-ggarrange(plotlist=list(p2_2, p3_2, p4_2), nrow=1)
  p3<-ggarrange(plotlist=list(p2_3, p3_3, p4_3), nrow=1)
  
  p5<-ggarrange(plotlist=list(p5_1, p5_2, p5_3), nrow=1)
  pp<-ggarrange(plotlist=list(p1, p2, p3, p5), nrow=4, 
                heights = c(2, 1,1,1))
  pp<-annotate_figure(pp, fig.lab.pos = "top.left", 
                      fig.lab = sprintf("%s %s", as.character(y/10), x_label),
                      fig.lab.size=15, fig.lab.face="bold")
  
  ggsave(pp, filename=targetxxx, 
         width=12, height=12, bg="white")
  
  
}


#cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Movie3.Species.Richness/years
#ls | xargs -I {} mv {} y-{}

#ffmpeg -r 20 -start_number -1200 -i y%d.png -y ../species.richness.mp4
