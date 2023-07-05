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
library(data.table)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
setDTthreads(20)
#source("commons/diverging_map.r")
polygon<-readRDS("../Figures/Movie2.Example/polygon.rda")
#diversity<-readRDS("../Data/diversity.rda")
nb<-"BROAD"
da<-"GOOD"
#diversity<-readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", nb, da))
world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

year<-0


if (F){
  library(data.table)
  library(RSQLite)
  library(DBI)
  setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
  base_db<-"../Configuration/conf.sqlite"
  mydb <- dbConnect(RSQLite::SQLite(), base_db)
  #mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
  simulations<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb)
  simulations<-data.table(simulations)
  simulations<-simulations[is_run==1]
  combinations<-simulations[, .(N=.N), by=list(nb, da, species_evo_type, directional_speed)]
  combinations<-combinations[((directional_speed %in% c(0) & species_evo_type==1) |
                                (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                                (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
  
  bootstrap_seeds_all<-readRDS("../Data/diversity/bootstrap_seeds.rda")
  template<-"%d_%s_%s_%d_%s_%d"
  j=1
  i=1
  base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
  
  rrrr<-51
  
  for (rrrr in c(1:100)){
    target<-sprintf("../Data/diversity_boostrap_items_full_year/r_%d", rrrr)
    if (dir.exists(target)){
      next()
    }
    dir.create(target)
    
    bootstrap_seeds<-bootstrap_seeds_all[rep==rrrr]
    d_year<-list()
    for (y in c(0:1198)){
      d_year[[as.character(y)]]<-list()
    }
    for (i in c(1:nrow(bootstrap_seeds))){
      seed<-bootstrap_seeds[i]
      
      for (j in c(1:nrow(combinations))){
        
        com<-combinations[j]
        print(paste("rep", rrrr, i, nrow(bootstrap_seeds), j, nrow(combinations),
                    seed$global_id, com$da, com$nb, com$species_evo_type, 
                    com$directional_speed))
        sp<-sprintf(template, seed$global_id, com$da, com$nb, com$species_evo_type, 
                    com$directional_speed, 0)
        ttt<-sprintf("%s/%s/%s.diversity.rda", base, sp, sp)
        df_full<-readRDS(ttt)
        
        if (is.null(df_full)){
          next()
        }
        years<-unique(df_full$year)
        for (y in years){
          df_item<-df_full[year==y]
          if (nrow(df_item)==0){
            next()
          }
          
          #range(df_item$year)
          df_item$seed_id<-seed$global_id
          df_item$da<-com$da
          df_item$nb<-com$nb
          df_item$species_evo_type<-com$species_evo_type
          df_item$directional_speed<-com$directional_speed 
          d_year[[as.character(y)]][[length(d_year[[as.character(y)]])+1]]<-df_item
        }
        
      }
      
    }
    for (y in c(0:1198)){
      setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
      print(paste("Saving ", y))
      dddd<-d_year[[as.character(y)]]
      
      dddd<-rbindlist(dddd)
      diversity_se<-dddd[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                         by=list(year, global_id, 
                                 species_evo_type, directional_speed)]
      diversity_se$rep<-rrrr
      saveRDS(diversity_se, sprintf("%s/y%d_r%d.rda", target, y, rrrr))
      diversity_se_nb<-dddd[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                            by=list(year, global_id, 
                                    species_evo_type, directional_speed, nb)]
      diversity_se_nb$rep<-rrrr
      saveRDS(diversity_se_nb, sprintf("%s/y%d_r%d_nb.rda", target, y, rrrr))
      diversity_se_nb_da<-dddd[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                               by=list(year, global_id, 
                                       species_evo_type, directional_speed, nb, da)]
      
      diversity_se_nb_da$rep<-rrrr
      saveRDS(diversity_se_nb_da, sprintf("%s/y%d_r%d_nb_da.rda", target, y, rrrr))
    }
  }
  
}
if (F){
  dddd<-list()
  dddd_nb<-list()
  dddd_nb_da<-list()
  i=1
  for (i in c(1:100)){
    print(i)
    target<-sprintf("../Data/diversity_boostrap_items_full_year/%d.rda", i)
    item<-readRDS(target)
    dddd[[length(dddd)+1]]<-item
    
    target_nb<-sprintf("../Data/diversity_boostrap_items_full_year/%d_nb.rda", i)
    item_nb<-readRDS(target_nb)
    dddd_nb[[length(dddd_nb)+1]]<-item_nb
    
    target_nb_da<-sprintf("../Data/diversity_boostrap_items_full_year/%d_nb_da.rda", i)
    item_nb_da<-readRDS(target_nb_da)
    dddd_nb_da[[length(dddd_nb_da)+1]]<-item_nb_da
  }
  dddd<-rbindlist(dddd)
  dddd_se<-dddd[, .(mean_N_SPECIES=mean(N_SPECIES), medium_N_SPECIES=quantile(N_SPECIES, 0.5),
                    sd_N_SPECIES=sd(N_SPECIES)),
                by=list(global_id, species_evo_type, directional_speed, year)]
  dddd_se$group<-"random"
  dddd_se[species_evo_type==1]$group<-"conservatism"
  dddd_se[species_evo_type==2]$group<-"shift-directional"
  dddd_se[species_evo_type==3]$group<-"expansion-directional"
  dddd_se[species_evo_type==4]$group<-"expansion-omnidirectional"
  saveRDS(dddd_se, "../Data/diversity/diversity_bootstrap_full.rda")
  
  dddd_nb<-rbindlist(dddd_nb)
  dddd_nb_se<-dddd_nb[, .(mean_N_SPECIES=mean(N_SPECIES), medium_N_SPECIES=quantile(N_SPECIES, 0.5),
                          sd_N_SPECIES=sd(N_SPECIES)),
                      by=list(global_id, species_evo_type, directional_speed, nb, year)]
  dddd_nb_se$group<-"random"
  dddd_nb_se[species_evo_type==1]$group<-"conservatism"
  dddd_nb_se[species_evo_type==2]$group<-"shift-directional"
  dddd_nb_se[species_evo_type==3]$group<-"expansion-directional"
  dddd_nb_se[species_evo_type==4]$group<-"expansion-omnidirectional"
  saveRDS(dddd_nb_se, "../Data/diversity/diversity_bootstrap_nb_full.rda")
  
  dddd_nb_da<-rbindlist(dddd_nb_da)
  dddd_nb_da_se<-dddd_nb_da[, .(mean_N_SPECIES=mean(N_SPECIES), medium_N_SPECIES=quantile(N_SPECIES, 0.5),
                                sd_N_SPECIES=sd(N_SPECIES)),
                            by=list(global_id, species_evo_type, directional_speed, nb, da, year)]
  dddd_nb_da_se$group<-"random"
  dddd_nb_da_se[species_evo_type==1]$group<-"conservatism"
  dddd_nb_da_se[species_evo_type==2]$group<-"shift-directional"
  dddd_nb_da_se[species_evo_type==3]$group<-"expansion-directional"
  dddd_nb_da_se[species_evo_type==4]$group<-"expansion-omnidirectional"
  saveRDS(dddd_nb_da_se, "../Data/diversity/diversity_bootstrap_nb_da_full.rda")
  
}
source("commons/functions.r")


diversity_final<-readRDS("../Data/diversity/diversity_bootstrap_full.rda")
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_final<-merge(diversity_final, ll, by="global_id", all=T)
diversity_final[is.na(mean_N_SPECIES)]$mean_N_SPECIES<-0
diversity_final[is.na(medium_N_SPECIES)]$medium_N_SPECIES<-0

diversity_final
diversity_final[group=="conservatism" & medium_N_SPECIES==max(diversity_final[group=="conservatism"]$medium_N_SPECIES)]
diversity_final$N_SPECIES<-round(diversity_final$mean_N_SPECIES)
#diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
#  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
p1<-create_fig(diversity_final[group=="conservatism" & year==1200], label="Niche conservatism", 
               barwidth=10, with_label=T, legend_label="", polygon)

ggsave(p1, filename="../Figures/Diversity/Diversity.conservatism_bootstrap_mean.png", bg="white", width=8, height=4)
#p2_1<-create_fig(diversity_final[group=="shift-directional" & directional_speed==0.01], 
#                 "shift-directional (0.01)")
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

p2<-ggarrange(plotlist=list(p2_2, p3_2, p4_2), nrow=1)
p3<-ggarrange(plotlist=list(p2_3, p3_3, p4_3), nrow=1)

p5<-ggarrange(plotlist=list(p5_1, p5_2, p5_3), nrow=1)
pp<-ggarrange(plotlist=list(p1, p2, p3, p5), nrow=4, heights = c(2, 1,1,1))
ggsave(pp, filename="../Figures/Diversity/Diversity.bootstrap_mean.without_IQR_outliers.png", width=12, height=12, bg="white")

