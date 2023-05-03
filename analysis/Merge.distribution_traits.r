library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
library(tidyverse)
library(plotKML)
library(ggtree)
library(RColorBrewer)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
sub_seeds<-c(27790,25837,24109,54786,19759,13651,17569,18517,4497,4847,9898,11847,40440,23724)
#species_evo_type
#1: niche conservatism
#2: niche shift (directional)
#3: niche expansion (directional)
#4: niche expansion (omnidirectional)
#5: niche shift (random in box center)
#6: niche shift (random symmetrical change in box limit)
#7: niche shift (random asymmetrical change in box limit)
species_evo_types<-data.frame(species_evo_type=c(1,2,2,2,3,3,3,4,4,4,5,6,7),
                              directional_speed=c(0,0.5,0.1,0.01,0.5,0.1,0.01,
                                                  0.5,0.1,0.01,0.01,0.01,0.01))
nb<-c("BROAD", "NARROW")
da<-c("GOOD", "POOR")

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=1

nb<-"BROAD"
da<-"GOOD"
species_evo_level<-0

if (species_evo_level==0){
  simulations<-simulations[which((simulations$nb==nb)&
                                   (simulations$is_run==1)&
                                   (simulations$species_evo_level==species_evo_level)&
                                   (simulations$da==da)),]
}else{
  simulations1<-simulations[which((simulations$nb==nb)&
                                    (simulations$is_run==1)&
                                    (simulations$species_evo_level==species_evo_level)&
                                    (simulations$da==da)),]
  
  simulations2<-simulations[which((simulations$nb==nb)&
                                    (simulations$is_run==1)&
                                    (simulations$species_evo_level==0)&
                                    (simulations$da==da)&
                                    (simulations$species_evo_type==1)),]
  
  simulations<-rbind(simulations1, simulations2)
}

all_df<-simulations

table(all_df$species_evo_type)

#23724_POOR_BROAD_2_0.01_0
template<-"%d_%s_%s_%d_%s_%d"

df_all<-list()
i=3
for (i in c(1:nrow(all_df))){
  
  print(paste(i, nrow(all_df), nb, da, species_evo_level))
  item<-all_df[i,]
  
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
              item$directional_speed, item$species_evo_level)
  #print(paste(i, nrow(all_df), sp))
  
  if (species_evo_level==0){
    if ((nb=="BROAD")&(da=="GOOD")){
      ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.DISTRIBUTION.csv", sp, sp)
    }else{
      ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.DISTRIBUTION.csv", sp, sp)  
    }
  }else{
    if (item$species_evo_type==1){
      ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.DISTRIBUTION.csv", sp, sp)  
    }else{
      ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.DISTRIBUTION.csv", sp, sp) 
    }
  }
  
  
  if (!file.exists(ttt)){
    #print("skip")
    #next()
    print(ttt)
    next()
    
  }
  if (file.size(ttt)<100){
    next()
  }
  #print(sprintf("rm %s", ttt))
  item_df<-readRDS(ttt)
  if (nrow(item_df)==0){
    next()
  }
  
  #cols<-c("year", "N_SPECIES", "N_SPECIATION", "N_EXTINCTION")
  item_df<-item_df[, -c("FROM_YEAR")]
  from_year<-item_df[, .(FROM_YEAR=max(year)), by="sp_id"]
  item_df<-merge(item_df, from_year, by="sp_id")
  item_df$species_evo_type<-item$species_evo_type
  item_df$directional_speed<-item$directional_speed
  item_df$nb<-item$nb
  item_df$da<-item$da
  item_df$global_id<-item$global_id
  
  df_all[[i]]<-item_df
}

df_all<-rbindlist(df_all)

saveRDS(df_all, sprintf("../Data/distribution_traits_items/distribution_traits_%s_%s.rda", nb, da))

if (F){
  ddd<-readRDS("../Data/distribution_traits/distribution_traits_se.rda")
}
