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
da<-"POOR"
species_evo_level<-0

simulations<-simulations[which((simulations$nb==nb)&
                                 (simulations$is_run==1)&
                                 (simulations$species_evo_level==species_evo_level)&
                                 (simulations$da==da)),]
all_df<-simulations

table(all_df$species_evo_type)

#23724_POOR_BROAD_2_0.01_0
template<-"%d_%s_%s_%d_%s_%d"

df_all<-list()
i=3
for (i in c(1:nrow(all_df))){
  
  print(paste(i, nrow(all_df)))
  item<-all_df[i,]
  
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
              item$directional_speed, item$species_evo_level)
  #print(paste(i, nrow(all_df), sp))
  ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.N.speciation.extinction_new.rda", sp, sp)  
  if (!file.exists(ttt)){
    #print("skip")
    #next()
    print(ttt)
    asdf
  }
  #print(sprintf("rm %s", ttt))
  item_df<-readRDS(ttt)
  #cols<-c("year", "N_SPECIES", "N_SPECIATION", "N_EXTINCTION")
  item_df$species_evo_type<-item$species_evo_type
  item_df$directional_speed<-item$directional_speed
  item_df$nb<-item$nb
  item_df$da<-item$da
  item_df$global_id<-item$global_id
  
  df_all[[i]]<-item_df
}

df_all<-rbindlist(df_all)
df_all_null<-df_all[(species_evo_type)==1&(directional_speed==0)]
df_all_null<-df_all_null[,c("nb", "da", "global_id", "year", 
                            "N_SPECIES", "N_SPECIATION", "N_EXTINCTION")]
colnames(df_all_null)<-c("nb", "da", "global_id", "year", 
                         "N_SPECIES_null", "N_SPECIATION_null", "N_EXTINCTION_null")
df_all_with_null<-merge(df_all, df_all_null, 
                        by=c("nb", "da", "global_id", "year"),
                        all=F)
saveRDS(df_all_with_null, sprintf("../Data/N_speciation_extinction_items/N_speciation_extinction_%s_%s.rda", 
                                  nb, da))

