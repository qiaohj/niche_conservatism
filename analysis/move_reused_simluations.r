library(data.table)
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggpubr)
library(RSQLite)
library(DBI)


setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(10)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233


simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]

table(simulations[, c("nb", "da")])

removed_ids<-c(93, 13335, 20110, 20193, 20192, 19889, 19888, 19887, 19886, 19967, 19968, 19969, 19970)
template<-"%d_%s_%s_%d_%s_%d"
#all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
all_df<-simulations
all_df<-data.table(all_df)
cmds<-c()
for (i in c(1:nrow(all_df))){
  item<-all_df[i,]
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, item$directional_speed, item$species_evo_level)
  
  print(paste(i, nrow(all_df), sp))
  
  if (item$species_evo_level==1){
    base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results_1"
  }else{
    if ((item$nb=="BROAD")&(item$da=="GOOD")){
      base_old<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
      base_new<-"../Results"
    }else{
      base_new<-"../Results"
      base_old<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }
  }
  ttt_new<-sprintf("%s/%s/%s.log", base_new, sp, sp)
  if (file.exists(ttt_new)){
    next()
  }
  ttt_old<-sprintf("%s/%s/%s.diversity.rda", base_old, sp, sp)
  df<-readRDS(ttt_old)
  
  
  
  if (nrow(df[global_id %in% removed_ids])==0){
    cmd<-sprintf("cp -R %s %s", sprintf("%s/%s", base_old, sp), sprintf("%s/%s", base_new, sp))
    cmds<-c(cmds, cmd)
  }
  
}

write.table(cmds, "move_reused_simulations.sh", quote = F, row.names = F, col.names = F)
