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

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)

simulations<-simulations[which(((simulations$nb=="NARROW"))),]
simulations<-simulations[which(simulations$species_evo_level==1),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(4)),]

table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])

template<-"%d_%s_%s_%d_%s_%d"
mask<-readRDS("../Data/mask_lonlat.rda")
#all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]

cmds<-c()
for (i in c(1:nrow(all_df))){
  item<-all_df[i,]
  
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, item$directional_speed, item$species_evo_level)
  print(paste(i, nrow(all_df), sp))
  N<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.N.csv", sp, sp)
  distribution<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results_1/%s/%s.DISTRIBUTION.csv", sp, sp)
  t1<-file.mtime(distribution)
  t2<-file.mtime(N)
  if (t2>t1){
    cmd<-sprintf("rm %s", N)
    cmds<-c(cmds, cmd)
  }
  
}
write.csv(cmds, "../Data/temp/rm_distribution.sh", row.names = F, col.names = F, quote=F)
