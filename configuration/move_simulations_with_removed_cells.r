library(dplyr)
library(ggplot2)
library(RSQLite)
library(DBI)
library(data.table)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)
simulations<-data.table(simulations)
simulations<-simulations[is_run==1]
folder1<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
folder2<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results_with_removed_cells"
folder3<-"../Results"
cmds1<-c()
cmds2<-c()
i=1
for (i in c(1:nrow(simulations))){
  cmd<-sprintf("mv %s/%d_%s_%s_%d_%s_0 %s",
               folder1, simulations[i]$global_id, 
               simulations[i]$da,
               simulations[i]$nb, 
               simulations[i]$species_evo_type,
               as.character(simulations[i]$directional_speed),
               folder2)
  cmds1<-c(cmds1, cmd)
  cmd<-sprintf("mv %s/%d_%s_%s_%d_%s_0 %s",
               folder3, simulations[i]$global_id, 
               simulations[i]$da,
               simulations[i]$nb, 
               simulations[i]$species_evo_type,
               as.character(simulations[i]$directional_speed),
               folder1)
  cmds2<-c(cmds2, cmd)
}
write.table(cmds1, "../Data/temp/mv1.sh", row.names = F, col.names = F, quote = F)
write.table(cmds2, "../Data/temp/mv2.sh", row.names = F, col.names = F, quote = F)
