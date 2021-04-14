library(rgdal)
library(raster)
library(stringr)
library(dplyr)
library(RSQLite)
library(DBI)
envdb<-"/home/huijieqiao/git/ees_3d_data/ISEA3H8/SQLITE/env_Hadley3D.sqlite"
confdb<-"/home/huijieqiao/git/ees_3d_data/TEST/conf.sqlite"
result<-"/home/huijieqiao/git/ees_3d_data/TEST/Results"
cmd_folder<-"/home/huijieqiao/git/ees_3d_data/TEST/Command"

mydb <- dbConnect(RSQLite::SQLite(), confdb)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)
simulations<-simulations[which(simulations$is_run==1),]
simulations<-simulations[which(simulations$nb!="NARROW"),]
cmd_templete<-"./ees_3d %s %s %s %d 64 0 0"
rm_template<-"rm -rf %s/%s"
cmd<-c()
rm<-c()
i=1
for (i in c(1:nrow(simulations))){
  simulation <-simulations[i,]
  check_file<-sprintf("%s/%s/%s.log", result, simulation$label, simulation$label)
  if (!file.exists(check_file)){
    cmd<-c(cmd, sprintf(cmd_templete, envdb, confdb, result, simulation$id))
    rm<-c(rm, sprintf(rm_template, result, simulation$label))
  }
}
write.table(cmd, sprintf("%s/all.sh", cmd_folder), row.names = F, col.names = F, quote=F)
write.table(rm, sprintf("%s/rm.sh", cmd_folder), row.names = F, col.names = F, quote=F)
