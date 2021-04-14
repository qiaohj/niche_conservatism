setwd("~/git/ees_3d/R/smart_species")

library(rgdal)
library(raster)
library(stringr)
library(dplyr)
library(RSQLite)
library(DBI)

mydb <- dbConnect(RSQLite::SQLite(), "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
if (F){
  
  simulations[which(simulations$nb=="Narrow"), "label"]<-gsub("Narrow", "NARROW", 
                                                              simulations[which(simulations$nb=="Narrow"), "label"])
  simulations[which(simulations$nb=="Narrow"), "nb"]<-"NARROW"
  head( simulations[which(simulations$nb=="NARROW"),])
  
  labels<-str_split_fixed(simulations$label, "_", 5)
  simulations$evo_type<-as.numeric(labels[, 4])
  dbWriteTable(mydb, "simulations", simulations, overwrite=T)
}
dbDisconnect(mydb) 

simulations<-simulations[which(simulations$niche_breadth_evolution_ratio=="0,1,1,1"),]
simulations<-data.frame(lapply(simulations, function(x) { gsub("_2_", "_5_", x)}))
simulations$niche_breadth_evolution_ratio<-"0.5,0.75,1,1"
mydb <- dbConnect(RSQLite::SQLite(), "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/conf_combine.sqlite")
timeLine<-data.frame(from=1200, to=0, step=-1)
dbWriteTable(mydb, "timeline", timeLine, overwrite=T)
dbWriteTable(mydb, "simulations", simulations, overwrite=T)
dbDisconnect(mydb)


