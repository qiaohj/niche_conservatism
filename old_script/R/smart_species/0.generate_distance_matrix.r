library(rgdal)
library(raster)
library(stringr)
library(dplyr)
library(RSQLite)
library(DBI)

dist<-read.table("/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/SQLITE/neighbor_distancs.db", head=F, sep=",", stringsAsFactors = F)
colnames(dist)<-c("i", "j", "dist")
mydb <- dbConnect(RSQLite::SQLite(), "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/SQLITE/env_Hadley3D.sqlite")
dbWriteTable(mydb, "distances", dist, overwrite=T)
dbDisconnect(mydb) 

dist_item<-dist[which((dist$i==12512)|(dist$j==12512)),]
dist_item$z<-dist_item$i
dist_item[which(dist_item$z==12512), "z"]<-dist_item[which(dist_item$z==12512), "j"]
shape <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/isea3hGen/outputfiles", layer = "isea3h8p")
shape_t<-shape
shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% dist_item$z)
shape_t@data<-subset(shape_t@data, shape_t$global_id %in% dist_item$z)
shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
shape_t@data<-inner_join(shape_t@data, dist_item, by=c("global_id"="z"))
writeOGR(shape_t, dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/test", 
         layer = "neighbor_test", driver="ESRI Shapefile", overwrite_layer=T)
