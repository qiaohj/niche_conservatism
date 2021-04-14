library(rgdal)
library(raster)
library(stringr)
library(dplyr)
library(RSQLite)
library(DBI)
library("ape")
library("phangorn")
library("phytools")
library("geiger")
library("stringr")
library(tidyverse)
library(plotKML)
#data(eberg_contours)
## Not run: 
#plotKML(eberg_contours)




logdb<-"/media/huijieqiao/Butterfly/SMART_SPECIES/RESULTS/17956_GOOD_NARROW_2_0.01/17956_GOOD_NARROW_2_0.01.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), logdb)
trees<-dbReadTable(mydb, "trees")
suitable<-dbReadTable(mydb, "suitable")
dbDisconnect(mydb)
text.string<-trees[1,2]
text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
vert.tree<-read.tree(text=text.string)

plotTree(vert.tree, ftype="i")
#tiplabels(vert.tree$tip.label)
nodelabels(vert.tree$node.label)

map<-read.table(gsub("sqlite", "log", logdb), head=F, sep=",")
colnames(map)<-c("YEAR", "ID", "group_id", "n", "sp_id", "issuitable")

shape <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/isea3hGen/outputfiles", layer = "isea3h8p")
shape_t<-shape
shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% suitable$ID)
shape_t@data<-subset(shape_t@data, shape_t$global_id %in% suitable$ID)
shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
shape_t@data<-inner_join(shape_t@data, suitable, by=c("global_id"="ID"))
writeOGR(shape_t, dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/test", 
         layer = "suitable", driver="ESRI Shapefile", overwrite_layer=T)
table(map$YEAR)
y<-0
for (y in c(1100:0)){
  
  item <- map[which(map$YEAR==y),]
  shape_t<-shape
  shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% item$ID)
  shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
  shape_t@data<-subset(shape_t@data, shape_t$global_id %in% item$ID)
  shape_t@data<-inner_join(shape_t@data, item, by=c("global_id"="ID"))
  writeOGR(shape_t, dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/test", 
           layer = sprintf("%s_3", str_pad(y, 4, pad="0")), driver="ESRI Shapefile", overwrite_layer=T)
  plot(shape_t, col=rainbow(2)[shape_t@data$issuitable], cex=0.2)
  print(table(item$group_id))
  x<-readline(prompt=sprintf("Year is %d, Found %d groups. (X=exit): ", y, length(unique(item$group_id))))
  if (tolower(x)=="x"){
    break()
  }
}
length(unique(map$sp_id))


unique(map[which(map$sp_id=='20667-1-1-1'), "YEAR"])


#Test neighbor
if (F){
  df_neighbor<-read.table("/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/SQLITE/neighbor_distancs (copy).db", head=F, sep=",")
  shape <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/isea3hGen/outputfiles", layer = "isea3h8p")
  shape_t<-shape
  shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% df_neighbor$V2)
  shape_t@data<-subset(shape_t@data, shape_t$global_id %in% df_neighbor$V2)
  shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
  shape_t@data<-inner_join(shape_t@data, df_neighbor, by=c("global_id"="V2"))
  writeOGR(shape_t, dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/test", 
           layer = "neighbor_error", driver="ESRI Shapefile", overwrite_layer=T)
}
