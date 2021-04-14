library("DBI")
library("ape")
library("phangorn")
library("phytools")
library("geiger")
library("stringr")
library("tidyverse")
library("rgdal")
library("raster")
library("stringr")
library("dplyr")
library("RSQLite")

base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/Results_TEST"
sims<-list.dirs(base, full.names=F)
sim<-sims[7]

shape <- readOGR(dsn = "/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/isea3hGen/outputfiles", layer = "isea3h8p")


result<-data.frame()
for (sim in sims){
  if (sim==""){
    next()
  }
  
  str = strsplit(sim, "_")[[1]]
  id = str[1]
  da = str[2]
  nb = str[3]
  evo_type = str[4]
  evo_ratio = str[5]
  print(sim)
  mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/%s/%s.sqlite", base, sim, sim))  
  trees<-dbReadTable(mydb, "trees")
  suitable<-dbReadTable(mydb, "suitable")
  dbDisconnect(mydb) 
  
  shape_t<-shape
  shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% suitable$ID)
  shape_t@data<-subset(shape_t@data, shape_t$global_id %in% suitable$ID)
  shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
  shape_t@data<-inner_join(shape_t@data, suitable, by=c("global_id"="ID"))
  writeOGR(shape_t, dsn = sprintf("%s/%s", base, sim), 
           layer = "suitable", driver="ESRI Shapefile", overwrite_layer=T)
  plot(shape_t)
  text.string<-trees[1,2]
  text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
  if (!grepl("\\(", text.string)){
    text.string<-paste("()", text.string, sep="")
  }
  vert.tree<-read.newick(text=text.string)
  n_node<-length(vert.tree$node.label)
  n_leaf<-length(vert.tree$tip.label)
  if (n_leaf>0){
    plotTree(vert.tree, ftype="i")
    nodelabels(vert.tree$node.label)
  }
  map<-read.table(sprintf("%s/%s/%s.log", base, sim, sim), head=F, sep=",")
  colnames(map)<-c("YEAR", "ID", "group_id", "n", "sp_id")
  item <- map[which(map$YEAR==0),]
  shape_t<-shape
  shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% item$ID)
  shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
  shape_t@data<-subset(shape_t@data, shape_t$global_id %in% item$ID)
  #plot(shape_t, col=rainbow(20)[shape_t@data$group_id+1], cex=0.2, pch=as.numeric(shape_t@data$sp_id))
  writeOGR(shape_t, dsn = sprintf("%s/%s", base, sim), 
           layer = "0000", driver="ESRI Shapefile", overwrite_layer=T)
  
  logbase<-sprintf("%s/%s/%s", base, sim, sim)
  nb_df<-read.table(sprintf("%s.nb.log", logbase), head=F, sep=",", quote = "\'", stringsAsFactors = F)
  head(nb_df)
  tail(nb_df)
  dim(nb_df)
  
}