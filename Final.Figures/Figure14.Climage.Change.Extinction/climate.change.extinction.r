library(data.table)
library(ggplot2)
library(ggpubr)
library(rempsyc)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)

library(ggtree)
library(phylobase)
library(ggpubr)
library(heatmaply)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  nb<-readRDS("../Data/niche_traits/niche_traits_fn_without_outlier.rda")
  base_db<-sprintf("%s/Configuration/env_Hadley3D.sqlite", "../")
  envdb <- dbConnect(RSQLite::SQLite(), base_db)
  
  v_prcp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
  v_tmax<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
  v_tmin<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
  dbDisconnect(envdb)
  v_prcp<-data.table(v_prcp)
  v_tmax<-data.table(v_tmax)
  v_tmin<-data.table(v_tmin)
  v_tmax<-v_tmax[year==0]
  v_tmin<-v_tmin[year==0]
  v_prcp<-v_prcp[year==0]
  
  v_prcp$var<-"Debiased_Maximum_Monthly_Precipitation"
  v_tmax$var<-"Debiased_Maximum_Monthly_Temperature"
  v_tmin$var<-"Debiased_Minimum_Monthly_Temperature"
  
  env_df_full<-merge(v_tmax, v_tmin, by=c("global_id", "year"))
  colnames(env_df_full)<-c("global_id", "year", "v_tmax", "tmax", "v_tmin", "tmin")
  saveRDS(env_df_full, "../Data/niche_traits/temperature_last_year.rda")
}
env_df_full<-readRDS("../Data/niche_traits/temperature_last_year.rda")

nb<-nb[var!="Debiased_Maximum_Monthly_Precipitation"]
for (i in c(-100:100)){
  env_item<-env_df_full
  env_item$v_tmax<-env_item$v_tmax + i
  env_item$v_tmin<-env_item$v_tmin + i
  nb[sp_id %in% c("57715-2-1-1-1", "57715-2-1-1-2")]
  nb_item<-nb[between(env_item$v_tmax, )]
  
}