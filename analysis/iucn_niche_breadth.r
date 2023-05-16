library(data.table)
library(sf)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

base_db<-sprintf("%s/Configuration/env_Hadley3D.sqlite", "../")
envdb <- dbConnect(RSQLite::SQLite(), base_db)

v_prcp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
v_tmax<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
v_tmin<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
dbDisconnect(envdb)
v_prcp<-data.table(v_prcp)
v_tmax<-data.table(v_tmax)
v_tmin<-data.table(v_tmin)
v_prcp_last<-v_prcp[year==0]
v_tmax_last<-v_tmax[year==0]
v_tmin_last<-v_tmin[year==0]

sf_use_s2(FALSE)
polygon<-st_read("../Shape/isea3h8/continent.shp")
vessel <- sf::st_read(dsn = "../Shape/IUCN/BIRDS/BOTW.gdb", layer = "All_Species")
polygon<-st_transform(polygon, crs=st_crs(vessel))
i=1
species<-unique(vessel$SCINAME)
nb_full<-list()
for (i in c(1:length(species))){
  print(paste(i, length(species)))
  sp<-species[i]
  item<-vessel[which(vessel$SCINAME ==sp),]
  item<-item[which(st_geometry_type(item)=="MULTIPOLYGON"),]
  if (nrow(item)==0){
    next()
  }
  index<-st_intersects(item, polygon)
  index<-unique(unlist(index))
  global_ids<-polygon[index,]$global_id
  if (length(global_ids)>0){
    range_tmin<-range(v_tmin_last[global_id %in% global_ids]$v)
    range_tmax<-range(v_tmax_last[global_id %in% global_ids]$v)
    range_prcp<-range(v_prcp_last[global_id %in% global_ids]$v)
    item_df<-data.table(species=sp, 
                        tmin_min=range_tmin[1], tmin_max=range_tmin[2],
                        tmax_min=range_tmax[1], tmax_max=range_tmax[2],
                        prcp_min=range_prcp[1], prcp_max=range_prcp[2]
    )
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)
saveRDS(nb_full_df, "../Data/IUCN_NB/Birds.rda")



polygon<-st_read("../Shape/isea3h8/continent.shp")

mammals<-st_read("../Shape/IUCN/MAMMALS_TERRESTRIAL_ONLY_20220228/MAMMALS_TERRESTRIAL_ONLY.shp")
polygon<-st_transform(polygon, crs=st_crs(mammals))
i=1
species<-unique(mammals$binomial)
nb_full<-list()
for (i in c(1:length(species))){
  print(paste(i, length(species)))
  sp<-species[i]
  item<-mammals[which(mammals$binomial ==sp),]
  index<-st_intersects(item, polygon)
  index<-unique(unlist(index))
  global_ids<-polygon[index,]$global_id
  if (length(global_ids)>0){
    range_tmin<-range(v_tmin_last[global_id %in% global_ids]$v)
    range_tmax<-range(v_tmax_last[global_id %in% global_ids]$v)
    range_prcp<-range(v_prcp_last[global_id %in% global_ids]$v)
    item_df<-data.table(species=sp, 
                        tmin_min=range_tmin[1], tmin_max=range_tmin[2],
                        tmax_min=range_tmax[1], tmax_max=range_tmax[2],
                        prcp_min=range_prcp[1], prcp_max=range_prcp[2]
                        )
    nb_full[[length(nb_full)+1]]<-item_df
  }
}
nb_full_df<-rbindlist(nb_full)
saveRDS(nb_full_df, "../Data/IUCN_NB/Mammals.rda")