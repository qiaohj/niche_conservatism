library(dplyr)
library(rgdal)
library(raster)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
df<-readRDS("../old_data/stat.rda")
mask<-readRDS("../old_data/ENV/mask_df.rda")
df_sub<-df%>%dplyr::filter(EVO_TYPE==1)
length(unique(df_sub$SEED_ID))
dim(df_sub)
seeds<-unique(df_sub$SEED_ID)
seeds_df<-mask%>%dplyr::filter((global_id %in% seeds)&(Y==0))
shape <- readOGR(dsn = "../Shape/isea3h8", layer = "isea3h8p")
shape_t<-shape
shape_t@coords<-subset(shape_t@coords, shape_t$global_id %in% seeds)
shape_t@data<-subset(shape_t@data, shape_t$global_id %in% seeds)
shape_t@data$global_id<-as.numeric(as.character(shape_t@data$global_id))
writeOGR(shape_t, dsn = "../Shape/seeds", 
         layer = "seeds", driver="ESRI Shapefile", overwrite_layer=T)




target_dir<-"../"
base_db<-"../old_data/conf.sqlite"
envdb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(envdb, "simulations")
dbDisconnect(envdb)


base_db<-"../Configuration/env_Hadley3D.sqlite"
envdb <- dbConnect(RSQLite::SQLite(), base_db)
v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
v_max_temp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
v_max_prec<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
dbDisconnect(envdb)
if (F){
  colnames(v_min_temp)[2]<-"v_min"
  colnames(v_max_temp)[2]<-"v_max"
  v_temp<-merge(v_min_temp, v_max_temp, by=c("global_id", "year"))  
  v_temp$v<-(v_temp$v_min + v_temp$v_max)/2
  quantile(v_temp$v)
  v_temp$v_range<-(v_temp$v_max - v_temp$v_min)
  range(v_temp$v_range)
  quantile(v_temp[year==1200]$v_range, c(0.05, 0.1, 0.5, 0.6, 0.95))
  mean(v_temp[year==1200]$v_range)
  
  quantile(v_max_prec$v, c(0.5, 0.95))
}
nb_range<-list("BROAD"=c(60, 10),
               "NARROW"=c(40, 5))
#-23.648161649704,36.351838350296|-23.648161649704,36.351838350296|1.96372151374817,11.9637215137482
#-13.648161649704,26.351838350296|-13.648161649704,26.351838350296|4.46372151374817,9.46372151374817
simulations<-simulations[which(simulations$evo_type==1),]
simulations<-simulations[which(simulations$nb!="MODERATE"),]
simulations$id<-1
simulations$random_index<-1
simulations$nb_v<-NA
simulations<-unique(simulations)
v_min_temp<-v_min_temp%>%dplyr::filter(year==1200)
v_max_temp<-v_max_temp%>%dplyr::filter(year==1200)
v_max_prec<-v_max_prec%>%dplyr::filter(year==1200)
for (i in c(1:nrow(simulations))){
  print(i)
  s<-simulations[i,]
  min_temp<-v_min_temp%>%dplyr::filter(global_id==s$global_id)
  max_temp<-v_max_temp%>%dplyr::filter(global_id==s$global_id)
  nb_temp<-mean(c(min_temp$v, max_temp$v))
  nb1_min<-nb_temp-nb_range[[s$nb]][1]/2
  nb1_max<-nb_temp+nb_range[[s$nb]][1]/2
  nb_prec<-(v_max_prec %>% dplyr::filter((global_id==s$global_id)))$v
  nb2_min<-nb_prec-nb_range[[s$nb]][2]/2
  nb2_max<-nb_prec+nb_range[[s$nb]][2]/2
  
  simulations[i,]$nb_v<-paste(paste(nb1_min, nb1_max, sep=","), 
                              paste(nb1_min, nb1_max, sep=","),
                              paste(nb2_min, nb2_max, sep=","),
                              sep="|")
}

#species_evo_type
#1: niche conservatism
#2: niche shift (directional)
#3: niche expansion (directional)
#4: niche expansion (omnidirectional)
#5: niche shift (random in box center)
#6: niche shift (random symmetrical change in box limit)
#7: niche shift (random asymmetrical change in box limit)
species_evo_types<-data.frame(species_evo_type=c(1,2,2,2,2,3,3,3,3,4,4,4,4,5,6,7),
                              directional_speed=c(0,1,0.5,0.1,0.01,1,0.5,0.1,0.01,
                                                  1,0.5,0.1,0.01,0.01,0.01,0.01))
#species_evo_level
#0: species as a whole responds (until speciation)
#1: geographically contiguous pieces of species responds
item1<-species_evo_types
item1$species_evo_level<-0
item2<-species_evo_types
item2$species_evo_level<-1
species_evo_types<-rbind(item1, item2)
full_simulations<-NULL
i=1
for (i in c(1:nrow(species_evo_types))){
  item<-species_evo_types[i,]
  simulations$species_evo_type<-item$species_evo_type
  simulations$directional_speed<-item$directional_speed
  simulations$species_evo_level<-item$species_evo_level
  
  if (is.null(full_simulations)){
    full_simulations<-simulations
  }else{
    full_simulations<-rbind(full_simulations, simulations)
  }
}

full_simulations$label<-paste(full_simulations$global_id, 
                              full_simulations$da, 
                              full_simulations$nb, 
                              full_simulations$species_evo_type,
                              full_simulations$directional_speed,
                              full_simulations$species_evo_level, sep="_")

base_db<-"../Configuration/conf.sqlite"
full_simulations$id<-c(1:nrow(full_simulations))
full_simulations[which(full_simulations$directional_speed==1),]$is_run<-0
mydb <- dbConnect(RSQLite::SQLite(), base_db)
dbWriteTable(mydb, "simulations", full_simulations, overwrite=T)
dbDisconnect(mydb)


#select * from simulations where global_id=23724 and nb='BROAD' and da='POOR' and species_evo_type=2 and directional_speed=1 and species_evo_level=0 


#cd ~/git/ees_3d/Debug
#is_overwrite, is_debug, is_detail
#./ees_3d /media/huijieqiao/Butterfly/Niche_Conservatism/Configuration/env_Hadley3D.sqlite /media/huijieqiao/Butterfly/Niche_Conservatism/Configuration/conf.sqlite /media/huijieqiao/Butterfly/Niche_Conservatism/Results 6551 64 1 1 1