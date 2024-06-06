library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)
library(sf)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))

if (F){
  continents<-read_sf("../Shape/continents/splitted/polygons.shp")
  continent<-"Asia"
  lonlat<-data.frame(readRDS("../Data/mask_lonlat.rda"))
  lonlat_p <- st_as_sf(lonlat, coords = c("lon","lat"), crs=st_crs(continents), remove=F)
  lonlat_p$CONTINENT<-""
  
  for (continent in unique(continents$CONTINENT)){
    item<-continents[which(continents$CONTINENT==continent),]
    index<-st_contains(item, lonlat_p)
    index<-unlist(index)
    lonlat_p[index,]$CONTINENT<-continent
  }
  lonlat_p<-lonlat_p[which(lonlat_p$CONTINENT!=""),]
  #ggplot(lonlat_p)+geom_sf(aes(color=CONTINENT))
  write_sf(lonlat_p, "../Data/continents/mask_lonlat_continent.shp")
  lonlat_df<-data.table(lonlat_p)
  lonlat_df$geometry<-NULL
  saveRDS(lonlat_df, "../Data/continents/mask_lonlat_continent.rda")
}
lonlat<-readRDS("../Data/continents/mask_lonlat_continent.rda")
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233

#simulations<-simulations[which(((simulations$nb=="BROAD" & simulations$da=="GOOD"))),]
#simulations<-simulations[which(((simulations$nb=="NARROW"))),]
#simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
simulations<-simulations[which(simulations$species_evo_type %in% c(1)),]
outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
all_df<-simulations
all_df<-all_df[which(all_df$species_evo_level==0),]
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
all_df<-data.table(all_df)
all_df<-all_df[!global_id %in% unique(outliers$global_id)]
#19980
#all_df<-all_df[species_evo_type==1]


coms<-data.table(expand.grid(nb=c("NARROW", "BROAD"),
                             da=c("POOR", "GOOD"),
                             continent=unique(lonlat$CONTINENT)))
df_full<-list()
for (j in c(1:nrow(coms))){
  label<-sprintf("%s_%s_%s", coms[j]$nb, coms[j]$da, coms[j]$continent)
  df_full[[label]]<-list()
}
  
template<-"%d_%s_%s_%d_%s_%d"

for (i in c(1:nrow(all_df))){
  item<-all_df[i,]
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
              item$directional_speed, item$species_evo_level)
  
  print(paste(i, nrow(all_df), sp))
  if ((item$species_evo_level==1)&(item$nb=="BROAD")){
    print("1 broad skip")
    next()
  }
  if ((item$species_evo_level==1)&(item$species_evo_type==1)){
    print("1 1 skip")
    next()
  }
  
  if (item$species_evo_level==1){
    base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results_1"
  }else{
    base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
  }
  ttt<-sprintf("%s/%s/%s.density.rda", base, sp, sp)
  df<-readRDS(ttt)
  if (is.null(df)){
    next()
  }
  if (nrow(df)>0){
    
    df$is_seed<-df$global_id==item$global_id
    df$nb<-item$nb
    df$da<-item$da
    df_with_continent<-merge(df, lonlat, by="global_id")
    #setorderv(df_with_continent, "year", -1)
    coms_sub<-coms[nb==item$nb & da==item$da]
    
    for (j in c(1:nrow(coms_sub))){
      label<-sprintf("%s_%s_%s", coms_sub[j]$nb, coms_sub[j]$da, coms_sub[j]$continent)
      
      
      df_item<-df_with_continent
      item_sub<-df_item[nb==coms_sub[j]$nb & da==coms_sub[j]$da & CONTINENT==coms_sub[j]$continent]
      if (nrow(item_sub)>0){
        print(paste(coms_sub[j]$nb, coms_sub[j]$da, coms_sub[j]$continent, i, sp, nrow(item_sub)))
        df_full[[label]][[length(df_full[[label]])+1]]<-item_sub
      }
    }
  }
}

for (j in c(1:nrow(coms))){
  label<-sprintf("%s_%s_%s", coms[j]$nb, coms[j]$da, coms[j]$continent)
  print(label)
  df_df<-rbindlist(df_full[[label]])
  saveRDS(df_df, sprintf("../Data/density/%s_%s_%d_%d_%d_%s.rda", coms[j]$nb, coms[j]$da, 1, 0, 0, coms[j]$continent))
}
