library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233

#simulations<-simulations[which(((simulations$nb=="BROAD")&(simulations$da=="GOOD"))),]
#simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(4)),]

table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])


template<-"%d_%s_%s_%d_%s_%d"
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1361&species_evo_type==2&directional_speed==0.1]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1567&species_evo_type==2&directional_speed==0.1]
i=1
df_all<-list()
for (i in c(1:nrow(all_df))){
  item<-all_df[i,]
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, item$directional_speed, item$species_evo_level)
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
    if ((item$nb=="BROAD")&(item$da=="GOOD")){
      
      base<-"../Results"
    }else{
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }
  }
  ttt<-sprintf("%s/%s/%s.sp.rda", base, sp, sp)
  
  if (!file.exists(ttt)){
    print("not exist, skip")
    next()
  }
  dddd<-readRDS(ttt)
  if (is.null(dddd)){
    print("not data, skip")
    next()
  }
  dddd$global_id<-item$global_id
  dddd$da<-item$da
  dddd$nb<-item$nb
  dddd$nb_v<-item$nb_v
  dddd$environments<-item$environments
  dddd$evo_type<-item$evo_type
  dddd$species_evo_type<-item$species_evo_type
  dddd$directional_speed<-item$directional_speed
  dddd$species_evo_level<-item$species_evo_level
  
  
  df_all[[length(df_all)+1]]<-dddd
  
}
df_all<-rbindlist(df_all)
saveRDS(df_all, "../Data/niche_traits.rda")

niche_traits_se<-df_all[, .(V_range=mean(V_range), sd_V_range=sd(V_range)),
                        by=list(year, evo_type, nb, da, V_L,
                                species_evo_type, directional_speed, species_evo_level)]
saveRDS(niche_traits_se, "../Data/niche_traits_se.rda")