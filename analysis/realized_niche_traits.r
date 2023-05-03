library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))

base_db<-"../Configuration/env_Hadley3D.sqlite"
envdb <- dbConnect(RSQLite::SQLite(), base_db)
v_min_temp<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
v_max_temp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
v_max_prec<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
dbDisconnect(envdb)
v_min_temp<-data.table(v_min_temp)
v_max_temp<-data.table(v_max_temp)
v_max_prec<-data.table(v_max_prec)

base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233

#simulations<-simulations[which(((simulations$nb=="BROAD")&(simulations$da=="GOOD"))),]
simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(!((simulations$nb=="BROAD")&(simulations$da=="GOOD"))),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(4)),]

table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])


template<-"%d_%s_%s_%d_%s_%d"
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1361&species_evo_type==2&directional_speed==0.1]
#item<-all_df[nb=="BROAD"&da=="GOOD"&global_id==1567&species_evo_type==2&directional_speed==0.1]
i=1

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
  
  if ((item$species_evo_level==1)){
    print("1 skip")
    next()
  }
  
  
  if (item$species_evo_level==1){
    base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results_1"
  }else{
    base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
  }
  ttt<-sprintf("%s/%s/%s.realized_niche_traits.rda", base, sp, sp)
  
  if (file.exists(ttt)){
    print("exist skip")
    next()
  }
  
  log<-sprintf("%s/%s/%s.log", base, sp, sp)
  
  if (!file.exists(log)){
    print("no log, skip")
    next()
  }
  saveRDS(NULL, ttt)
  if (file.size(log)==0){
    next()
  }
  df<-fread(log)
  colnames(df)<-c("year", "global_id", "group_id", "n_individual", "sp_id", "suitable")
  df<-df[suitable==1]
  if (nrow(df)==0){
    next()
  }
  df_tmin<-merge(df, v_min_temp, by=c("year", "global_id"))
  df_tmin_se<-df_tmin[, .(N_CELLS=.N, N_Individual=sum(n_individual),
                          min_v=min(v)),
                      by=c("sp_id", "year")]
  
  df_tmax<-merge(df, v_max_temp, by=c("year", "global_id"))
  df_tmax_se<-df_tmax[, .(max_v=max(v)),
                      by=c("sp_id", "year")]
  df_tmax_se$var<-"TEMP"
  
  df_temp_se<-merge(df_tmax_se, df_tmin_se, by=c("sp_id", "year"))
  df_prec<-merge(df, v_max_prec, by=c("year", "global_id"))
  df_prec_se<-df_prec[, .(N_CELLS=.N, N_Individual=sum(n_individual),
                          min_v=min(v), max_v=max(v)),
                      by=c("sp_id", "year")]
  df_prec_se$var<-"PREC"
  
  df_all<-rbindlist(list(df_temp_se, df_prec_se), use.names=T)
  
  saveRDS(df_all, ttt)
}

