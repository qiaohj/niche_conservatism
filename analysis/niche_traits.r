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
  ttt<-sprintf("%s/%s/%s.niche_traits.rda", base, sp, sp)
  
  if (file.exists(ttt)){
    print("exist skip")
    next()
  }
  
  log<-sprintf("%s/%s/%s.sp.log", base, sp, sp)
  
  if (!file.exists(log)){
    print("no log, skip")
    next()
  }
  
  saveRDS(NULL, ttt)
  if (file.size(log)==0){
    next()
  }
  df<-fread(log)
  
  
  if (ncol(df)==13){
    colnames(df)<-c("year", "SP_ID",
                    "V1_L", "V1_min", "V1_max",
                    "V2_L", "V2_min", "V2_max",
                    "V3_L", "V3_min", "V3_max", 
                    "TT", "group_id")
  }else{
    colnames(df)<-c("year", "SP_ID",
                    "V1_L", "V1_min", "V1_max",
                    "V2_L", "V2_min", "V2_max",
                    "V3_L", "V3_min", "V3_max", 
                    "TT")
    df$group_id<-1
  }
  
  unique(df$V1_L)
  
  v_labels<-c("Debiased_Maximum_Monthly_Temperature",
              "Debiased_Minimum_Monthly_Temperature",
              "Debiased_Maximum_Monthly_Precipitation")
  v_label_f<-"max_t"
  df_all<-list()
  i=1
  for (v_label in v_labels){
    for (i in c(1:3)){
      item<-df[get(sprintf("V%d_L", i))==v_label]
      cols<-c("year", "SP_ID", "group_id",
              sprintf("V%d_L", i), sprintf("V%d_min", i), sprintf("V%d_max", i))
      item<-item[, ..cols]
      if (nrow(item)==0){
        next()
      }
      colnames(item)<-c("year", "SP_ID", "group_id",
                        "V_L", "V_min", "V_max")
      df_all[[length(df_all)+1]]<-item
    }
  }
  df_all<-rbindlist(df_all)
  
  df_all$V_mean<-(df_all$V_max+df_all$V_min)/2
  df_all$V_range<-df_all$V_max-df_all$V_min
  saveRDS(df_all, ttt)
}

