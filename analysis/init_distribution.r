library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)
library(reshape2)
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
v_prcp$var<-"Debiased_Maximum_Monthly_Precipitation"
v_tmax$var<-"Debiased_Maximum_Monthly_Temperature"
v_tmin$var<-"Debiased_Minimum_Monthly_Temperature"

env_df_full<-rbindlist(list(v_prcp, v_tmax, v_tmin))

base_db<-sprintf("%s/Configuration/conf.sqlite", "../")
config <- dbConnect(RSQLite::SQLite(), base_db)

simulations<-dbReadTable(config, "simulations")

dbDisconnect(config)
simulations<-data.table(simulations)

simulations<-simulations[(is_run==1)&
                                 (species_evo_level==0)]

simulations_unique<-simulations
simulations_unique<-simulations_unique[, c("global_id", "nb_v")]
simulations_unique<-unique(simulations_unique)

transform(simulations_unique, nb_v = colsplit(nb_v,  "\\|", names = c('a', 'b', 'c')))
library(splitstackshape)
simulations_unique<-cSplit(simulations_unique, "nb_v", "|")
simulations_unique<-cSplit(simulations_unique, "nb_v_1", ",")
simulations_unique<-cSplit(simulations_unique, "nb_v_3", ",")
simulations_unique$nb_v_2<-NULL
colnames(simulations_unique)<-c("global_id", "t_min", "t_max", "p_min", "p_max")
env_start<-env_df_full[year==1200]

all_suitable<-list()
for (i in c(1:nrow(simulations_unique))){
  print(i)
  item<-simulations_unique[i]
  env_item<-env_start[(between(v, item$t_min, item$t_max) & 
                        (var %in% c("Debiased_Minimum_Monthly_Temperature", "Debiased_Maximum_Monthly_Temperature")))|
                        (between(v, item$p_min, item$p_max) & 
                           (var %in% c("Debiased_Maximum_Monthly_Precipitation")))]
  N_suitable<-env_item[, .(N=.N), by=list(global_id)]
  N_suitable<-N_suitable[N==3]
  N_suitable$seed_id<-item$global_id
  all_suitable[[length(all_suitable)+1]]<-N_suitable
  
}
all_suitable_df<-rbindlist(all_suitable)
saveRDS(all_suitable_df, "../Data/diversity/diversity_all_initial_year.rda")
