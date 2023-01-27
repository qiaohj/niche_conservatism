library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(10)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233


nb<-"NARROW"
da<-"POOR"
simulations<-simulations[which(((simulations$nb==nb & simulations$da==da))),]
simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
outlier_type<-"IQR"
outlier_ids<-readRDS(sprintf("../Data/outliers/outliers_%s.rda", outlier_type))
simulations<-simulations[which(!(simulations$global_id %in% outlier_ids)),]
table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])


template<-"%d_%s_%s_%d_%s_%d"
#all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
all_df<-data.table(all_df)
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
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }else{
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }
  }
  ttt<-sprintf("%s/%s/%s.diversity.rda", base, sp, sp)
  df<-readRDS(ttt)
  
  if (is.null(df)){
    next()
  }
  df$seed_id<-item$initial_seeds
  df$da<-item$da
  df$nb<-item$nb
  df$evo_type<-item$evo_type
  df$species_evo_type<-item$species_evo_type
  df$directional_speed<-item$directional_speed 
  df$species_evo_level<-item$species_evo_level
  
  df_all[[length(df_all)+1]]<-df
}
diversity_se_all<-list()
for (i in seq(1, length(df_all), by=1300)){
  print(i)
  last<-i+1199
  if (last>length(df_all)){
    last<-length(df_all)
  }
  df_all_df<-rbindlist(df_all[c(i:last)])
  diversity_se<-df_all_df[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                          by=list(year, global_id, da, nb, evo_type, 
                                  species_evo_type, directional_speed,
                                  species_evo_level)]
  diversity_se$i<-i
  diversity_se_all[[length(diversity_se_all)+1]]<-diversity_se
}

diversity_se_all_df<-rbindlist(diversity_se_all[c(1:8)])


diversity_se_all_df_se<-diversity_se_all_df[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                            by=list(year, global_id, da, nb, evo_type, 
                                                    species_evo_type, directional_speed,
                                                    species_evo_level)]
if (length(diversity_se_all)>8){
  diversity_se_all_df2<-rbindlist(diversity_se_all[c(9:length(diversity_se_all))])
  diversity_se_all_df_se2<-diversity_se_all_df2[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                                by=list(year, global_id, da, nb, evo_type, 
                                                        species_evo_type, directional_speed,
                                                        species_evo_level)]
  
  diversity_se_all_df_se_xx<-rbindlist(list(diversity_se_all_df_se, diversity_se_all_df_se2))
  
  diversity_se_all_df_se_xx2<-diversity_se_all_df_se_xx[, .(N_SPECIES=sum(N_SPECIES), N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                                        by=list(year, global_id, da, nb, evo_type, 
                                                                species_evo_type, directional_speed,
                                                                species_evo_level)]
}else{
  diversity_se_all_df_se_xx2<-diversity_se_all_df_se
}

#saveRDS(diversity_se_all_df_se, 
#        sprintf("../Data/diversity_items/%s_%s_without_outliers.rda", nb, da))
#da<-"GOOD"
saveRDS(diversity_se_all_df_se_xx2, 
        sprintf("../Data/diversity_items/%s_%s_without_%s_outliers.rda", nb, da, outlier_type))

#saveRDS(diversity_se_all_df_se2, 
#        sprintf("../Data/diversity_items_%s_%s2_without_outliers.rda", nb, da))


#diversity_se_all_df_se1<-readRDS(sprintf("../Data/diversity_items_%s_%s.rda", nb, da))

#diversity_se_all_df_se2<-readRDS(sprintf("../Data/diversity_items_%s_%s2.rda", nb, da))

