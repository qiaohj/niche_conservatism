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
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233


#nb<-"NARROW"
#da<-"POOR"
#simulations<-simulations[which(((simulations$nb==nb & simulations$da==da))),]
simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(simulations$species_evo_type==1),]
#simulations<-simulations[which(simulations$is_run==1),]
outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
outliers<-unique(outliers$global_id)
simulations<-simulations[which(!(simulations$global_id %in% outliers)),]
table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])


template<-"%d_%s_%s_%d_%s_%d"
t=1
ratio<-0
i=1
for (t in c(1:7)){
  #all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
  all_df2<-simulations[which(simulations$species_evo_type==t),]
  for (ratio in unique(all_df2$directional_speed)){
    target<-sprintf("../Data/diversity_items/%d_%s_without_3SD_outliers.rda", 
                    t, as.character(ratio))
    if (file.exists(target)){
      next()
    }
    saveRDS(NULL, target)
    all_df<-all_df2[which(all_df2$directional_speed==ratio),]
    all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
    all_df<-data.table(all_df)
    df_all<-list()
    for (i in c(1:nrow(all_df))){
      item<-all_df[i,]
      if (item$global_id==20109){
        next()
      }
      sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
                  item$directional_speed, item$species_evo_level)
      
      print(paste(i, nrow(all_df), sp))
      
      
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
      ttt<-sprintf("%s/%s/%s.diversity.rda", base, sp, sp)
      
      df<-readRDS(ttt)
      if (is.null(df)){
        
        next()
      }
      df<-df[year==0]
      if (nrow(df)==0){
        next()
      }
      
      print(ttt)
      df$seed_id<-item$initial_seeds
      df$da<-item$da
      df$nb<-item$nb
      df$evo_type<-item$evo_type
      df$species_evo_type<-item$species_evo_type
      df$directional_speed<-item$directional_speed 
      df$species_evo_level<-item$species_evo_level
      
      df_all[[length(df_all)+1]]<-df
    }
    
    if (T){
      
      df_all_df_list<-rbindlist(df_all)
      
      saveRDS(df_all_df_list, target)
      
      
      
    }
  }
}
if (F){
  df_all<-list()
  for (t in c(1:7)){
    #all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
    all_df2<-simulations[which(simulations$species_evo_type==t),]
    
    for (ratio in unique(all_df2$directional_speed)){
      if (t %in% c(2:4)){
        if (ratio==0.01){
          print("skip")
          next()
        }
      }
      target<-sprintf("../Data/diversity_items/%d_%s_without_3SD_outliers.rda", 
                      t, as.character(ratio))
      item<-readRDS(target)
      df_all[[length(df_all)+1]]<-item
    }
  }
  df_all_df<-rbindlist(df_all)
  
  saveRDS(df_all_df, "../Data/diversity/diversity_full_without_outlier_last_year.rda")
}





