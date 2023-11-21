library(data.table)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=1

nb<-"BROAD"
da<-"POOR"
species_evo_level<-0
simulations<-simulations[which((simulations$nb==nb)&
                                 (simulations$is_run==1)&
                                 (simulations$species_evo_level==species_evo_level)&
                                 (simulations$da==da)),]
simulations<-simulations[which(simulations$species_evo_type!=1),]
simulations<-data.table(simulations)
simulations<-simulations[((directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
    (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
all_df<-simulations

table(all_df$species_evo_type)

#23724_POOR_BROAD_2_0.01_0
template<-"%d_%s_%s_%d_%s_%d"

df_all<-list()
i=9000
for (i in c(1:nrow(all_df))){
  
  print(paste(i, nrow(all_df), nb, da, species_evo_level))
  item<-all_df[i]
  
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, 
              item$directional_speed, item$species_evo_level)
  #print(paste(i, nrow(all_df), sp))
  
  ttt<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.delta.log", sp, sp)
  
  if (!file.exists(ttt)){
    print("skip")
    
    print(ttt)
    next()
    #asdf
    
  }
  if (file.size(ttt)<100){
    print("skip")
    
    print(ttt)
    next()
    #asdf
    
  }
  
  #print(sprintf("rm %s", ttt))
  item_df<-fread(ttt)
  colnames(item_df)<-c("year", "sp_id", "delta_lower_limit", "delta_upper_limit", "var")
  nvs<-strsplit(item$nb_v, "\\|")[[1]]
  sss<-as.numeric(unlist(strsplit(nvs, ",")))
  min<-sss[c(1, 3, 5)]
  max<-sss[c(2, 4, 6)]
  vars<-strsplit(item$environments, ",")[[1]]
  fn<-data.table(min=min, max=max, var=vars)
  item_df$global_id<-item$global_id
  item_df$nb<-item$nb
  item_df$da<-item$da
  item_df$species_evo_type<-item$species_evo_type
  item_df$directional_speed<-item$directional_speed
  item_df<-merge(item_df, fn, by="var")
  df_all[[length(df_all)+1]]<-item_df
}
df_all_df<-rbindlist(df_all)
saveRDS(df_all_df, sprintf("../Data/niche_traits/niche_delta_%s_%s.rda", nb, da))

if (F){
  item1<-readRDS("../Data/niche_traits/niche_delta_NARROW_GOOD.rda")
  item2<-readRDS("../Data/niche_traits/niche_delta_NARROW_POOR.rda")
  item3<-readRDS("../Data/niche_traits/niche_delta_BROAD_GOOD.rda")
  item4<-readRDS("../Data/niche_traits/niche_delta_BROAD_POOR.rda")
  
  df_full<-rbindlist(list(item1, item2, item3, item4))
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  df_full_outliers<-df_full[!(global_id %in% unique(outliers$global_id))]
  colnames(df_full_outliers)[4]<-"delta_c"
  colnames(df_full_outliers)[5]<-"delta_box"
  df_full_outliers$delta_lower_limit<-0
  df_full_outliers$delta_upper_limit<-0
  df_full_outliers[species_evo_type==2]$delta_lower_limit<-
    df_full_outliers[species_evo_type==2]$delta_c * df_full_outliers[species_evo_type==2]$directional_speed
  df_full_outliers[species_evo_type==2]$delta_upper_limit<-
    df_full_outliers[species_evo_type==2]$delta_c * df_full_outliers[species_evo_type==2]$directional_speed
  
  df_full_outliers[species_evo_type==3 & delta_c<0]$delta_lower_limit<-
    df_full_outliers[species_evo_type==3 & delta_c<0]$delta_c * 
    df_full_outliers[species_evo_type==3 & delta_c<0]$directional_speed
  df_full_outliers[species_evo_type==3 & delta_c>0]$delta_upper_limit<-
    df_full_outliers[species_evo_type==3 & delta_c>0]$delta_c * 
    df_full_outliers[species_evo_type==3 & delta_c>0]$directional_speed
  
  df_full_outliers[species_evo_type==4]$delta_lower_limit<-
    abs(df_full_outliers[species_evo_type==4]$delta_c) * -1 *
    df_full_outliers[species_evo_type==4]$directional_speed
  df_full_outliers[species_evo_type==4]$delta_upper_limit<-
    abs(df_full_outliers[species_evo_type==4]$delta_c) * 
    df_full_outliers[species_evo_type==4]$directional_speed
  
  df_full_outliers[species_evo_type==5]$delta_lower_limit<-
    (df_full_outliers[species_evo_type==5]$max - df_full_outliers[species_evo_type==5]$min) *
    df_full_outliers[species_evo_type==5]$delta_c * 
    df_full_outliers[species_evo_type==5]$directional_speed
  df_full_outliers[species_evo_type==5]$delta_upper_limit<-
    (df_full_outliers[species_evo_type==5]$max - df_full_outliers[species_evo_type==5]$min) *
    df_full_outliers[species_evo_type==5]$delta_c * 
    df_full_outliers[species_evo_type==5]$directional_speed
  
  df_full_outliers[species_evo_type==6]$delta_lower_limit<-
    (df_full_outliers[species_evo_type==6]$max - df_full_outliers[species_evo_type==6]$min) *
    df_full_outliers[species_evo_type==6]$delta_box * 
    df_full_outliers[species_evo_type==6]$directional_speed
  df_full_outliers[species_evo_type==6]$delta_upper_limit<-
    (df_full_outliers[species_evo_type==6]$max - df_full_outliers[species_evo_type==6]$min) *
    df_full_outliers[species_evo_type==6]$delta_box * -1 *
    df_full_outliers[species_evo_type==6]$directional_speed
  
  df_full_outliers[species_evo_type==7]$delta_lower_limit<-
    (df_full_outliers[species_evo_type==7]$max - df_full_outliers[species_evo_type==7]$min) *
    (df_full_outliers[species_evo_type==7]$delta_c + df_full_outliers[species_evo_type==7]$delta_box) * 
    df_full_outliers[species_evo_type==7]$directional_speed
  df_full_outliers[species_evo_type==7]$delta_upper_limit<-
    (df_full_outliers[species_evo_type==7]$max - df_full_outliers[species_evo_type==7]$min) *
    (df_full_outliers[species_evo_type==7]$delta_c + df_full_outliers[species_evo_type==7]$delta_box * -1) * 
    df_full_outliers[species_evo_type==7]$directional_speed
  
  df_full_outliers$delta_centra<-(df_full_outliers$delta_upper_limit+df_full_outliers$delta_lower_limit)/2
  saveRDS(df_full_outliers, "../Data/niche_traits/niche_delta_raw.rda")
  
  df_full_outliers_se_nb_da<-df_full_outliers[, .(delta_lower_limit=mean(delta_lower_limit), 
                                            delta_lower_limit_sd=sd(delta_lower_limit),
                                            delta_upper_limit=mean(delta_upper_limit), 
                                            delta_upper_limit_sd=sd(delta_upper_limit),
                                            delta_centra=mean(delta_centra), 
                                            delta_centra_sd=sd(delta_centra)),
                                        by=list(var, year, species_evo_type, directional_speed, nb, da)]
  
  df_full_outliers_se_nb<-df_full_outliers[, .(delta_lower_limit=mean(delta_lower_limit), 
                                                  delta_lower_limit_sd=sd(delta_lower_limit),
                                                  delta_upper_limit=mean(delta_upper_limit), 
                                                  delta_upper_limit_sd=sd(delta_upper_limit),
                                                  delta_centra=mean(delta_centra), 
                                                  delta_centra_sd=sd(delta_centra)),
                                              by=list(var, year, species_evo_type, directional_speed, nb)]
  
  df_full_outliers_se<-df_full_outliers[, .(delta_lower_limit=mean(delta_lower_limit), 
                                                  delta_lower_limit_sd=sd(delta_lower_limit),
                                                  delta_upper_limit=mean(delta_upper_limit), 
                                                  delta_upper_limit_sd=sd(delta_upper_limit),
                                                  delta_centra=mean(delta_centra), 
                                                  delta_centra_sd=sd(delta_centra)),
                                              by=list(var, year, species_evo_type, directional_speed)]
  saveRDS(df_full_outliers_se_nb_da, "../Data/niche_traits/niche_delta_nb_da.rda")
  saveRDS(df_full_outliers_se_nb, "../Data/niche_traits/niche_delta_nb.rda")
  saveRDS(df_full_outliers_se, "../Data/niche_traits/niche_delta.rda")
  
  df_full_outliers$year_100<-floor(df_full_outliers$year/100) * 100
  
  df_full_outliers_se_nb_da<-df_full_outliers[, .(delta_lower_limit=mean(delta_lower_limit), 
                                                  delta_lower_limit_sd=sd(delta_lower_limit),
                                                  delta_upper_limit=mean(delta_upper_limit), 
                                                  delta_upper_limit_sd=sd(delta_upper_limit),
                                                  delta_centra=mean(delta_centra), 
                                                  delta_centra_sd=sd(delta_centra)),
                                              by=list(var, year_100, species_evo_type, directional_speed, nb, da)]
  
  df_full_outliers_se_nb<-df_full_outliers[, .(delta_lower_limit=mean(delta_lower_limit), 
                                               delta_lower_limit_sd=sd(delta_lower_limit),
                                               delta_upper_limit=mean(delta_upper_limit), 
                                               delta_upper_limit_sd=sd(delta_upper_limit),
                                               delta_centra=mean(delta_centra), 
                                               delta_centra_sd=sd(delta_centra)),
                                           by=list(var, year_100, species_evo_type, directional_speed, nb)]
  
  df_full_outliers_se<-df_full_outliers[, .(delta_lower_limit=mean(delta_lower_limit), 
                                            delta_lower_limit_sd=sd(delta_lower_limit),
                                            delta_upper_limit=mean(delta_upper_limit), 
                                            delta_upper_limit_sd=sd(delta_upper_limit),
                                            delta_centra=mean(delta_centra), 
                                            delta_centra_sd=sd(delta_centra)),
                                        by=list(var, year_100, species_evo_type, directional_speed)]
  saveRDS(df_full_outliers_se_nb_da, "../Data/niche_traits/niche_delta_nb_da_year_100.rda")
  saveRDS(df_full_outliers_se_nb, "../Data/niche_traits/niche_delta_nb_year_100.rda")
  saveRDS(df_full_outliers_se, "../Data/niche_traits/niche_delta_year_100.rda")
  
  
  
}