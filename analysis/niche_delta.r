library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
df_all1<-readRDS("../Data/niche_traits/niche_traits_fn.rda")
head(df_all1)
outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
df_all1_without_outliers<-df_all1[!(global_id %in% unique(outliers$global_id))]

tail(df_all1)


nb_range_year<-df_all1_without_outliers[, .(V_range=mean(V_range), 
                                            V_range_median=quantile(V_range, 0.5), 
                                            sd_V_range=sd(V_range)),
                       by=list(year, da, nb, species_evo_type, directional_speed, V_L)]

splist<-df_all1_without_outliers[, .(N=.N), by=list(global_id, nb, nb_v, environments)]
nb_str<-strsplit(splist$nb_v, "\\|")
nb_df <-  data.table(temp=sapply(nb_str,"[[",1), prcp=sapply(nb_str,"[[",3))
temp_str<-strsplit(nb_df$temp, ",")
prcp_str<-strsplit(nb_df$prcp, ",")
nb_df<-data.table(temp_min=as.numeric(sapply(temp_str,"[[",1)), 
                  temp_max=as.numeric(sapply(temp_str,"[[",2)),
                  prcp_min=as.numeric(sapply(prcp_str,"[[",1)), 
                  prcp_max=as.numeric(sapply(prcp_str,"[[",2)),
                  global_id=splist$global_id,
                  nb=splist$nb)
nb_df$temp_range<-nb_df$temp_max - nb_df$temp_min
nb_df$prcp_range<-nb_df$prcp_max - nb_df$prcp_min

df_all1_without_outliers_with_fn<-merge(df_all1_without_outliers, nb_df,
                                        by=c("global_id", "nb"))
df_all1_without_outliers_with_fn$V_min_delta<-0
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_min_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_min - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$temp_min
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_min_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_min - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$prcp_min

df_all1_without_outliers_with_fn$V_max_delta<-0
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_max_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_max - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$temp_max
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_max_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_max - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$prcp_max

df_all1_without_outliers_with_fn$temp_mean<-(df_all1_without_outliers_with_fn$temp_max+df_all1_without_outliers_with_fn$temp_min)/2
df_all1_without_outliers_with_fn$prcp_mean<-(df_all1_without_outliers_with_fn$prcp_max+df_all1_without_outliers_with_fn$prcp_min)/2
df_all1_without_outliers_with_fn$V_mean_delta<-0
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_mean_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_mean - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$temp_mean
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_mean_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_mean - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$prcp_mean


df_all1_without_outliers_with_fn$nb_delta<-0
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$nb_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_range - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$temp_range
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$nb_delta<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_range - 
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$prcp_range

df_all1_without_outliers_with_fn$nb_delta_ratio<-0
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$nb_delta_ratio<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$V_range /
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Minimum_Monthly_Temperature","Debiased_Maximum_Monthly_Temperature")]$temp_range
df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$nb_delta_ratio<-
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$V_range /
  df_all1_without_outliers_with_fn[V_L %in% c("Debiased_Maximum_Monthly_Precipitation")]$prcp_range

saveRDS(df_all1_without_outliers_with_fn, "../Data/niche_traits/niche_traits_fn_without_outlier.rda")



nb_range_last_year<-df_all1_without_outliers[year==1199]
dim(nb_range_last_year)
dim()
length(unique(nb_range_last_year$SP_ID))

saveRDS(nb_range_last_year, "../Data/niche_traits/niche_traits_last_year_fn.rda")
saveRDS(nb_range_year, "../Data/niche_traits/nb_range_year_fn.rda")
table(nb_range_last_year$species_evo_type)


df_all_rn<-readRDS("../Data/niche_traits/niche_traits_rn.rda")
df_all_rn$range_v<-df_all_rn$max_v - df_all_rn$min_v
outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
df_all_rn_without_outliers<-df_all_rn[!(global_id %in% unique(outliers$global_id))]

tail(df_all_rn)

nb_range_year_rn<-df_all_rn_without_outliers[, .(V_range=mean(range_v), 
                                             V_range_median=quantile(range_v, 0.5), 
                                             sd_V_range=sd(range_v)),
                                        by=list(year, da, nb, species_evo_type, 
                                                directional_speed, var)]
df_all_rn_without_outliers[year==0]
nb_range_last_rn_year<-df_all_rn_without_outliers[year==0]
dim(nb_range_last_rn_year)
dim()
length(unique(nb_range_last_rn_year$SP_ID))

saveRDS(nb_range_last_rn_year, "../Data/niche_traits/niche_traits_last_year_rn.rda")
saveRDS(nb_range_year_rn, "../Data/niche_traits/nb_range_year_rn.rda")
table(nb_range_last_rn_year$species_evo_type)



df_all1_without_outliers_with_fn<-readRDS("../Data/niche_traits/niche_traits_fn_without_outlier.rda")
df_all1_without_outliers_with_fn$environments<-NULL
df_all1_without_outliers_with_fn$nb_v<-NULL
df_all1_without_outliers_with_fn$group_id<-NULL
df_all1_without_outliers_with_fn$evo_type<-NULL
df_all1_without_outliers_with_fn$species_evo_level<-NULL
cols<-c("global_id", "nb", "da", "year", "SP_ID", "V_L", "V_min", "V_max", "V_mean", "V_range", "species_evo_type", "directional_speed")
df_all1_next_year<-df_all1_without_outliers_with_fn[, ..cols]
df_all1_next_year$year<-df_all1_next_year$year-1
colnames(df_all1_next_year)[c(7, 8, 9, 10)]<-c("V_min_next", "V_max_next", "V_mean_next", "V_range_next")
df_all<-merge(df_all1_without_outliers_with_fn, df_all1_next_year, 
              by=c("global_id", "nb", "da", "year", "SP_ID", "V_L", "species_evo_type", "directional_speed"),
              all.x=T, all.y=F)
df_all$V_min_next_delta<-df_all$V_min_next - df_all$V_min
df_all$V_max_next_delta<-df_all$V_max_next - df_all$V_max
df_all$V_mean_next_delta<-df_all$V_mean_next - df_all$V_mean
df_all$V_range_next_delta<-df_all$V_range_next - df_all$V_range
df_all$V_range_next_delta_ratio<-df_all$V_range_next / df_all$V_range

saveRDS(df_all, "../Data/niche_traits/niche_traits_fn_without_outlier_with_next_year.rda")

df_all_se_nb_da<-df_all[, .(V_min_delta=mean(V_min_delta, na.rm=T),
                      V_max_delta=mean(V_max_delta, na.rm=T),
                      V_mean_delta=mean(V_mean_delta, na.rm=T),
                      nb_delta=mean(nb_delta, na.rm=T),
                      nb_delta_ratio=mean(nb_delta_ratio, na.rm=T),
                      V_min_next=mean(V_min_next, na.rm=T),
                      V_max_next_delta=mean(V_max_next_delta, na.rm=T),
                      V_mean_next_delta=mean(V_mean_next_delta, na.rm=T),
                      V_range_next_delta=mean(V_range_next_delta, na.rm=T),
                      V_range_next_delta_ratio=mean(V_range_next_delta_ratio, na.rm=T)),
                  by=list(nb, da, year, V_L, species_evo_type, directional_speed)]

df_all_se_nb<-df_all[, .(V_min_delta=mean(V_min_delta, na.rm=T),
                            V_max_delta=mean(V_max_delta, na.rm=T),
                            V_mean_delta=mean(V_mean_delta, na.rm=T),
                            nb_delta=mean(nb_delta, na.rm=T),
                            nb_delta_ratio=mean(nb_delta_ratio, na.rm=T),
                            V_min_next=mean(V_min_next, na.rm=T),
                            V_max_next_delta=mean(V_max_next_delta, na.rm=T),
                            V_mean_next_delta=mean(V_mean_next_delta, na.rm=T),
                            V_range_next_delta=mean(V_range_next_delta, na.rm=T),
                            V_range_next_delta_ratio=mean(V_range_next_delta_ratio, na.rm=T)),
                        by=list(nb, year, V_L, species_evo_type, directional_speed)]

df_all_se<-df_all[, .(V_min_delta=mean(V_min_delta, na.rm=T),
                         V_max_delta=mean(V_max_delta, na.rm=T),
                         V_mean_delta=mean(V_mean_delta, na.rm=T),
                         nb_delta=mean(nb_delta, na.rm=T),
                         nb_delta_ratio=mean(nb_delta_ratio, na.rm=T),
                         V_min_next_delta=mean(V_min_next, na.rm=T),
                         V_max_next_delta=mean(V_max_next_delta, na.rm=T),
                         V_mean_next_delta=mean(V_mean_next_delta, na.rm=T),
                         V_range_next_delta=mean(V_range_next_delta, na.rm=T),
                         V_range_next_delta_ratio=mean(V_range_next_delta_ratio, na.rm=T)),
                     by=list(year, V_L, species_evo_type, directional_speed)]


saveRDS(df_all_se_nb_da, "../Data/niche_traits/niche_traits_fn_se_nb_da_without_outlier_with_next_year.rda")
saveRDS(df_all_se_nb, "../Data/niche_traits/niche_traits_fn_se_nb_without_outlier_with_next_year.rda")
saveRDS(df_all_se, "../Data/niche_traits/niche_traits_fn_se_without_outlier_with_next_year.rda")

df_all_last_year<-df_all1_without_outliers_with_fn[year==1200]
saveRDS(df_all_last_year, "../Data/niche_traits/niche_traits_fn_without_outlier_last_year.rda")
