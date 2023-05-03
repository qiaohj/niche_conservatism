library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(20)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))
outlier_type<-"IQR"
diversity_se_all_df_se1<-
  readRDS(sprintf("../Data/diversity_items/%s_%s_without_%s_outliers.rda", "NARROW", "POOR", outlier_type))


diversity_se_all_df_se2<-
  readRDS(sprintf("../Data/diversity_items/%s_%s_without_%s_outliers.rda", "NARROW", "GOOD", outlier_type))


diversity_se_all_df_se3<-
  readRDS(sprintf("../Data/diversity_items/%s_%s_without_%s_outliers.rda", "BROAD", "POOR", outlier_type))


diversity_se_all_df_se4<-
  readRDS(sprintf("../Data/diversity_items/%s_%s_without_%s_outliers.rda", "BROAD", "GOOD", outlier_type))

#diversity_df<-diversity_se_all_df_se1
diversity_df<-rbindlist(list(diversity_se_all_df_se1, diversity_se_all_df_se2,
                             diversity_se_all_df_se3, diversity_se_all_df_se4))

diversity_df<-diversity_df[, .(N_SPECIES=sum(N_SPECIES),
                               N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                           by=list(year, global_id, evo_type, species_evo_type,
                                   directional_speed, species_evo_level)]

saveRDS(diversity_df,sprintf("../Data/diversity/diversity_without_%s_outliers.rda", outlier_type))
