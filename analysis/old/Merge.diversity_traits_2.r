library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(20)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))

diversity_se_all_df_se1<-
  readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", "NARROW", "POOR"))


diversity_se_all_df_se2<-
  readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", "NARROW", "GOOD"))


diversity_se_all_df_se3<-
  readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", "BROAD", "POOR"))


diversity_se_all_df_se4<-
  readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", "BROAD", "GOOD"))

#diversity_df<-diversity_se_all_df_se1
diversity_df<-rbindlist(list(diversity_se_all_df_se1, diversity_se_all_df_se2,
                             diversity_se_all_df_se3, diversity_se_all_df_se4))

diversity_df<-diversity_df[, .(N_SPECIES=sum(N_SPECIES),
                               N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                           by=list(year, global_id, evo_type, species_evo_type,
                                   directional_speed, species_evo_level)]

saveRDS(diversity_df, "../Data/diversity.rda")
