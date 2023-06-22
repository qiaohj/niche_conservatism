library(data.table)
library(ggplot2)
library(agricolae)
library(dplyr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  d<-readRDS("../Data/distribution_traits/distribution_traits_se_without_nb_da_without_3SD_outliers_global_id.rda")
  
  
  d$outlier<-"F"
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  d[global_id %in% unique(outliers$global_id)]$outlier<-"T"
  d<-d[outlier=="F"]
  d<-d[((directional_speed %in% c(0) & species_evo_type==1) |
          (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
          (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
  d_null<-d[species_evo_type==1]
  d_null<-d_null[, c("year", "global_id")]
  
  
  d_null$tag<-1
  d_with_null<-merge(d, d_null, 
                     by=c("year", "global_id"))
  
  d_with_null$evo_type<-format_evoType(d_with_null$species_evo_type)
  d_with_null[, label:=format_evoLabel(evo_type, directional_speed), 
    by=seq_len(nrow(d_with_null))]
  table(d_with_null$label)
  d_with_null$AREA_PER_GROUP<-d_with_null$N_CELLS/d_with_null$N_GROUP
  saveRDS(d_with_null, "../Data/distribution_traits/distribution_traits_se_without_nb_da_without_3SD_outliers_global_id_t.test.rda")
}


my.pairwise.t.test<-function(item, var, side){
  t.test<-pairwise.t.test(pull(item[, ..var]), 
                          item$label, 
                          p.adjust.method="bonferroni", 
                          alternative=side)
  p.table<-data.table(as.table(t.test$p.value))
  p.table<-p.table[V2=="conservatism"]
  colnames(p.table)[3]<-"p_value"
  p.table$p_label<-""
  p.table[p_value<0.05]$p_label<-"*"
  p.table[p_value<0.01]$p_label<-"**"
  p.table[p_value<0.001]$p_label<-"***"
  p.table$var<-var
  p.table$side<-side
  p.table
}

coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
i=1
var="N_GROUP"
for (i in c(1:nrow(coms))){
  com<-coms[i]
  print(i)
  item<-d_with_null[year>=-900]
  if (!is.na(com$nb)){
    item<-item[nb==com$nb]
  }
  if (!is.na(com$da)){
    item<-item[da==com$da]
  }
  #ggplot(item)+geom_boxplot(aes(x=label, y=N_GROUP))
  
  ##p.adjust.method= c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")
  two.side.p.table.N_GROUP<-my.pairwise.t.test(item, "N_GROUP", "two.sided")
  less.t.test.N_GROUP<-my.pairwise.t.test(item, "N_GROUP", "less")
  greater.t.test.N_GROUP<-my.pairwise.t.test(item, "N_GROUP", "greater")
  
  two.side.p.table.AREA_PER_GROUP<-my.pairwise.t.test(item, "AREA_PER_GROUP", "two.sided")
  less.t.test.AREA_PER_GROUP<-my.pairwise.t.test(item, "AREA_PER_GROUP", "less")
  greater.t.test.AREA_PER_GROUP<-my.pairwise.t.test(item, "AREA_PER_GROUP", "greater")
  
  two.side.p.table.N_CELLS<-my.pairwise.t.test(item, "N_CELLS", "two.sided")
  less.t.test.N_CELLS<-my.pairwise.t.test(item, "N_CELLS", "less")
  greater.t.test.N_CELLS<-my.pairwise.t.test(item, "N_CELLS", "greater")
  
  df_result<-rbindlist(list(two.side.p.table.N_GROUP, 
                            less.t.test.N_GROUP, 
                            greater.t.test.N_GROUP,
                            two.side.p.table.AREA_PER_GROUP, 
                            less.t.test.AREA_PER_GROUP,
                            greater.t.test.AREA_PER_GROUP,
                            two.side.p.table.N_CELLS,
                            less.t.test.N_CELLS,
                            greater.t.test.N_CELLS))
  df_result$p_str<-round(df_result$p_value, 3)
  saveRDS(df_result, sprintf("../Figures/20230616/t.test/t.test_by_species_distribution_%s_%s.rda", com$nb, com$da))
  write.csv(df_result, sprintf("../Figures/20230616/t.test/t.test_by_species_distribution_%s_%s.csv", com$nb, com$da), row.names = F)
  
  df_result<-rbindlist(list(two.side.p.table.N_GROUP, 
                            less.t.test.N_GROUP, 
                            greater.t.test.N_GROUP))
  
  p<-ggplot(df_result[side!="two.sided" & p_label!=""])+
    geom_tile(aes(x=var, y=V1, fill=side))+
    geom_text(aes(x=var, y=V1, label=p_label))+
    ggtitle(sprintf("%s_%s", com$nb, com$da))+
    scale_fill_manual(values=c("#D55E00", "#0072B2", "white"),
                      breaks=c("greater", "less", "no sig dif"))
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))
  p
  
  
  ggsave(p, filename=sprintf("../Figures/20230616/t.test/t.test_distribution_%s_%s.png", com$nb, com$da), width=10, height=6)
}
