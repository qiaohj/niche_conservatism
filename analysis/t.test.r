library(data.table)
library(ggplot2)
library(agricolae)
library(dplyr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

d<-readRDS("../Data/tslm_and_glm/d_ndr.rda")

d_null<-d[species_evo_type==1]
d_null<-d_null[, c("from", "to", "nb", "da", "global_id")]
d_null$tag<-1
d_with_null<-merge(d, d_null, 
                   by=c("from", "to", "nb", "da", "global_id"))

var<-"net_dr"




my.pairwise.t.test<-function(item, var, side){
  t.test<-pairwise.t.test(pull(item[, ..var]), 
                                   item$label, 
                                   p.adjust.method="bonferroni", 
                                   alternative=side, paired=T)
  
  
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
for (i in c(1:nrow(coms))){
  com<-coms[i]
  print(i)
  item<-d[from>=-1000]
  item<-item[from %in% seq(-1000, -100, by=100)]
  if (!is.na(com$nb)){
    item<-item[nb==com$nb]
  }
  if (!is.na(com$da)){
    item<-item[da==com$da]
  }
  table(item$outlier)
  #ggplot(item)+geom_boxplot(aes(x=label, y=net_dr))
  
  ##p.adjust.method= c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
  #   "fdr", "none")
  item_N<-item[, .(N=.N), by=list(from, to, nb, da, global_id)]
  item_with_N<-merge(item, item_N, by=c("from", "to", "nb", "da", "global_id"))
  item_with_N<-item_with_N[N==10]
  length(unique(item_with_N$global_id))
  
  
  table(item_with_N$label)
  two.side.p.table.net_dr<-my.pairwise.t.test(item_with_N, "net_dr", "two.sided")
  less.t.test.net_dr<-my.pairwise.t.test(item_with_N, "net_dr", "less")
  greater.t.test.net_dr<-my.pairwise.t.test(item_with_N, "net_dr", "greater")
  
  two.side.p.table.R_SPECIATION_SPECIES<-my.pairwise.t.test(item_with_N, "R_SPECIATION_SPECIES", "two.sided")
  less.t.test.R_SPECIATION_SPECIES<-my.pairwise.t.test(item_with_N, "R_SPECIATION_SPECIES", "less")
  greater.t.test.R_SPECIATION_SPECIES<-my.pairwise.t.test(item_with_N, "R_SPECIATION_SPECIES", "greater")
  
  two.side.p.table.R_EXTINCTION_SPECIES<-my.pairwise.t.test(item_with_N, "R_EXTINCTION_SPECIES", "two.sided")
  less.t.test.R_EXTINCTION_SPECIES<-my.pairwise.t.test(item_with_N, "R_EXTINCTION_SPECIES", "less")
  greater.t.test.R_EXTINCTION_SPECIES<-my.pairwise.t.test(item_with_N, "R_EXTINCTION_SPECIES", "greater")
  
  df_result<-rbindlist(list(two.side.p.table.net_dr, less.t.test.net_dr, greater.t.test.net_dr,
                            two.side.p.table.R_SPECIATION_SPECIES, 
                            less.t.test.R_SPECIATION_SPECIES,
                            greater.t.test.R_SPECIATION_SPECIES,
                            two.side.p.table.R_EXTINCTION_SPECIES,
                            less.t.test.R_EXTINCTION_SPECIES,
                            greater.t.test.R_EXTINCTION_SPECIES))
  df_result$p_str<-round(df_result$p_value, 3)
  saveRDS(df_result, sprintf("../Figures/20230616/t.test/t.test_by_species_%s_%s_paired_fixed_window.rda", com$nb, com$da))
  write.csv(df_result, sprintf("../Figures/20230616/t.test/t.test_by_species_%s_%s_paired_fixed_window.csv", com$nb, com$da), row.names = F)
  
  df1<-df_result[side!="two.sided" & p_label!=""]
  df2<-unique(df_result[side=="two.sided", c("V1", "V2", "var")])
  df1<-merge(df1, df2, by=c("V1", "V2", "var"), all=T)
  df1[is.na(p_label)]$p_label<-"-"
  df1[is.na(side)]$side<-"no sig dif"
  
  p<-ggplot(df1)+
    geom_tile(aes(x=var, y=V1, fill=side))+
    geom_text(aes(x=var, y=V1, label=p_label))+
    ggtitle(sprintf("%s_%s", com$nb, com$da))+
    scale_fill_manual(values=c("#D55E00", "#0072B2", "lightgrey"),
                      breaks=c("greater", "less", "no sig dif"))
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))
  p
  
  
  ggsave(p, filename=sprintf("../Figures/20230616/t.test/t.test_%s_%s_paired_fixed_window.png", com$nb, com$da), width=10, height=6)
}
