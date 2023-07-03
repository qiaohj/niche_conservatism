library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  d<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction_rolling_window.rda")
  
  d$outlier<-"F"
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  d[global_id %in% unique(outliers$global_id)]$outlier<-"T"
  d<-d[outlier=="F"]
  d<-d[((directional_speed %in% c(0) & species_evo_type==1) |
          (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
          (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
  
  d$R_SPECIATION_SPECIES<-d$N_SPECIATION/d$N_SPECIES * 1000
  d$R_EXTINCTION_SPECIES<-d$N_EXTINCTION/d$N_SPECIES * 1000
  d<-d[!is.nan(R_SPECIATION_SPECIES)]
  
  d$evo_type<-format_evoType(d$species_evo_type)
  d[, label:=format_evoLabel(evo_type, directional_speed), 
    by=seq_len(nrow(d))]
  labels<-unique(d$label)
  
  
  d_final<-readRDS("../Data/20230616/tslm/d_N_Species_final.rda")
  
  d[N_SPECIES_BEGIN==0]
  
  d$net_dr<-(d$N_SPECIES_END - d$N_SPECIES_BEGIN)/d$N_SPECIES_BEGIN
  d$net_dr_2<-d$R_SPECIATION_SPECIES - d$R_EXTINCTION_SPECIES
  samp<-sample(nrow(d), 5e3)
  plot(d[samp]$net_dr, d[samp]$R_SPECIATION_SPECIES - d[samp]$R_EXTINCTION_SPECIES)
  cor(d$net_dr, d$R_SPECIATION_SPECIES - d$R_EXTINCTION_SPECIES)
  
  saveRDS(d, "../Data/tslm_and_glm/d_ndr.rda")
  d<-readRDS("../Data/tslm_and_glm/d_ndr.rda")
  d_null<-d[species_evo_type==1]
  d_null<-d_null[, c("from", "to", "nb", "da", "global_id")]
  d_null$tag<-1
  d_with_null<-merge(d, d_null, 
                     by=c("from", "to", "nb", "da", "global_id"))
  ggplot(d)+geom_histogram(aes(y=net_dr))+facet_wrap(~label)
  cor(d$N_SPECIES, d$net_dr)
  cor(d$N_SPECIES, d$N_SPECIATION)
  coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
  i=1
  df_result_list<-list()
  for (i in c(1:nrow(coms))){
    com<-coms[i]
    print(i)
    item<-d[from>=-1000]
    if (!is.na(com$nb)){
      item<-item[nb==com$nb]
    }
    if (!is.na(com$da)){
      item<-item[da==com$da]
    }
    
    item<-formatLabels(item)
    df_N_SPECIES<-TukeyHSD_B("N_SPECIES", item)
    df_R_SPECIATION_SPECIES<-TukeyHSD_B("R_SPECIATION_SPECIES", item)
    df_R_EXTINCTION_SPECIES<-TukeyHSD_B("R_EXTINCTION_SPECIES", item)
    df_net_dr<-TukeyHSD_B("net_dr", item)
    df_net_dr_2<-TukeyHSD_B("net_dr_2", item)
    df_result<-rbindlist(list(df_net_dr, df_N_SPECIES,
                              df_R_SPECIATION_SPECIES, 
                              df_R_EXTINCTION_SPECIES))
    df_result$diff_str<-round(df_result$diff, 3)
    df_result$NB<-com$nb
    df_result$DA<-com$da
    df_result_list[[length(df_result_list)+1]]<-df_result
    
  }
  df_result_list<-rbindlist(df_result_list)
  saveRDS(df_result_list, "../Figures/Figure4.Tukey.Test/TukeyHSD_by_species.rda")
  write.csv(df_result_list, "../Figures/Figure4.Tukey.Test/TukeyHSD_by_species.csv", row.names = F)
}
coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
i=1
df_result_list<-readRDS("../Figures/20230616/TukeyHSD/TukeyHSD_by_species.rda")
df_result_list$label<-gsub("-conservatism", "", df_result_list$label)
df_result_list<-formatLabelX(df_result_list)
for (i in c(1:nrow(coms))){
  com<-coms[i]
  print(i)
  df_result<-df_result_list
  if (is.na(com$nb)){
    df_result<-df_result_list[is.na(NB)]
  }else{
    df_result<-df_result_list[NB==com$nb]
  }
  if (is.na(com$da)){
    df_result<-df_result[is.na(DA)]
  }else{
    df_result<-df_result[DA==com$da]
  }
  df_result<-df_result[type!="N_SPECIES"]
  unique(df_result$type)
  type.labs <- c("net_dr"= "net per capita diversification rate",
                "R_EXTINCTION_SPECIES"=  "net per capita extinction",
                "R_SPECIATION_SPECIES" ="net per capita speciation")
  
  df_result[type!="net_dr"]$diff <- df_result[type!="net_dr"]$diff/1000
  df_result[type!="net_dr"]$lwr <- df_result[type!="net_dr"]$lwr/1000
  df_result[type!="net_dr"]$upr <- df_result[type!="net_dr"]$upr/1000
  
  df_result$diff_str<-round(df_result$diff, 3)
  df_result$alternative<-""
  df_result[diff>0]$alternative<-"Greater"
  df_result[diff<0]$alternative<-"Less"
  df_result[p_label==""]$alternative<-"No sig dif"
  
  
  
  table(df_result$alternative)
  df_result$label<-gsub("-conservatism", "", df_result$label)
  df_result$diff_label<-sprintf("%.3f %s", df_result$diff_str, df_result$p_label)
  p<-ggplot(df_result)+geom_errorbarh(aes(y=label_x, xmin=lwr, xmax=upr, color=alternative), height=0.1)+
    geom_vline(aes(xintercept=0), linetype=2, color="#444444")+
    scale_y_discrete(limits=rev)+
    facet_wrap(~type, scale="free_x", labeller = 
                 labeller(type = type.labs))+
    scale_color_manual(values=c("#EE6677", "#000000", "#4477AA"), breaks=c("Greater", "No sig dif", "Less"))+
    labs(color="")+
    theme_bw()+
    guides(y = ggh4x::guide_axis_nested(delim = "&"))+
    theme(ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
          ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
          axis.title.y = element_blank())
  
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))
  p
  
  
  ggsave(p, filename=sprintf("../Figures/Figure4.Tukey.Test/TukeyHSD_%s_%s.png", com$nb, com$da),
         width=10, height=3)
}
