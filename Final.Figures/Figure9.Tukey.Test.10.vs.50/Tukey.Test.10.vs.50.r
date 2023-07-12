library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  
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
    df_net_dr<-TukeyHSD_B("net_dr", item, "0.5")
    df_net_dr<-df_net_dr[label %in% c("expansion-directional (0.1)-expansion-directional (0.5)",
                                      "expansion-omnidirectional (0.1)-expansion-omnidirectional (0.5)",
                                      "shift-directional (0.1)-shift-directional (0.5)")]
    df_R_SPECIATION_SPECIES<-TukeyHSD_B("R_SPECIATION_SPECIES", item, "0.5")
    df_R_SPECIATION_SPECIES<-df_R_SPECIATION_SPECIES[label %in% c("expansion-directional (0.1)-expansion-directional (0.5)",
                                                                  "expansion-omnidirectional (0.1)-expansion-omnidirectional (0.5)",
                                                                  "shift-directional (0.1)-shift-directional (0.5)")]
    df_R_EXTINCTION_SPECIES<-TukeyHSD_B("R_EXTINCTION_SPECIES", item, "0.5")
    df_R_EXTINCTION_SPECIES<-df_R_EXTINCTION_SPECIES[label %in% c("expansion-directional (0.1)-expansion-directional (0.5)",
                                                                  "expansion-omnidirectional (0.1)-expansion-omnidirectional (0.5)",
                                                                  "shift-directional (0.1)-shift-directional (0.5)")]
    
    df_result<-rbindlist(list(df_net_dr, 
                              df_R_SPECIATION_SPECIES, 
                              df_R_EXTINCTION_SPECIES))
    df_result$diff_str<-round(df_result$diff, 3)
    df_result$NB<-com$nb
    df_result$DA<-com$da
    df_result_list[[length(df_result_list)+1]]<-df_result
    
  }
  df_result_list<-rbindlist(df_result_list)
  saveRDS(df_result_list, "../Figures/Figure9.Tukey.Test.10.vs.50/TukeyHSD_10.vs.50.rda")
  
}
coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
i=1
df_result_list<-readRDS("../Figures/Figure9.Tukey.Test.10.vs.50/TukeyHSD_10.vs.50.rda")
df_result_list<-formatLabelXY(df_result_list)
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
  
  type.labs <- c("net_dr"= "net per capita diversification rate",
                 "R_EXTINCTION_SPECIES"=  "net extinction",
                 "R_SPECIATION_SPECIES" ="net speciation")
  
  df_result[type!="net_dr"]$diff <- df_result[type!="net_dr"]$diff/1000
  df_result[type!="net_dr"]$lwr <- df_result[type!="net_dr"]$lwr/1000
  df_result[type!="net_dr"]$upr <- df_result[type!="net_dr"]$upr/1000
  
  df_result$diff_str<-round(df_result$diff, 3)
  df_result$alternative<-""
  df_result[diff>0]$alternative<-"Greater"
  df_result[diff<0]$alternative<-"Less"
  df_result[p_label==""]$alternative<-"No sig diff"
  
  
  
  table(df_result$alternative)
  df_result$diff_label<-sprintf("%.3f %s", df_result$diff_str, df_result$p_label)
  p<-ggplot(df_result)+geom_errorbarh(aes(y=label_x, xmin=lwr, xmax=upr, color=alternative), height=0.1)+
    geom_vline(aes(xintercept=0), linetype=2, color="#444444")+
    geom_point(aes(y=label_x, x=diff, color=alternative), size=0.5)+
    geom_text(aes(y=label_x, x=upr, label=p_label), hjust=0.5, vjust=-0.2, size=2)+
    scale_y_discrete(limits=rev)+
    facet_wrap(~type, scale="free_x", labeller = 
                 labeller(type = type.labs), nrow=1, ncol=3)+
    scale_color_manual(values=c("#EE6677", "#000000", "#4477AA"), 
                       breaks=c("Greater", "No sig diff", "Less"))+
    theme_bw()+
    guides(y = ggh4x::guide_axis_nested(delim = "&"))+
    theme(legend.position = c(0.1, 0.2),
          legend.title = element_blank(),
          legend.background = element_rect(fill=bg),
          legend.key.size = unit(5, "mm"),
          legend.text = element_text(size=7, margin = margin(t = 5)),
          legend.box.spacing = unit(1, "mm"),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.spacing.y = unit(1.0, 'mm'),
          ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
          ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          panel.spacing.x = unit(8, "mm"))
  
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))
  p
  
  
  ggsave(p, filename=sprintf("../Figures/Figure9.Tukey.Test.10.vs.50/TukeyHSD_10.vs.50_%s_%s.png", com$nb, com$da),
         width=10, height=3)
  
  ggsave(p, filename=sprintf("../Figures/Figure9.Tukey.Test.10.vs.50/TukeyHSD_10.vs.50_%s_%s.pdf", com$nb, com$da),
         width=10, height=3)
}
