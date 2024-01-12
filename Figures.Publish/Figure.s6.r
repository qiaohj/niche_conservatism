library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(reshape2)
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
  d<-d[from>=-1000]
  d<-d[from %in% seq(from=-1000, to=-100, by=100)]
  
  coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
  i=1
  coms<-coms[c(5,6,8,9)]
  N_list<-list()
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
    
    N<-item[, .(N=.N), by=list(label_x, nb, da)]
    N_list[[length(N_list)+1]]<-N
    
  }
  N_list<-rbindlist(N_list)
  N_list$label_y<-sprintf("%s/%s", N_list$nb, N_list$da)
  N_matrix<-acast(N_list, label_x ~ label_y, value.var="N")
  write.csv(N_matrix, "../Figures.Publish/Figure.S6/Figure.s6.N.csv")
  
}

coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
i=1
df_result_list<-readRDS("../Figures/Figure4.Tukey.Test/TukeyHSD_by_species.fixed.window.rda")
df_result_list$label<-gsub("-conservatism", "", df_result_list$label)
df_result_list<-formatLabelX(df_result_list)

fwrite(df_result_list[!is.na(NB) & !is.na(DA), 
                      c("diff", "lwr", "upr", "p_adj", "label_x", "type")], 
       "../Figures.Publish/Data/Figure.S6/Figure.S6.csv")
coms<-coms[c(5,6,8,9)]
N_list<-list()
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
  N<-df_result[, .(N=.N), by=list(type, label)]
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
  df_result$label<-gsub("-conservatism", "", df_result$label)
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
    theme(legend.position = c(0.95, 0.2),
          legend.title = element_blank(),
          legend.background = element_rect(fill=bg),
          legend.key.size = unit(5, "mm"),
          legend.text = element_text(size=7),
          legend.box.spacing = unit(1, "mm"),
          legend.spacing.y = unit(1.0, 'mm'),
          ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
          ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          panel.spacing.x = unit(8, "mm"))
  
  #scale_x_discrete(guide = guide_axis(n.dodge = 2))
  p
  
  ggsave(p, filename=sprintf("../Figures.Publish/Figures/Figure.S6/Figure.S6_%s_%s.png", com$nb, com$da),
         width=12, height=3)
  ggsave(p, filename=sprintf("../Figures.Publish/Figures/Figure.S6/Figure.S6_%s_%s.pdf", com$nb, com$da),
         width=12, height=3)
}
