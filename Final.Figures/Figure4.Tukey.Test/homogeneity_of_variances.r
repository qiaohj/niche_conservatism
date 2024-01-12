library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(car)
library(pracma)
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
    if (F){
      d_sum<-item[, .(N_SPECIATION=sum(N_SPECIATION),
                      to=min(to)), by=c("global_id", "nb", "da", "label")]
      xx<-d_sum[N_SPECIATION==0]
      table(xx$to)
    }
    if (!is.na(com$nb)){
      item<-item[nb==com$nb]
    }
    if (!is.na(com$da)){
      item<-item[da==com$da]
    }
    
    item<-formatLabels(item)
    if (F){
      item_mean<-item[, .(net_dr=mean(net_dr)), by=list(label, global_id)]
      item<-item[from %in% seq(-1000,-100, 100)]
      item$net_dr_root3<-nthroot(item$net_dr, 3)
      plant.lm <- lm(as.formula(sprintf("%s ~ label", "net_dr_root3")), data = item)
      dw1<-durbinWatsonTest(plant.lm)
      
      plant.lm2 <- lm(as.formula(sprintf("%s ~ label", "net_dr")), data = item)
      dw2<-durbinWatsonTest(plant.lm2)
      
      
      plant.av <- aov(plant.lm)
      summary(plant.av)
      
      tukey.test <- TukeyHSD(plant.av)
      #tukey.test
      #plot(tukey.test)
      tukey.test.df<-data.table(tukey.test$label)
      tukey.test.df$label<-row.names(tukey.test$label)
      tukey.test.df<-tukey.test.df[grepl(tested_label, label)]
      
      
      tukey.test.df$p_label<-""
      colnames(tukey.test.df)[4]<-"p_adj"
    }
    
    labels<-unique(item$label)
    item_N<-item[, .(N=.N), by=list(global_id, nb, da, from)]
    item_merged<-merge(item, item_N, by=c("global_id", "nb", "da", "from"))
    item_merged<-item_merged[N==10]
    
    table(item_merged$label)
    #library(lme4)
    
    #model_lmer<-lmer(net_dr~label+(1|global_id)+(1|nb)+(1|da)+(1|from), data=item_merged)
    
    #model_lmer<-lmer(net_dr~label+(1|global_id)+(1|nb)+(1|da)+(1|from), data=item_merged)
    
    ll<-labels[1]
    for (ll in labels){
      item_item<-item_merged[label==ll & from %in% seq(-1000, -100, 100)]
      item_item$run<-sprintf("%d-%s-%s", item_item$global_id, item_item$nb, item_item$da)
      item_item_N<-item_item[, .(N=.N), by=list(global_id, nb, da, label, from)]
      #model <- lm(as.formula(sprintf("%s ~ run", "net_dr")), data = item_item)
      var<-"net_dr"
      for (var in c("net_dr", "N_SPECIATION", "N_EXTINCTION")){
        item_item_N<-item_item[, .(N=.N), by=list(run)]
        item_item<-item_item[run %in% item_item_N[N>2]$run]
        fligner.test(nthroot(pull(item_item[, ..var]), 3), item_item$run)
        
        fligner.test(pull(item_item[, ..var]), item_item$run)
        
        result_item<-data.table(chi.squared=obj$statistic,
                                p.value=obj$p.value,
                                Df=obj$parameter,
                                group=ll,
                                nb=com$nb,
                                da=com$da)
      }
      #leveneTest(net_dr ~ run, data = item_item)
      #bartlett.test(N_EXTINCTION ~ run, data = item_item)
    }
    
    
    
    df_result_list[[length(df_result_list)+1]]<-df_result
    
  }
  df_result_list<-rbindlist(df_result_list)
  unique(df_result_list[, c("N", "NB", "DA")])
  saveRDS(df_result_list, "../Figures/Figure4.Tukey.Test/TukeyHSD_by_species_200.rda")
  write.csv(df_result_list, "../Figures/Figure4.Tukey.Test/TukeyHSD_by_species_200.csv", row.names = F)
}
coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
i=1
df_result_list<-readRDS("../Figures/Figure4.Tukey.Test/TukeyHSD_by_species_200.rda")
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
  
  
  ggsave(p, filename=sprintf("../Figures/Figure4.Tukey.Test/TukeyHSD_%s_%s_200.png", com$nb, com$da),
         width=12, height=3)
  
  ggsave(p, filename=sprintf("../Figures/Figure4.Tukey.Test/TukeyHSD_%s_%s_200.pdf", com$nb, com$da),
         width=12, height=3)
}
