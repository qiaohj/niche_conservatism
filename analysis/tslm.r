library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
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
  d[is.nan(R_SPECIATION_SPECIES)]$R_SPECIATION_SPECIES<-0
  d$R_EXTINCTION_SPECIES<-d$N_EXTINCTION/d$N_SPECIES * 1000
  d[is.nan(R_EXTINCTION_SPECIES)]$R_EXTINCTION_SPECIES<-0
  #d$net_dr<-(d$R_SPECIATION_SPECIES - d$R_EXTINCTION_SPECIES)/1000
  #d$SPECIES_PER_SPECIATION<-d$N_SPECIES/d$N_SPECIATION
  
  d$evo_type<-format_evoType(d$species_evo_type)
  d[, label:=format_evoLabel(evo_type, directional_speed), 
    by=seq_len(nrow(d))]
  labels<-unique(d$label)
  
  
  d_next<-d
  d_next$from<-d_next$from - 100
  d_next<-d_next[, c("from", "N_SPECIES", "species_evo_type", "directional_speed", "nb", "da", "global_id")]
  d_species_increasing_rate<-merge(d, d_next, by=c("from", "species_evo_type", "directional_speed", "nb", "da", "global_id"))
  d_species_increasing_rate$species_increasing_rate<-
    (d_species_increasing_rate$N_SPECIES.y - d_species_increasing_rate$N_SPECIES.x)/
    d_species_increasing_rate$N_SPECIES.x
  saveRDS(d, "../Data/tslm/d_ratio.rda")
  
  
  d_final<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction.rda")
  d_final<-d_final[year==0]
  d_final$outlier<-"F"
  d_final[global_id %in% unique(outliers$global_id)]$outlier<-"T"
  d_final<-d_final[outlier=="F"]
  d_final<-d_final[((directional_speed %in% c(0) & species_evo_type==1) |
                      (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                      (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
  d_final$evo_type<-format_evoType(d_final$species_evo_type)
  d_final[, label:=format_evoLabel(evo_type, directional_speed), 
          by=seq_len(nrow(d_final))]
  
  d_final_N<-d_final[, .(N=.N), by=c("species_evo_type", "directional_speed", "nb", "da")]
  colnames(d_final)[5]<-"N_SPECIES_FINAL"
  saveRDS(d_final, "../Data/tslm/d_N_Species_final.rda")
  
  
  d_max<-readRDS("../Data/N_speciation_extinction/N_species_per_speciation.rda")
  
  d_max<-d_max[!is.infinite(SPECIES_PER_SPECIATION)]
  d_max$evo_type<-format_evoType(d_max$species_evo_type)
  d_max[, label:=format_evoLabel(evo_type, directional_speed), 
        by=seq_len(nrow(d_max))]
  hist(d_max$SPECIES_PER_SPECIATION)
  d_max$N_SPECIATION<-NULL
  d_ndr<-merge(d, d_max, by=c("species_evo_type", "directional_speed", "nb", "da", "global_id", "evo_type", "label"))
  #d_ndr$net_dr<-d_ndr$R_SPECIATION_SPECIES * d_ndr$SPECIES_PER_SPECIATION - d_ndr$R_EXTINCTION_SPECIES
  d_ndr$net_dr<-d_ndr$N_SPECIATION * d_ndr$SPECIES_PER_SPECIATION - d_ndr$N_EXTINCTION
  
  d_ndr$net_dr_erin<-d_ndr$R_SPECIATION_SPECIES - d_ndr$R_EXTINCTION_SPECIES
  
  cor(d_ndr$net_dr, d_ndr$N_SPECIATION)
  cor(d_ndr$net_dr, d_ndr$N_EXTINCTION)
  sample_i<-sample(nrow(d_ndr), 1e4)
  
  plot(d_ndr[sample_i]$N_SPECIATION, d_ndr[sample_i]$N_EXTINCTION)
  plot(d_ndr[sample_i]$N_SPECIATION, d_ndr[sample_i]$net_dr)
  
  plot(d_ndr[sample_i]$R_SPECIATION_SPECIES, d_ndr[sample_i]$R_EXTINCTION_SPECIES)
  
  saveRDS(d_ndr, "../Data/tslm/d_ndr.rda")

  
  ggplot(d_max)+geom_boxplot(aes(y=SPECIES_PER_SPECIATION, x=label))
  range(d_max$SPECIES_PER_SPECIATION)
  ggplot(d_ndr)+geom_histogram(aes(y=net_dr))+facet_wrap(~label)
  cor(d_ndr$N_SPECIES, d_ndr$net_dr)
  cor(d_ndr$N_SPECIES, d_ndr$N_SPECIATION)
  cor(d_ndr$N_SPECIES, d_ndr$N_SPECIATION)
  
  df_N_SPECIES<-TukeyHSD_B("N_SPECIES", d)
  df_N_SPECIES_FINAL<-TukeyHSD_B("N_SPECIES_FINAL", d_final)
  df_species_increasing_rate<-TukeyHSD_B("species_increasing_rate", d_species_increasing_rate)
  df_R_SPECIATION_SPECIES<-TukeyHSD_B("R_SPECIATION_SPECIES", d)
  df_R_EXTINCTION_SPECIES<-TukeyHSD_B("R_EXTINCTION_SPECIES", d)
  df_SPECIES_PER_SPECIATION<-TukeyHSD_B("SPECIES_PER_SPECIATION", d_max)
  df_net_dr<-TukeyHSD_B("net_dr", d_ndr)
  df_net_dr_erin<-TukeyHSD_B("net_dr_erin", d_ndr)
  
  df_result<-rbindlist(list(df_net_dr, df_net_dr_erin, df_N_SPECIES, df_N_SPECIES_FINAL,
                            df_species_increasing_rate,
                            df_R_SPECIATION_SPECIES, df_R_EXTINCTION_SPECIES,
                            df_SPECIES_PER_SPECIATION))
  saveRDS(df_result, "../Figures/TukeyHSD/TukeyHSD_by_species.rda")
  write.csv(df_result, "../Figures/TukeyHSD/TukeyHSD_by_species.csv", row.names = F)
  
  df_result<-rbindlist(list(df_net_dr, 
                            df_R_SPECIATION_SPECIES, df_R_EXTINCTION_SPECIES))
  
  df_result$alternative<-""
  df_result[diff>0]$alternative<-"greater"
  df_result[diff<0]$alternative<-"less"
  table(df_result$alternative)
  df_result$label<-gsub("-conservatism", "", df_result$label)
  p<-ggplot(df_result[p_label!="" & !grepl("0.01", label)])+
    geom_tile(aes(x=type, y=label, fill=alternative))+
    geom_text(aes(x=type, y=label, label=alternative))
    #scale_x_discrete(guide = guide_axis(n.dodge = 2))
  p
  
  
  ggsave(p, filename="../Figures/TukeyHSD/TukeyHSD.png", width=10, height=6)
}
if (F){
  df_nb_trait<-readRDS("../Data/niche_traits/niche_traits_fn_without_outlier_with_next_year.rda")
  df_nb_trait<-df_nb_trait[((directional_speed %in% c(0) & species_evo_type==1) |
                              (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                              (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
  df_nb_trait$year<-df_nb_trait$year - 1200
  df_nb_trait$V_mean_next_delta<-df_nb_trait$V_mean_next - df_nb_trait$V_mean
  df_nb_trait$nb_next_delta_ratio<-df_nb_trait$V_range_next / df_nb_trait$V_range
  
  j=-1000
  all_r<-list()
  for (j in seq(from=-1100, to=0, by=10)){
    from_y<-j-100
    to_y<-j
    print(paste(from_y, to_y))
    #sub_df<-df[(year==j),]
    #nb_mean_delta_in_windows+nb_mean_next_delta+nb_delta_ratio_in_windows+nb_next_delta_ratio
    subnodes<-df_nb_trait[between(year, from_y, to_y)]
    subnodes_se<-subnodes[, .(nb_mean_delta_in_windows=max(V_mean_delta, na.rm=T)-min(V_mean_delta, na.rm=T),
                              nb_mean_next_delta=mean(V_mean_next_delta, na.rm=T),
                              nb_delta_ratio_in_windows=max(nb_delta_ratio, na.rm=T)-min(nb_delta_ratio, na.rm=T),
                              nb_next_delta_ratio=mean(nb_next_delta_ratio, na.rm=T)),
                          by=list(global_id, nb, da, V_L, species_evo_type, directional_speed)]
    subnodes_se$from<-from_y
    subnodes_se$to<-to_y
    all_r[[length(all_r)+1]]<-subnodes_se
    if (F){
      x<-df_nb_trait[global_id==81 & nb=="BROAD" &da=="GOOD" & species_evo_type==2 & 
                       directional_speed==0.1 & V_L=="Debiased_Maximum_Monthly_Precipitation"]
      setorder(x, "year")
      x
    }
  }
  all_r_df<-rbindlist(all_r)
  saveRDS(all_r_df, "../Data/tslm/d_nb_traits_by_global_id.rda")
}
if (F){
  all_r_df<-readRDS("../Data/tslm/d_nb_traits_by_global_id.rda")
  all_r_df_1<-all_r_df[, .(N=.N), by=c("global_id", "nb", "da", "V_L", "from", "to")]
  all_r_df_1$N<-NULL
  all_r_df_1$species_evo_type<-1
  all_r_df_1$directional_speed<-0
  all_r_df_1$nb_mean_delta_in_windows<-0
  all_r_df_1$nb_mean_next_delta<-0
  all_r_df_1$nb_delta_ratio_in_windows<-1
  all_r_df_1$nb_next_delta_ratio<-1
  all_r_df<-rbindlist(list(all_r_df, all_r_df_1), use.names = T)
  #d_ratio<-readRDS("../Data/tslm/d_ratio.rda")
  d_ndr<-readRDS("../Data/tslm/d_ndr.rda")
  
  cor(d_ndr$N_SPECIATION * d_ndr$SPECIES_PER_SPECIATION - d_ndr$N_EXTINCTION,
      d_ndr$net_dr)
  #d_ratio_traits<-merge(all_r_df, d_ratio, by=c("from", "to", "global_id", "nb", "da", 
  #                                              "species_evo_type", "directional_speed"))
  d_ratio_traits_ndr<-merge(all_r_df, d_ndr, by=c("from", "to", "global_id", "nb", "da", 
                                                        "species_evo_type", "directional_speed"))
  saveRDS(d_ratio_traits_ndr, "../Data/tslm/d_tslm_full.rda")
  d_ratio_traits_ndr_sample<-d_ratio_traits_ndr[sample(nrow(d_ratio_traits_ndr), 1e4)]
  saveRDS(d_ratio_traits_ndr_sample, "../Data/tslm/d_tslm_sampled.rda")
}

d_ratio_traits_ndr<-readRDS("../Data/tslm/d_tslm_full.rda")
colnames(d_ratio_traits_ndr)

cor(d_ratio_traits_ndr$N_SPECIATION * d_ratio_traits_ndr$SPECIES_PER_SPECIATION -
      d_ratio_traits_ndr$N_EXTINCTION, 
    d_ratio_traits_ndr$net_dr)

d_ratio_traits_ndr$net_dr2<- d_ratio_traits_ndr$R_SPECIATION_SPECIES * d_ratio_traits_ndr$SPECIES_PER_SPECIATION - 
  d_ratio_traits_ndr$R_EXTINCTION_SPECIES
d_ratio_traits_ndr[SPECIES_PER_SPECIATION==max(d_ratio_traits_ndr$SPECIES_PER_SPECIATION)]

d_se<-d_ratio_traits_ndr[, .(nb_mean_delta_in_windows=mean(nb_mean_delta_in_windows, na.rm=T),
                             nb_mean_next_delta=mean(nb_mean_next_delta, na.rm=T),
                             nb_delta_ratio_in_windows=mean(nb_delta_ratio_in_windows, na.rm=T),
                             nb_next_delta_ratio=mean(nb_next_delta_ratio, na.rm=T),
                             net_dr=mean(net_dr, na.rm=T),
                             net_dr2=mean(net_dr2, na.rm=T),
                             R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
                             R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T)),
                         by=c("species_evo_type", "directional_speed", "from", "V_L")]
d_ratio_traits_ndr[, .(cor=cor(nb_delta_ratio_in_windows, net_dr)),
                   by=c("species_evo_type", "directional_speed", "V_L")]
cor(d_ratio_traits_ndr$nb_delta_ratio_in_windows, 
     d_ratio_traits_ndr$net_dr)
coms<-d_se[, .(N=.N), by=c("species_evo_type", "directional_speed", "V_L")]
i=1
result_list_full<-list()
for (i in c(1:nrow(coms))){
  item<-d_se[species_evo_type==coms[i]$species_evo_type & 
               directional_speed==coms[i]$directional_speed & 
               V_L==coms[i]$V_L]
  
  df_ts_full<-item[,c("nb_mean_delta_in_windows", "nb_mean_next_delta",
                 "nb_delta_ratio_in_windows", "nb_next_delta_ratio")]
  
  
  df_ts<-df_ts_full
  if (coms[i]$species_evo_type %in% c(2, 5)){
    df_ts_full$nb_mean_delta_in_windows<-scale(df_ts_full$nb_mean_delta_in_windows)
    df_ts_full$nb_mean_next_delta<-scale(df_ts_full$nb_mean_next_delta)
    df_ts_full$nb_delta_ratio_in_windows<-1
    df_ts_full$nb_next_delta_ratio<-1
    df_ts<-df_ts_full[,c("nb_mean_delta_in_windows", "nb_mean_next_delta")]
  }
  if (coms[i]$species_evo_type %in% c(4, 6)){
    df_ts_full$nb_mean_delta_in_windows<-0
    df_ts_full$nb_mean_next_delta<-0
    df_ts_full$nb_delta_ratio_in_windows<-scale(df_ts_full$nb_delta_ratio_in_windows)
    df_ts_full$nb_next_delta_ratio<-scale(df_ts_full$nb_next_delta_ratio)
    df_ts<-df_ts_full[,c("nb_delta_ratio_in_windows", "nb_next_delta_ratio")]
  }
  var<-"net_dr2"
  for (var in c("net_dr", "net_dr2", "R_SPECIATION_SPECIES", "R_EXTINCTION_SPECIES")){
    print(paste(i, nrow(coms), var))
    #df_ts<-item[,c("nb_delta_ratio_in_windows", "nb_next_delta_ratio")]
    
    if (coms[i]$species_evo_type==1){
      aicc_tslm<-NA
      aic_tslm<-NA
      bic_tslm<-NA
      cor_tslm<-NA
    }else{
      tsobj<-ts(dplyr::pull(item[, ..var]), start = -120, end=-10, freq = 1)
      model<-auto.arima(tsobj, xreg=as.matrix(df_ts), ic = "aic")
      aicc_tslm<-model$aicc
      aic_tslm<-model$aic
      bic_tslm<-model$bic
      net_dr_predict<-forecast(model, xreg=as.matrix(df_ts))
      cor_tslm<-cor(as.vector(net_dr_predict$fitted), dplyr::pull(item[, ..var]))
    }
    
    
    #plot(as.vector(net_dr_predict$fitted), item$net_dr)
    
    model_ts<-auto.arima(tsobj, ic = "aic")
    aicc_ts<-model_ts$aicc
    aic_ts<-model_ts$aic
    bic_ts<-model_ts$bic
    
    net_dr_predict_ts<-forecast(model_ts)
    
    cor_ts<-cor(as.vector(net_dr_predict_ts$fitted), dplyr::pull(item[, ..var]))
    formula_str<-sprintf("%s~nb_mean_delta_in_windows+nb_mean_next_delta+
                             nb_delta_ratio_in_windows+nb_next_delta_ratio", var)
    model_rf<-randomForest(as.formula(formula_str), data=item)
    
    net_dr_predict_rf<-predict(model_rf, data=item)
    
    cor_rf<-cor(net_dr_predict_rf, dplyr::pull(item[, ..var]))
    
    model_lm<-lm(as.formula(formula_str), data=item)
    
    net_dr_predict_lm<-predict(model_lm, data=item)
    aic_lm<-AIC(model_lm)
    bic_lm<-BIC(model_lm)
    
    cor_lm<-cor(net_dr_predict_lm, dplyr::pull(item[, ..var]))
    
    vs<-c(cor_tslm, aic_tslm, bic_tslm, cor_ts, aic_ts, bic_ts,
          cor_lm, aic_lm, bic_lm, cor_rf, NA, NA)
    metrics<-rep(c("cor", "aic", "bic"), 4)
    models<-c(rep("tslm", 3), rep("ts", 3), rep("lm", 3), rep("rf", 3))
    result_list<-list()
    for (j in c(1:length(vs))){
      result_item<-coms[i]
      result_item$value<-vs[j]
      result_item$metrics<-metrics[j]
      result_item$model<-models[j]
      result_list[[length(result_list)+1]]<-result_item
    }
    result_list<-rbindlist(result_list)
    result_list$var<-var
    result_list_full[[length(result_list_full)+1]]<-result_list
  }
}
result_df_full<-rbindlist(result_list_full)
saveRDS(result_df_full, "../Data/tslm/full_models.rda")

library(ggplot2)
library(ggpubr)
result_df_full$evo_type<-format_evoType(result_df_full$species_evo_type)
result_df_full[, label:=format_evoLabel(evo_type, directional_speed), 
           by=seq_len(nrow(result_df_full))]
rm("var")
v<-"cor"
for (v in unique(result_df_full$metrics)){
  p<-ggplot(result_df_full[metrics==v & var!="net_dr2"])+geom_point(aes(x=label, y=value, color=model))+
    facet_grid(var~V_L, scale="free")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(v)
  
  p
  ggsave(p, filename=sprintf("../Figures/tslm/%s.png", v), width=10, height=8)
}

ggplot(
  result_df_full[V_L=="Debiased_Maximum_Monthly_Precipitation" & metrics==v & 
                   var!="net_dr2" & model=="tslm"])+
  geom_point(aes(x=label, y=value, color=model))+
  facet_wrap(~var, scale="free")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle(v)


p1<-ggplot(item)+geom_line(aes(x=from, y=net_dr)) 
p2<-ggplot(item)+geom_line(aes(x=from, y=R_SPECIATION_SPECIES)) 
p3<-ggplot(item)+geom_line(aes(x=from, y=R_EXTINCTION_SPECIES)) 
p4<-ggplot(item)+geom_line(aes(x=from, y=nb_mean_next_delta)) 
p5<-ggplot(item)+geom_line(aes(x=from, y=nb_mean_delta_in_windows)) 
p6<-ggplot(item)+geom_line(aes(x=from, y=nb_next_delta_ratio)) 
p7<-ggplot(item)+geom_line(aes(x=from, y=nb_delta_ratio_in_windows)) 
ggarrange(plotlist=list(p1, p2, p3, p4, p5, p6, p7), nrow=7, ncol=1)

