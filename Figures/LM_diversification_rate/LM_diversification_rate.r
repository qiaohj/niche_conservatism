library(data.table)
library(ggplot2)
library(ggpubr)
library(quantmod)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  
  
}
if (F){
  d<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction.rda")
  #cols<-c("nb", "da", "global_id", "species_evo_type", "directional_speed", "species_evo_level")
  #d[, min_year:=min(year), by=cols]
  
  #d_unique<-unique(d[, ..cols])
  d$outlier<-"F"
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  d[global_id %in% unique(outliers$global_id)]$outlier<-"T"
  d<-d[outlier=="F"]
  d<-d[((directional_speed %in% c(0) & species_evo_type==1) |
          (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
          (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) & 
         species_evo_level==0]
  d
  d$R_SPECIATION_SPECIES_YEAR<-d$N_SPECIATION_YEAR/d$N_SPECIES * 1000
  d[is.nan(R_SPECIATION_SPECIES_YEAR)]$R_SPECIATION_SPECIES_YEAR<-0
  d$R_EXTINCTION_SPECIES_YEAR<-d$N_EXTINCTION_YEAR/d$N_SPECIES* 1000
  d[is.nan(R_EXTINCTION_SPECIES_YEAR)]$R_EXTINCTION_SPECIES_YEAR<-0
  
  d$R_SPECIATION_SPECIES<-d$N_SPECIATION/d$N_ALL_SPECIES * 1000
  d[is.nan(R_SPECIATION_SPECIES)]$R_SPECIATION_SPECIES<-0
  d$R_EXTINCTION_SPECIES<-d$N_EXTINCTION/d$N_ALL_SPECIES * 1000
  d[is.nan(R_EXTINCTION_SPECIES)]$R_EXTINCTION_SPECIES<-0
  #d$net_dr<-d$R_SPECIATION_SPECIES - d$R_EXTINCTION_SPECIES
  
  d$year<-d$year* -1
  i=-1100
  #d$SPECIES_PER_SPECIATION<-d$N_ALL_SPECIES/d$N_SPECIATION
  df_se_list<-list()
  for (i in seq(from=-1100, to=0, by=10)){
    print(i)
    from_y<-i-100
    to_y<-i
    df_item<-d[between(year, from_y, to_y)]
    
    df_se<-df_item[, .(net_dr=mean(net_dr, na.rm=T),
                       N_SPECIES=mean(N_SPECIES, na.rm=T),
                       N_ALL_SPECIES=mean(N_ALL_SPECIES, na.rm=T),
                       N_SPECIATION=mean(N_SPECIATION, na.rm=T),
                       N_EXTINCTION=mean(N_EXTINCTION, na.rm=T),
                       R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
                       R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T),
                       R_SPECIATION_SPECIES_YEAR=mean(R_SPECIATION_SPECIES_YEAR, na.rm=T),
                       R_EXTINCTION_SPECIES_YEAR=mean(R_EXTINCTION_SPECIES_YEAR, na.rm=T),
                       SPECIES_PER_SPECIATION=mean(SPECIES_PER_SPECIATION, na.rm=T)
    ),
    by=list(species_evo_type, directional_speed,
            global_id, nb, da)]
    df_se$from_y<-from_y
    df_se$to_y<-to_y
    df_se_list[[length(df_se_list)+1]]<-df_se
  }
  df_se_full<-rbindlist(df_se_list)
  df_se_full$evo_type<-format_evoType(df_se_full$species_evo_type)
  df_se_full[, label:=format_evoLabel(evo_type, directional_speed), 
         by=seq_len(nrow(df_se_full))]
  labels<-unique(df_se_full$label)
  saveRDS(df_se_full, "../Figures/LM/raw_ndr_by_species.rda")
  null<-df_se_full[label=="conservatism"]
  colnames(null)[c(1, 2, 6:14, 17, 18, 19)]<-
    paste(colnames(null)[c(1, 2, 6:14, 17, 18,)], "null", sep="_")
  i=2
  l<-labels[i]
  df_item<-df_se_full[label==l]
  df_merge<-merge(df_item, null, by=c("global_id",               
                                      "nb", "da", "from_y", "to_y"),
                  all=T)
  df_net_dr<-TukeyHSD_B("net_dr", df_se_full[from_y>=-500])
  df_N_SPECIES<-TukeyHSD_B("N_SPECIES", df_se_full[from_y>=-500])
  df_R_SPECIATION_SPECIES<-TukeyHSD_B("R_SPECIATION_SPECIES", df_se_full)
  df_R_EXTINCTION_SPECIES<-TukeyHSD_B("R_EXTINCTION_SPECIES", df_se_full)
  
  df_result<-rbindlist(list(df_net_dr, df_N_SPECIES, df_R_SPECIATION_SPECIES, df_R_EXTINCTION_SPECIES))
  saveRDS(df_result, "../Figures/TukeyHSD/TukeyHSD_by_species.rda")
  write.csv(df_result, "../Figures/TukeyHSD/TukeyHSD_by_species.csv", row.names = F)
  
  df_result$alternative<-""
  df_result[diff>0]$alternative<-"less"
  df_result[diff<0]$alternative<-"greater"
  table(df_result$alternative)
  
  p<-ggplot(df_result[p_label!=""])+geom_tile(aes(x=type, y=label, fill=alternative))+
    geom_text(aes(x=type, y=label, label=alternative))
  p
  
  ggsave(p, filename="../Figures/TukeyHSD/TukeyHSD.png", width=10, height=6)
  
}
speciation_extinction<-readRDS("../Figures/Figure1.Speciation.Extinction.Rate/figure_data.rda")
speciation_extinction<-speciation_extinction[((directional_speed %in% c(0) & species_evo_type==1) |
              (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
              (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) & 
             species_evo_level==0]
speciation_extinction$evo_type<-format_evoType(speciation_extinction$species_evo_type)

speciation_extinction_all<-speciation_extinction[, .(
  N_SPECIES=sum(N_SPECIES), N_ALL_SPECIES=sum(N_ALL_SPECIES),
  N_SPECIATION=sum(N_SPECIATION),
  N_EXTINCTION=sum(N_EXTINCTION),
  N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
  N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
  R_SPECIATION_SPECIES_YEAR=mean(R_SPECIATION_SPECIES_YEAR) * 1000,
  R_EXTINCTION_SPECIES_YEAR=mean(R_EXTINCTION_SPECIES_YEAR) * 1000,
  R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES) * 1000,
  R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES) * 1000),
  by=list(species_evo_level, species_evo_type, 
          directional_speed, year, evo_type, outlier)]
speciation_extinction_all[, label:=format_evoLabel(evo_type, directional_speed), 
                          by = seq_len(nrow(speciation_extinction_all))]
#speciation_extinction_all$net_dr<-speciation_extinction_all$R_SPECIATION_SPECIES/1000 - 
#  speciation_extinction_all$R_EXTINCTION_SPECIES/1000
hist(speciation_extinction_all[outlier=="F"]$net_dr)

ggplot(speciation_extinction_all[outlier=="F"])+geom_point(aes(x=year, y=net_dr, color=label))

speciation_extinction_all<-speciation_extinction_all[outlier=="F"]

table(speciation_extinction_all$species_evo_type)
speciation_extinction_all$year<-speciation_extinction_all$year * -1
niche_trait<-readRDS("../Data/niche_traits/niche_traits_fn_se_without_outlier_with_next_year.rda")
niche_trait_cm<-niche_trait[species_evo_type==2 & directional_speed==0.10]
niche_trait_cm$species_evo_type<-1
niche_trait_cm$directional_speed<-0
niche_trait<-rbindlist(list(niche_trait, niche_trait_cm))

niche_trait[species_evo_type %in% c(1)]$V_mean_delta<-0
niche_trait[species_evo_type %in% c(1)]$V_min_delta<-0
niche_trait[species_evo_type %in% c(1)]$V_max_delta<-0
niche_trait[species_evo_type %in% c(1)]$V_mean_next_delta<-0
niche_trait[species_evo_type %in% c(1)]$V_min_next_delta<-0
niche_trait[species_evo_type %in% c(1)]$V_max_next_delta<-0
niche_trait[species_evo_type %in% c(1, 2, 5)]$V_range_next_delta<-0
niche_trait[species_evo_type %in% c(1, 2, 5)]$nb_delta<-0
niche_trait[species_evo_type %in% c(1, 2, 5)]$nb_delta_ratio<-1
niche_trait[species_evo_type %in% c(1, 2, 5)]$V_range_next_delta_ratio<-1

niche_trait$year<-niche_trait$year - 1200
table(niche_trait$species_evo_type)
env_df<-readRDS("../Data/env_yearly_avg.rda")
env_df$year<-env_df$year * -1


if (F){
  env_df$type<-"NONE"
  for (v in unique(env_df$var)){
    env_df[var==v][findValleys(env_df[var==v]$mean_v, thresh=0)-1]$type<-"Valley"
    env_df[var==v][findPeaks(env_df[var==v]$mean_v, thresh=0)-1]$type<-"Peak"
  }
  env_df$type<-env_df$v_delta>0
  ggplot(env_df)+
    geom_point(aes(x=year, y=mean_v, color=type))+
    #geom_point(data=env_df[type!="NONE"], aes(x=year, y=mean_v, color=type))+
    facet_wrap(~var, nrow=3, scale="free")
}

table(niche_trait$species_evo_type)
df<-merge(env_df, niche_trait, by.x=c("year", "var"), by.y=c("year", "V_L"))
table(df$species_evo_type)
df<-merge(df, speciation_extinction_all[outlier=="F"], 
          by=c("species_evo_type", "directional_speed", "year"))

df
i=0
df_se_list<-list()
for (i in seq(from=-1100, to=0, by=10)){
  from_y<-i-100
  to_y<-i
  df_item<-df[between(year, from_y, to_y)]
  env_item<-df_item[species_evo_type==1]
  env_item$slope<-"STABLE"
  env_item[v_delta>0]$slope<-"INCREASE"
  env_item[v_delta<0]$slope<-"DECREASE"
  env_se<-env_item[, .(N=.N), by=list(var, slope)]
  env_se<-env_se[slope=="INCREASE"]
  env_se$increase_per<-env_se$N/(nrow(env_item) / 3)
  env_se$slope<-NULL
  env_se$N<-NULL
  df_se<-df_item[, .(net_dr=mean(net_dr, na.rm=T),
                     N_SPECIES=mean(N_SPECIES, na.rm=T),
                     R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
                     R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T),
                nb_next_delta_ratio=mean(V_range_next_delta_ratio, na.rm=T),
                nb_next_delta_ratio_sd=sd(V_range_next_delta_ratio, na.rm=T),
                lower_limit_delta=mean(V_min_delta, na.rm=T),
                lower_limit_delta_sd=sd(V_min_delta, na.rm=T),
                lower_limit_delta_in_windows=max(V_min_delta, na.rm=T)-min(V_min_delta, na.rm=T),
                upper_limit_delta=mean(V_max_delta, na.rm=T),
                upper_limit_delta_sd=sd(V_max_delta, na.rm=T),
                upper_limit_delta_in_windows=max(V_max_delta, na.rm=T)-min(V_max_delta, na.rm=T),
                nb_mean_delta=mean(V_mean_delta, na.rm=T),
                nb_mean_delta_sd=sd(V_mean_delta, na.rm=T),
                nb_mean_delta_in_windows=max(V_mean_delta, na.rm=T)-min(V_mean_delta, na.rm=T),
                nb_delta=mean(nb_delta, na.rm=T),
                nb_delta_sd=sd(nb_delta, na.rm=T),
                nb_delta_in_windows=max(nb_delta, na.rm=T)-min(nb_delta, na.rm=T),
                nb_delta_ratio=mean(nb_delta_ratio, na.rm=T),
                nb_delta_ratio_sd=sd(nb_delta_ratio, na.rm=T),
                nb_delta_ratio_in_windows=max(nb_delta_ratio, na.rm=T)-min(nb_delta_ratio, na.rm=T),
                lower_limit_next_delta=mean(V_min_next_delta, na.rm=T),
                lower_limit_next_delta_sd=sd(V_min_next_delta, na.rm=T),
                upper_limit_next_delta=mean(V_max_next_delta, na.rm=T),
                upper_limit_next_delta_sd=sd(V_max_next_delta, na.rm=T),
                nb_mean_next_delta=mean(V_mean_next_delta, na.rm=T),
                nb_mean_next_delta_sd=sd(V_mean_next_delta, na.rm=T),
                nb_next_delta=mean(V_range_next_delta, na.rm=T),
                nb_next_delta_sd=sd(V_range_next_delta, na.rm=T),
                env_mean=mean(mean_v, na.rm=T),
                env_mean_sd=sd(mean_v, na.rm=T),
                env_mean_delta_in_windows=max(mean_v, na.rm=T)-min(mean_v, na.rm=T),
                env_delta=mean(v_delta, na.rm=T),
                env_delta_sd=sd(v_delta, na.rm=T)
  ),
  by=list(species_evo_type, directional_speed,
          var, evo_type, label)]
  df_se<-merge(df_se, env_se, by="var")
  df_se$from_y<-from_y
  df_se$to_y<-to_y
  df_se_list[[length(df_se_list)+1]]<-df_se
}
df_se<-rbindlist(df_se_list)

saveRDS(df_se, "../Figures/LM/traits_ndr.rda")
#corrlation
colnames(df_se)
vars<-c("lower_limit_delta_in_windows", "upper_limit_delta_in_windows",
        "nb_mean_delta_in_windows", 
        "lower_limit_next_delta", "upper_limit_next_delta", "nb_mean_next_delta",
        
        "nb_delta_in_windows", "nb_delta_ratio_in_windows",
        "nb_next_delta", "nb_next_delta_ratio", 
        
        "env_mean_sd", "env_mean_delta_in_windows",
        "env_delta", "increase_per")
df_se_comb<-df_se[, .(N=.N), by=list(species_evo_type, directional_speed)]
cor_table<-list()

for (i in c(1:nrow(df_se_comb))){
  print(paste(i, nrow(df_se_comb)))
  com_item<-df_se_comb[i]
  item<-df_se[species_evo_type==com_item$species_evo_type &
                directional_speed==com_item$directional_speed]
  for (v1 in vars){
    for (v2 in vars){
      cor<-as.numeric(cor(item[, ..v1], item[, ..v2]))
      itemx<-data.frame(v1=v1, v2=v2, cor=cor, 
                        species_evo_type=com_item$species_evo_type,
                        directional_speed=com_item$directional_speed)
      cor_table[[length(cor_table)+1]]<-itemx
    }
  }
}
cor_df<-rbindlist(cor_table)
cor_df$cor_s<-sprintf("%.2f", round(cor_df$cor * 100)/100)
cor_df<-cor_df[!is.na(cor)]
cor_df$evo_type<-format_evoType(cor_df$species_evo_type)
cor_df[, label:=format_evoLabel(evo_type, directional_speed), 
          by=seq_len(nrow(cor_df))]

p<-ggplot(data=cor_df, aes(v1, v2, fill= cor)) + 
  geom_tile()+geom_text(aes(label=cor_s))+
  scale_fill_gradientn(colours = c("#0072B2", "white", "#CC6666"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~label, nrow=4)
p
ggsave(p, filename="../Figures/LM/cor.png", width=20, height=16)

vars_env<-c("env_mean_sd", "env_mean_delta_in_windows",
        "env_delta", "increase_per")
p<-ggplot(data=cor_df[(v1 %in% vars_env) & (v2 %in% vars_env)], aes(v1, v2, fill= cor)) + 
  geom_tile()+geom_text(aes(label=cor_s))+
  scale_fill_gradientn(colours = c("#0072B2", "white", "#CC6666"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p
ggsave(p, filename="../Figures/LM/cor_env.png", width=5, height=5)


vars_limit<-c("lower_limit_delta_in_windows", "upper_limit_delta_in_windows",
            "nb_mean_delta_in_windows", 
            "lower_limit_next_delta", "upper_limit_next_delta", "nb_mean_next_delta",
            "env_mean_sd", 
            "env_delta", "increase_per")
p<-ggplot(data=cor_df[(v1 %in% vars_limit) & (v2 %in% vars_limit) & (species_evo_type %in% c(2, 5))& cor>=0.8 & v1!=v2], aes(v1, v2, fill= cor)) + 
  geom_tile()+geom_text(aes(label=cor_s))+
  scale_fill_gradientn(colours = c("#0072B2", "white", "#CC6666"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~label, nrow=1)
p
ggsave(p, filename="../Figures/LM/cor_limit.png", width=15, height=5)

vars_nb<-c("nb_delta_in_windows", "nb_delta_ratio_in_windows",
           "nb_next_delta", "nb_next_delta_ratio",
           "nb_mean_delta_in_windows", 
              "nb_mean_next_delta",
              "env_mean_sd", 
              "env_delta", "increase_per")
p<-ggplot(data=cor_df[(v1 %in% vars_nb) & (v2 %in% vars_nb) & (species_evo_type %in% c(3, 4, 6, 7)) & cor>=0.8 & v1!=v2], aes(v1, v2, fill= cor)) + 
  geom_tile()+geom_text(aes(label=cor_s))+
  scale_fill_gradientn(colours = c("#0072B2", "white", "#CC6666"))+
  theme_bw()+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_wrap(~label, nrow=1)
p
ggsave(p, filename="../Figures/LM/cor_nb.png", width=20, height=5)

#Random Forest Model
library(randomForest)
vars_1_2<-paste(c("env_mean_sd", "env_delta", "increase_per"), collapse="+")
vars_5<-paste(c(vars_1_2, "nb_mean_delta_in_windows", "nb_mean_next_delta"), collapse="+")
vars_3_4<-paste(c(vars_1_2, "nb_next_delta_ratio"), collapse="+")
vars_6_7<-paste(c(vars_1_2, "nb_delta_ratio_in_windows",
            "nb_next_delta", "nb_next_delta_ratio",
            "nb_mean_delta_in_windows", 
            "nb_mean_next_delta"), collapse="+")

formulas<-data.table(species_evo_type=c(1:7),
                     formula=c(vars_1_2, vars_1_2, vars_3_4, vars_3_4, 
                               vars_5, vars_6_7, vars_6_7))
df_se_comb<-df_se[, .(N=.N), by=list(species_evo_type, directional_speed)]
i=1
rm_result<-list()
for (i in c(1:nrow(df_se_comb))){
  print(paste(i, nrow(df_se_comb)))
  com_item<-df_se_comb[i]
  item<-df_se[species_evo_type==com_item$species_evo_type &
                directional_speed==com_item$directional_speed]
  rf_model <- randomForest(as.formula(sprintf("net_dr~%s", formulas[species_evo_type==com_item$species_evo_type]$formula)),
                           data=item, ntree = 1000,importance=TRUE)
  predicted<-predict(rf_model, item)
  
  
  #plot(predicted, item$net_dr)
  # Get variable importance
  var_imp<-data.table(varName=row.names(importance(rf_model)))
  var_imp$IncMSE=rf_model$importance[,1]
  var_imp$IncNodePurity=rf_model$importance[,2]
  var_imp$cor<-cor(predicted, item$net_dr)
  var_imp$species_evo_type<-com_item$species_evo_type
  var_imp$directional_speed<-com_item$directional_speed
  rm_result[[length(rm_result)+1]]<-var_imp
}
rm_result<-rbindlist(rm_result)
rm_result$evo_type<-format_evoType(rm_result$species_evo_type)
rm_result[, label:=format_evoLabel(evo_type, directional_speed), 
           by=seq_len(nrow(rm_result))]
range(rm_result$cor)
ggplot(rm_result)+geom_point(aes(x=label, y=cor))
rm_result$group<-"NB"
rm_result[varName %in% c("lower_limit_next_delta", "upper_limit_next_delta", "nb_mean_next_delta")]$group<-"LIMIT"
rm_result[varName %in% c("var", "env_mean_sd", "env_delta", "increase_per")]$group<-"ENV"
write.csv(rm_result, "../Figures/LM/rf_result_4_var.csv", row.names=F)

p<-ggplot(rm_result)+geom_point(aes(y=varName, x=IncMSE, color=group))+
  facet_grid(species_evo_type~directional_speed, scale="free_y")
ggsave(p, filename="../Figures/LM/rf_result_4_var.png", width=10, height=10)

df_se_comb<-df_se[, .(N=.N), by=list(species_evo_type, directional_speed, var)]

i=19
result_all<-list()
for (i in c(1:nrow(df_se_comb))){
  item<-df_se[species_evo_type==df_se_comb[i]$species_evo_type &
                directional_speed==df_se_comb[i]$directional_speed &
                var==df_se_comb[i]$var]
  
  model<-glm(net_dr~nb_mean_delta+nb_delta+nb_next_delta_ratio+nb_next_delta+env_mean_sd+env_mean,
             data=item)
  model_null<-glm(net_dr~1,
             data=item)
  table(df_se$species_evo_type)
  #summary(model)
  #predict<-predict(model, item)
  #plot(predict, item$net_dr)
  coef<-data.table(variable=names(model$coefficients), coef_v=model$coefficients)
  imp<-importance(model, model_null)
  #plot(imp)
  imp<-data.table(imp$data)
  result<-merge(imp, coef, by="variable", all=T)
  result$var<-df_se_comb[i]$var
  result$species_evo_type<-df_se_comb[i]$species_evo_type
  result$directional_speed<-df_se_comb[i]$directional_speed
  
  result$r2<-with(summary(model), 1 - deviance/null.deviance)
  result$aic<-model$aic
  result_all[[length(result_all)+1]]<-result
  print(paste(df_se_comb[i]$species_evo_type, df_se_comb[i]$directional_speed, r2))
}

result_all<-rbindlist(result_all)
saveRDS(result_all, "../Figures/LM/lm_result.rda")
hist(result_all$r2)
result_imp<-result_all[posit=="Cum"]
result_imp$evo_type<-format_evoType(result_imp$species_evo_type)
result_imp[, label:=format_evoLabel(evo_type, directional_speed), 
           by=seq_len(nrow(result_imp))]
p<-ggplot(result_imp)+geom_bar(aes(y=variable, x=value), stat='identity')+
  facet_grid(var~label)
p
ggsave(p, filename="../Figures/LM/LM_full.png", width=12, height=6)

write.csv(result_all, "../Figures/LM/lm_result.csv", row.names = F)

if (F){
  if (F){
    d<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction.rda")
    d$outlier<-"F"
    outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
    d[global_id %in% unique(outliers$global_id)]$outlier<-"T"
    d_end<-d[year==0]
    d_end<-d_end[outlier=="F"]
    d_end$R_SPECIATION<-d_end$N_SPECIATION/d_end$N_SPECIES * 1e3
    d_end$R_EXTINCTION<-d_end$N_EXTINCTION/d_end$N_SPECIES * 1e3
    d_end<-d_end[((directional_speed %in% c(0) & species_evo_type==1) |
                    (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                    (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))) &
                   species_evo_level==0]
    range(d_end$N_SPECIES)
  }
}
