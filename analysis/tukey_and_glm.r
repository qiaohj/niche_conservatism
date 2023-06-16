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
  
  
  d_final<-readRDS("../Data/tslm/d_N_Species_final.rda")
  
  d[N_SPECIES_BEGIN==0]
  
  d$net_dr<-(d$N_SPECIES_END - d$N_SPECIES_BEGIN)/d$N_SPECIES_BEGIN
  d$net_dr_2<-d$R_SPECIATION_SPECIES - d$R_EXTINCTION_SPECIES
  samp<-sample(nrow(d), 5e3)
  plot(d[samp]$net_dr, d[samp]$R_SPECIATION_SPECIES - d[samp]$R_EXTINCTION_SPECIES)
  cor(d$net_dr, d$R_SPECIATION_SPECIES - d$R_EXTINCTION_SPECIES)
  
  saveRDS(d, "../Data/tslm_and_glm/d_ndr.rda")
  
  d_null<-d[species_evo_type==1]
  d_null<-d_null[, c("from", "to", "nb", "da", "global_id")]
  d_null$tag<-1
  d_with_null<-merge(d, d_null, 
                     by=c("from", "to", "nb", "da", "global_id"))
  ggplot(d)+geom_histogram(aes(y=net_dr))+facet_wrap(~label)
  cor(d$N_SPECIES, d$net_dr)
  cor(d$N_SPECIES, d$N_SPECIATION)
  item<-d[from>=-900]
  df_N_SPECIES<-TukeyHSD_B("N_SPECIES", item)
  df_R_SPECIATION_SPECIES<-TukeyHSD_B("R_SPECIATION_SPECIES", item)
  df_R_EXTINCTION_SPECIES<-TukeyHSD_B("R_EXTINCTION_SPECIES", item)
  df_net_dr<-TukeyHSD_B("net_dr", item)
  df_net_dr_2<-TukeyHSD_B("net_dr_2", item)
  df_result<-rbindlist(list(df_net_dr, df_N_SPECIES,
                            df_R_SPECIATION_SPECIES, 
                            df_R_EXTINCTION_SPECIES))
  df_result$diff_str<-round(df_result$diff, 3)
  saveRDS(df_result, "../Figures/TukeyHSD/TukeyHSD_by_species.rda")
  write.csv(df_result, "../Figures/TukeyHSD/TukeyHSD_by_species.csv", row.names = F)
}
if (F){
  df_result<-readRDS("../Figures/TukeyHSD/TukeyHSD_by_species.rda")
  
  df_result$diff_str<-round(df_result$diff, 3)
  df_result$alternative<-""
  df_result[diff>0]$alternative<-"greater"
  df_result[diff<0]$alternative<-"less"
  df_result[p_label==""]$alternative<-"no sig dif"
  table(df_result$alternative)
  df_result$label<-gsub("-conservatism", "", df_result$label)
  df_result$diff_label<-sprintf("%.3f %s", df_result$diff_str, df_result$p_label)
  p<-ggplot(df_result[!grepl("0.01", label)])+
    geom_tile(aes(x=type, y=label, fill=alternative))+
    geom_text(aes(x=type, y=label, label=diff_label))+
    scale_fill_manual(values=c("#D55E00", "#0072B2", "white"),
                      breaks=c("greater", "less", "no sig dif"))
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
  saveRDS(all_r_df, "../Data/tslm_and_glm/d_nb_traits_by_global_id.rda")
}
if (F){
  all_r_df<-readRDS("../Data/tslm_and_glm/d_nb_traits_by_global_id.rda")
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
  d_ndr<-readRDS("../Data/tslm_and_glm/d_ndr.rda")
  
 
  d_ratio_traits_ndr<-merge(all_r_df, d_ndr, by=c("from", "to", "global_id", "nb", "da", 
                                                  "species_evo_type", "directional_speed"))
  saveRDS(d_ratio_traits_ndr, "../Data/tslm_and_glm/d_tslm_full.rda")
  
}

if (F){
  d_ratio_traits_ndr<-readRDS("../Data/tslm_and_glm/d_tslm_full.rda")
  
  d_se<-d_ratio_traits_ndr[, .(nb_mean_delta_in_windows=mean(nb_mean_delta_in_windows, na.rm=T),
                               nb_mean_next_delta=mean(nb_mean_next_delta, na.rm=T),
                               nb_delta_ratio_in_windows=mean(nb_delta_ratio_in_windows, na.rm=T),
                               nb_next_delta_ratio=mean(nb_next_delta_ratio, na.rm=T),
                               net_dr=mean(net_dr, na.rm=T),
                               R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
                               R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T)),
                           by=c("species_evo_type", "directional_speed", "from","to", "V_L", "label")]
  
  env_df<-readRDS("../Figures/LM/traits_ndr.rda")
  env_cols<-c("var", "env_mean", "env_mean_sd", "env_mean_delta_in_windows", "env_delta", 
              "env_delta_sd", "increase_per", "from_y",  "to_y")
  env_df<-env_df[, ..env_cols]
  env_df<-unique(env_df)
  d_ratio_traits_ndr_with_env<-merge(d_se, env_df, by.x=c("from", "to", "V_L"),
                                     by.y=c("from_y", "to_y", "var"))
  d_ratio_traits_ndr_with_env[species_evo_type %in% c(2, 5)]$nb_delta_ratio_in_windows<-0
  d_ratio_traits_ndr_with_env[species_evo_type %in% c(2, 5)]$nb_next_delta_ratio<-1
  d_ratio_traits_ndr_with_env[species_evo_type %in% c(4, 6)]$nb_mean_delta_in_windows<-0
  d_ratio_traits_ndr_with_env[species_evo_type %in% c(4, 6)]$nb_mean_next_delta<-0
  
  
  saveRDS(d_ratio_traits_ndr_with_env, "../Data/tslm_and_glm/d_ratio_traits_ndr_with_env.rda")
  vars_1<-c("env_mean_sd", "env_delta", "increase_per")
  vars_2_5<-c(vars_1, "nb_mean_delta_in_windows", "nb_mean_next_delta")
  vars_3_4_7<-c(vars_2_5, "nb_next_delta_ratio", "nb_delta_ratio_in_windows")
  vars_6<-c(vars_1, "nb_delta_ratio_in_windows", "nb_next_delta_ratio")
  vars_list<-list(vars_1, vars_2_5, vars_3_4_7, vars_3_4_7, vars_2_5, vars_6, vars_3_4_7)
  i=6
  
  for (i in c(1:7)){
    items<-as.data.frame(d_ratio_traits_ndr_with_env[species_evo_type==i])[, vars_list[[i]]]
    #items$V_L<-NULL
    corrplot(cor(items), title=i, method="number")
  }
  ggplot(d_ratio_traits_ndr_with_env)+geom_line(aes(x=from, y=net_dr, color=label))
}





vars_env<-c("env_mean_sd", "env_delta", "increase_per")
vars_niche_limit<-c("nb_mean_next_delta", "nb_mean_delta_in_windows")
vars_niche_breadth<-c("nb_next_delta_ratio", "nb_delta_ratio_in_windows")

d_ratio_traits_ndr_with_env<-readRDS("../Data/null_model/d_ratio_traits_ndr_with_env.rda")

net_dr_model<-glm(data=d_ratio_traits_ndr_with_env, 
                  net_dr * 1000 ~R_EXTINCTION_SPECIES+R_SPECIATION_SPECIES
)
#plot(net_dr_model)
cor(net_dr_model$fitted.values, d_ratio_traits_ndr_with_env$net_dr)
summary(net_dr_model)


#d_ratio_traits_ndr_with_env<-d_ratio_traits_ndr_with_env[from>=-1000]
d_null<-d_ratio_traits_ndr_with_env[species_evo_type==1]

colnames(d_null)[c(11, 12, 13)]<-paste(colnames(d_null)[c(11, 12, 13)], "null", sep="_")
d_null<-d_null[, c(1, 2, 3, 11, 12, 13)]

vars<-unique(d_ratio_traits_ndr_with_env$V_L)
v<-vars[1]
y<-"net_dr_diff"

i=2
df_item<-model_data
x<-"1"
coms<-d_ratio_traits_ndr_with_env[, .(N=.N), by=list(species_evo_type, directional_speed, V_L)]

models<-list()
estimations<-list()
for (y in c("net_dr_diff", "R_SPECIATION_SPECIES_diff", 
            "R_EXTINCTION_SPECIES_diff")){
  for (i in c(2:nrow(coms))){
    d_null_v<-d_null[V_L==coms[i]$V_L]
    model_data<-d_ratio_traits_ndr_with_env[species_evo_type==coms[i]$species_evo_type & 
                                              V_L==coms[i]$V_L & 
                                              directional_speed==coms[i]$directional_speed]
    model_data<-merge(model_data, d_null_v, by=c("from", "to", "V_L"), all=T)
    model_data$net_dr_diff<-model_data$net_dr - model_data$net_dr_null
    model_data$SPECIES_PER_SPECIATION_diff<-model_data$SPECIES_PER_SPECIATION - model_data$SPECIES_PER_SPECIATION_null
    model_data$R_EXTINCTION_SPECIES_diff<-model_data$R_EXTINCTION_SPECIES - model_data$R_EXTINCTION_SPECIES_null
    model_data$R_SPECIATION_SPECIES_diff<-model_data$R_SPECIATION_SPECIES - model_data$R_SPECIATION_SPECIES_null
    if (F){
      hist(model_data$net_dr_diff)
      hist(model_data$SPECIES_PER_SPECIATION_diff)
      ggplot(model_data)+geom_line(aes(x=from, y=net_dr))+
        geom_line(aes(x=from, y=net_dr_null), col="red")
      ggplot(model_data)+geom_line(aes(x=from, y=net_dr_diff))
    }
    #the result shows a Df of 1 (indicating that the more complex model has one additional parameter), 
    #and a very small p-value (< .001). This means that adding the clarity IV to the model did lead 
    #to a significantly improved fit over the model 1.
    
    null_model<-lm_fun(NULL, model_data, y, "1", "NULL",
                       coms[i]$species_evo_type,
                       coms[i]$directional_speed,
                       coms[i]$V_L)
    null_model<-null_model$model
    
    summary(net_dr_model)
    model_niche_limit<-lm_fun(null_model, model_data, y, "NL", 
                              vars_niche_limit, 
                              coms[i]$species_evo_type,
                              coms[i]$directional_speed,
                              coms[i]$V_L)
    models[[length(models)+1]]<-model_niche_limit
    estimations[[length(estimations)+1]]<-model_niche_limit$evaluation
    model_niche_limit_1<-lm_fun(null_model, model_data, y, "NL_NEXT", vars_niche_limit[1], 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                coms[i]$V_L)
    models[[length(models)+1]]<-model_niche_limit_1
    estimations[[length(estimations)+1]]<-model_niche_limit_1$evaluation
    model_niche_limit_2<-lm_fun(null_model, model_data, y, "NL_WIN", vars_niche_limit[2], 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                coms[i]$V_L)
    models[[length(models)+1]]<-model_niche_limit_2
    estimations[[length(estimations)+1]]<-model_niche_limit_2$evaluation
    model_niche_breadth<-lm_fun(null_model, model_data, y, "NB", vars_niche_breadth, 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                coms[i]$V_L)
    models[[length(models)+1]]<-model_niche_breadth
    estimations[[length(estimations)+1]]<-model_niche_breadth$evaluation
    model_niche_breadth_1<-lm_fun(null_model, model_data, y, "NB_NEXT", vars_niche_breadth[1], 
                                  coms[i]$species_evo_type,
                                  coms[i]$directional_speed,
                                  coms[i]$V_L)
    models[[length(models)+1]]<-model_niche_breadth_1
    estimations[[length(estimations)+1]]<-model_niche_breadth_1$evaluation
    model_niche_breadth_2<-lm_fun(null_model, model_data, y, "NB_WIN", vars_niche_breadth[2], 
                                  coms[i]$species_evo_type,
                                  coms[i]$directional_speed,
                                  coms[i]$V_L)
    models[[length(models)+1]]<-model_niche_breadth_2
    estimations[[length(estimations)+1]]<-model_niche_breadth_2$evaluation
    model_both<-lm_fun(null_model, model_data, y, "ALL", 
                       c(vars_niche_limit, vars_niche_breadth), 
                       coms[i]$species_evo_type,
                       coms[i]$directional_speed,
                       coms[i]$V_L)
    models[[length(models)+1]]<-model_both
    estimations[[length(estimations)+1]]<-model_both$evaluation
    
  }
}
estimations<-rbindlist(estimations)
estimations$evo_type<-format_evoType(estimations$species_evo_type)
estimations[, label:=format_evoLabel(evo_type, directional_speed), 
            by=seq_len(nrow(estimations))]
ggdf<-estimations[species_evo_type>1 & y!="net_dr2_diff" & V_L!="Debiased_Maximum_Monthly_Temperature"]
ggdf$var<-"Precipitation"
ggdf[V_L=="Debiased_Minimum_Monthly_Temperature"]$var<-"Temperature"
ggdf$r2_text<-round(ggdf$R2, 2)
p<-ggplot(ggdf[anova_p_label!=""])+geom_tile(aes(x=y, y=label, fill=anova_p_label))+
  geom_text(aes(x=y, y=label, label=r2_text))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  facet_grid(var~y_short)
p
ggsave(p, filename="../Figures/null_model/null_model_by_var.png", width=12, height=8)


#For all variables
coms<-d_ratio_traits_ndr_with_env[, .(N=.N), by=list(species_evo_type, directional_speed)]

models<-list()
estimations<-list()
for (y in c("net_dr_diff", "R_SPECIATION_SPECIES_diff", 
            "R_EXTINCTION_SPECIES_diff",
            "net_dr", "R_SPECIATION_SPECIES", 
            "R_EXTINCTION_SPECIES")){
  for (i in c(2:nrow(coms))){
    d_null_v<-d_null
    model_data<-d_ratio_traits_ndr_with_env[species_evo_type==coms[i]$species_evo_type & 
                                              directional_speed==coms[i]$directional_speed]
    model_data<-merge(model_data, d_null_v, by=c("from", "to", "V_L"), all=T)
    model_data$net_dr_diff<-model_data$net_dr - model_data$net_dr_null
    model_data$R_EXTINCTION_SPECIES_diff<-model_data$R_EXTINCTION_SPECIES - model_data$R_EXTINCTION_SPECIES_null
    model_data$R_SPECIATION_SPECIES_diff<-model_data$R_SPECIATION_SPECIES - model_data$R_SPECIATION_SPECIES_null
    if (F){
      hist(model_data$net_dr_diff)
      hist(model_data$net_dr2_diff)
      ggplot(model_data)+geom_line(aes(x=from, y=net_dr))+
        geom_line(aes(x=from, y=net_dr_null), col="red")
      ggplot(model_data)+geom_line(aes(x=from, y=net_dr_diff))
    }
    #the result shows a Df of 1 (indicating that the more complex model has one additional parameter), 
    #and a very small p-value (< .001). This means that adding the clarity IV to the model did lead 
    #to a significantly improved fit over the model 1.
    
    null_model<-lm_fun(NULL, model_data, y, "1", "NULL",
                       coms[i]$species_evo_type,
                       coms[i]$directional_speed,
                       "ALL")
    null_model<-null_model$model
    
    model_niche_limit<-lm_fun(null_model, model_data, y, "NL", c(vars_niche_limit, "V_L"), 
                              coms[i]$species_evo_type,
                              coms[i]$directional_speed,
                              "ALL")
    models[[length(models)+1]]<-model_niche_limit
    estimations[[length(estimations)+1]]<-model_niche_limit$evaluation
    model_niche_limit_1<-lm_fun(null_model, model_data, y, "NL_NEXT", c(vars_niche_limit[1], "V_L"), 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                "ALL")
    models[[length(models)+1]]<-model_niche_limit_1
    estimations[[length(estimations)+1]]<-model_niche_limit_1$evaluation
    model_niche_limit_2<-lm_fun(null_model, model_data, y, "NL_WIN", c(vars_niche_limit[2], "V_L"), 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                "ALL")
    models[[length(models)+1]]<-model_niche_limit_2
    estimations[[length(estimations)+1]]<-model_niche_limit_2$evaluation
    model_niche_breadth<-lm_fun(null_model, model_data, y, "NB", c(vars_niche_breadth, "V_L"), 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                "ALL")
    models[[length(models)+1]]<-model_niche_breadth
    estimations[[length(estimations)+1]]<-model_niche_breadth$evaluation
    model_niche_breadth_1<-lm_fun(null_model, model_data, y, "NB_NEXT", c(vars_niche_breadth[1], "V_L"), 
                                  coms[i]$species_evo_type,
                                  coms[i]$directional_speed,
                                  "ALL")
    models[[length(models)+1]]<-model_niche_breadth_1
    estimations[[length(estimations)+1]]<-model_niche_breadth_1$evaluation
    model_niche_breadth_2<-lm_fun(null_model, model_data, y, "NB_WIN", c(vars_niche_breadth[2], "V_L"), 
                                  coms[i]$species_evo_type,
                                  coms[i]$directional_speed,
                                  "ALL")
    models[[length(models)+1]]<-model_niche_breadth_2
    estimations[[length(estimations)+1]]<-model_niche_breadth_2$evaluation
    model_both<-lm_fun(null_model, model_data, y, "ALL", 
                       c(vars_niche_limit, vars_niche_breadth, "V_L"), 
                       coms[i]$species_evo_type,
                       coms[i]$directional_speed,
                       "ALL")
    models[[length(models)+1]]<-model_both
    estimations[[length(estimations)+1]]<-model_both$evaluation
    
  }
}
estimations<-rbindlist(estimations)
estimations$evo_type<-format_evoType(estimations$species_evo_type)
estimations[, label:=format_evoLabel(evo_type, directional_speed), 
            by=seq_len(nrow(estimations))]
ggdf<-estimations[species_evo_type>1 & y!="net_dr2_diff"]
ggdf$r2_text<-round(ggdf$cor, 2)
p<-ggplot(ggdf)+geom_tile(aes(x=y, y=label, fill=anova_p_label))+
  geom_text(aes(x=y, y=label, label=r2_text))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  facet_wrap(~y_short, nrow=1)
p
ggsave(p, filename="../Figures/null_model/null_model.png", width=30, height=6)

ggdf<-estimations[species_evo_type>1 & 
                    y %in% c("R_SPECIATION_SPECIES", "R_EXTINCTION_SPECIES")]
ggdf$r2_text<-round(ggdf$cor, 2)
p<-ggplot(ggdf)+geom_tile(aes(x=y, y=label, fill=anova_p_label))+
  geom_text(aes(x=y, y=label, label=r2_text))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  facet_wrap(~y_short, nrow=1)+
  scale_fill_manual(values=c("white", "green", "yellow", "red"),
                    breaks = c("", "*", "**", "***"))
p
ggsave(p, filename="../Figures/null_model/null_model_ratio.png", width=12, height=6)

ggdf<-estimations[species_evo_type>1 & 
                    y %in% c("R_SPECIATION_SPECIES", "R_EXTINCTION_SPECIES") &
                    y_short %in% c("ALL", "NB_NEXT", "NB_WIN", "NL_NEXT", "NL_WIN")]
ggdf$r2_text<-round(ggdf$cor, 2)
p<-ggplot(ggdf)+geom_tile(aes(x=y, y=label, fill=anova_p_label))+
  geom_text(aes(x=y, y=label, label=r2_text))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  facet_wrap(~y_short, nrow=1)+
  scale_fill_manual(values=c("white", "#56B4E9", "#009E73", "#D55E00"),
                    breaks = c("", "*", "**", "***"))
p
ggsave(p, filename="../Figures/null_model/null_model_ratio_short.png", width=10, height=6)

