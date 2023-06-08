library(data.table)
library(ggplot2)
library(randomForest)
library(corrplot)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
if (F){
  
  d_ratio_traits_ndr<-readRDS("../Data/tslm/d_tslm_full.rda")
  d_ratio_traits_ndr<-d_ratio_traits_ndr[!is.nan(species_increasing_rate)]
  d_ratio_traits_ndr$net_dr2<- d_ratio_traits_ndr$R_SPECIATION_SPECIES * 
    d_ratio_traits_ndr$SPECIES_PER_SPECIATION - 
    d_ratio_traits_ndr$R_EXTINCTION_SPECIES
  
  d_se<-d_ratio_traits_ndr[, .(nb_mean_delta_in_windows=mean(nb_mean_delta_in_windows, na.rm=T),
                               nb_mean_next_delta=mean(nb_mean_next_delta, na.rm=T),
                               nb_delta_ratio_in_windows=mean(nb_delta_ratio_in_windows, na.rm=T),
                               nb_next_delta_ratio=mean(nb_next_delta_ratio, na.rm=T),
                               net_dr=mean(net_dr, na.rm=T),
                               net_dr2=mean(net_dr2, na.rm=T),
                               R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
                               R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T),
                               SPECIES_PER_SPECIATION=mean(SPECIES_PER_SPECIATION, na.rm=T)),
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
  
  
  saveRDS(d_ratio_traits_ndr_with_env, "../Data/null_model/d_ratio_traits_ndr_with_env.rda")
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


net_dr_model<-glm(data=d_ratio_traits_ndr_with_env, 
                  net_dr~R_EXTINCTION_SPECIES+R_SPECIATION_SPECIES*SPECIES_PER_SPECIATION
)
cor(net_dr_model$fitted.values, d_ratio_traits_ndr_with_env$net_dr)
summary(net_dr_model)
vars_env<-c("env_mean_sd", "env_delta", "increase_per")
vars_niche_limit<-c("nb_mean_next_delta", "nb_mean_delta_in_windows")
vars_niche_breadth<-c("nb_next_delta_ratio", "nb_delta_ratio_in_windows")

d_ratio_traits_ndr_with_env<-readRDS("../Data/null_model/d_ratio_traits_ndr_with_env.rda")
#d_ratio_traits_ndr_with_env<-d_ratio_traits_ndr_with_env[from>=-1000]
d_null<-d_ratio_traits_ndr_with_env[species_evo_type==1]

colnames(d_null)[c(11, 13, 14, 15)]<-paste(colnames(d_null)[c(11, 13, 14, 15)], "null", sep="_")
d_null<-d_null[, c(1, 2, 3, 11, 13, 14, 15)]

vars<-unique(d_ratio_traits_ndr_with_env$V_L)
v<-vars[1]
y<-"net_dr_diff"

i=2
df_item<-model_data
x<-"1"
coms<-d_ratio_traits_ndr_with_env[, .(N=.N), by=list(species_evo_type, directional_speed, V_L)]

models<-list()
estimations<-list()
for (y in c("SPECIES_PER_SPECIATION_diff", "R_SPECIATION_SPECIES_diff", 
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
for (y in c("net_dr_diff", "SPECIES_PER_SPECIATION_diff", "R_SPECIATION_SPECIES_diff", 
            "R_EXTINCTION_SPECIES_diff")){
  for (i in c(2:nrow(coms))){
    d_null_v<-d_null
    model_data<-d_ratio_traits_ndr_with_env[species_evo_type==coms[i]$species_evo_type & 
                                              directional_speed==coms[i]$directional_speed]
    model_data<-merge(model_data, d_null_v, by=c("from", "to", "V_L"), all=T)
    model_data$net_dr_diff<-model_data$net_dr - model_data$net_dr_null
    model_data$SPECIES_PER_SPECIATION_diff<-model_data$SPECIES_PER_SPECIATION - model_data$SPECIES_PER_SPECIATION_null
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
    
    model_niche_limit<-lm_fun(null_model, model_data, y, "NL", vars_niche_limit, 
                              coms[i]$species_evo_type,
                              coms[i]$directional_speed,
                              "ALL")
    models[[length(models)+1]]<-model_niche_limit
    estimations[[length(estimations)+1]]<-model_niche_limit$evaluation
    model_niche_limit_1<-lm_fun(null_model, model_data, y, "NL_NEXT", vars_niche_limit[1], 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                "ALL")
    models[[length(models)+1]]<-model_niche_limit_1
    estimations[[length(estimations)+1]]<-model_niche_limit_1$evaluation
    model_niche_limit_2<-lm_fun(null_model, model_data, y, "NL_WIN", vars_niche_limit[2], 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                "ALL")
    models[[length(models)+1]]<-model_niche_limit_2
    estimations[[length(estimations)+1]]<-model_niche_limit_2$evaluation
    model_niche_breadth<-lm_fun(null_model, model_data, y, "NB", vars_niche_breadth, 
                                coms[i]$species_evo_type,
                                coms[i]$directional_speed,
                                "ALL")
    models[[length(models)+1]]<-model_niche_breadth
    estimations[[length(estimations)+1]]<-model_niche_breadth$evaluation
    model_niche_breadth_1<-lm_fun(null_model, model_data, y, "NB_NEXT", vars_niche_breadth[1], 
                                  coms[i]$species_evo_type,
                                  coms[i]$directional_speed,
                                  "ALL")
    models[[length(models)+1]]<-model_niche_breadth_1
    estimations[[length(estimations)+1]]<-model_niche_breadth_1$evaluation
    model_niche_breadth_2<-lm_fun(null_model, model_data, y, "NB_WIN", vars_niche_breadth[2], 
                                  coms[i]$species_evo_type,
                                  coms[i]$directional_speed,
                                  "ALL")
    models[[length(models)+1]]<-model_niche_breadth_2
    estimations[[length(estimations)+1]]<-model_niche_breadth_2$evaluation
    model_both<-lm_fun(null_model, model_data, y, "ALL", 
                       c(vars_niche_limit, vars_niche_breadth), 
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
ggdf$r2_text<-round(ggdf$R2, 2)
p<-ggplot(ggdf[anova_p_label!=""])+geom_tile(aes(x=y, y=label, fill=anova_p_label))+
  geom_text(aes(x=y, y=label, label=r2_text))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+
  facet_wrap(~y_short, nrow=1)
p
ggsave(p, filename="../Figures/null_model/null_model.png", width=12, height=4)
