library(data.table)
library(ggplot2)
library(agricolae)
library(dplyr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

t.test_my<-function(var, items, altern, com){
  
  var_null<-sprintf("%s_NULL", var)
  t.test<-t.test(pull(items[, ..var_null]), 
                 pull(items[, ..var]), 
                 paired=T, alternative=altern)
  p_value<-t.test$p.value
  
  
  com$p_value<-p_value
  com$var<-var
  com$alternative<-altern
  com$p_label<-""
  com[p_value<0.05]$p_label<-"*"
  com[p_value<0.01]$p_label<-"**"
  com[p_value<0.001]$p_label<-"***"
  com
}
TukeyHSD_B<-function(var, df){
  plant.lm <- lm(as.formula(sprintf("%s ~ label", var)), data = df)
  plant.av <- aov(plant.lm)
  summary(plant.av)
  tukey.test <- TukeyHSD(plant.av)
  #tukey.test
  #plot(tukey.test)
  tukey.test.df<-data.table(tukey.test$label)
  tukey.test.df$label<-row.names(tukey.test$label)
  tukey.test.df<-tukey.test.df[grepl("conservatism", label)]
  
  
  tukey.test.df$p_label<-""
  colnames(tukey.test.df)[4]<-"p_adj"
  tukey.test.df[p_adj<0.05]$p_label<-"*"
  tukey.test.df[p_adj<0.01]$p_label<-"**"
  tukey.test.df[p_adj<0.001]$p_label<-"***"
  tukey.test.df$type<-var
  tukey.test.df
}

lm_model<-function(var, items, com){
  model<-lm(data=items, 
            as.formula(
              sprintf("%s~nb_mean_delta_in_windows+nb_mean_next_delta+nb_delta_ratio_in_windows+nb_next_delta_ratio",
                      var)))
  model_null<-lm(as.formula(
    sprintf("%s~1",
            var)),
                 data=items)
  x<-summary(model)
  coef<-data.table(x$coefficients)
  colnames(coef)<-c("Estimate", "Std_Error", "t_value","pr_t")
  coef$variable<-row.names(x$coefficients)
  imp<-tornado::importance(model, model_null)
  predicted<-model$fitted.values
  cor<-cor(predicted, items$delta_net_dr)
  #plot(imp)
  imp<-data.table(imp$data)
  result<-merge(imp, coef, by="variable", all=T)
  result$directional_speed<-com$directional_speed
  result$species_evo_type<-com$species_evo_type
  result$env_var<-com$env_var
  result$cor<-cor
  result$r.squared<-x$r.squared
  result$adj.r.squared<-x$adj.r.squared
  result$fstatistic<-x$fstatistic[1]
  result$aic<-AIC(model)
  f <- x$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  result$p<-p
  result$var<-var
  
  result$p_label<-""
  result[p<0.05]$p_label<-"*"
  result[p<0.01]$p_label<-"**"
  result[p<0.001]$p_label<-"***"
  
  result$pr_t_label<-""
  result[pr_t<0.05]$pr_t_label<-"*"
  result[pr_t<0.01]$pr_t_label<-"**"
  result[pr_t<0.001]$pr_t_label<-"***"
  result
}
df<-readRDS("../Figures/LM/traits_ndr.rda")

df_net_dr<-TukeyHSD_B("net_dr", df)
df_N_SPECIES<-TukeyHSD_B("N_SPECIES", df)
df_R_SPECIATION_SPECIES<-TukeyHSD_B("R_SPECIATION_SPECIES", df)
df_R_EXTINCTION_SPECIES<-TukeyHSD_B("R_EXTINCTION_SPECIES", df)

df_result<-rbindlist(list(df_net_dr, df_N_SPECIES, df_R_SPECIATION_SPECIES, df_R_EXTINCTION_SPECIES))
saveRDS(df_result, "../Figures/TukeyHSD/TukeyHSD.rda")
write.csv(df_result, "../Figures/TukeyHSD/TukeyHSD.csv", row.names = F)

t.test(df[label==l]$net_dr, df[label=="conservatism"]$net_dr, alternative="less")

df_nc<-df[species_evo_type==1]
cols<-c("var", "net_dr", "N_SPECIES", "R_SPECIATION_SPECIES", "R_EXTINCTION_SPECIES", "from_y", "to_y")
df_nc<-df_nc[, ..cols]
colnames(df_nc)[c(2:5)]<-c("net_dr_NULL", "N_SPECIES_NULL", 
                           "R_SPECIATION_SPECIES_NULL", "R_EXTINCTION_SPECIES_NULL")
df_others<-df[species_evo_type!=1]
df_null_comp<-merge(df_nc, df_others, by=c("var", "from_y", "to_y"))
df_null_comp$delta_net_dr<-df_null_comp$net_dr - df_null_comp$net_dr_NULL
df_null_comp$delta_R_SPECIATION_SPECIES<-df_null_comp$R_SPECIATION_SPECIES - df_null_comp$R_SPECIATION_SPECIES_NULL
df_null_comp$delta_N_SPECIES<-df_null_comp$N_SPECIES - df_null_comp$N_SPECIES_NULL
df_null_comp$delta_R_EXTINCTION_SPECIES<-df_null_comp$R_EXTINCTION_SPECIES - df_null_comp$R_EXTINCTION_SPECIES_NULL

combs<-unique(df_null_comp[, c("species_evo_type", "directional_speed", "var")])
i=7
colnames(combs)[3]<-"env_var"
result_list<-list()
t.test_result_list<-list()
for (i in c(1:nrow(combs))){
  l<-combs[i]
  items<-df_null_comp[species_evo_type==l$species_evo_type & 
                        directional_speed==l$directional_speed & 
                        var==l$env_var]
  result1<-lm_model("delta_net_dr", items, l)
  result2<-lm_model("delta_N_SPECIES", items, l)
  result3<-lm_model("delta_R_SPECIATION_SPECIES", items, l)
  result4<-lm_model("delta_R_EXTINCTION_SPECIES", items, l)
  result<-rbindlist(list(result1, result2, result3, result4))

  result_list[[length(result_list)+1]]<-result
  
  for (v in c("net_dr", "N_SPECIES", "R_SPECIATION_SPECIES", "R_EXTINCTION_SPECIES")){
    t.test_result<-t.test_my(v, items, "greater", l)
    
    
    if (t.test_result$p_value>0.05){
      t.test_result2<-t.test_my(v, items, "less", l)
      t.test_result<-rbind(t.test_result, t.test_result2)
    }
    t.test_result_list[[length(t.test_result_list)+1]]<-t.test_result
  }
  
}

result_list<-rbindlist(result_list)
t.test_result_list<-rbindlist(t.test_result_list)

saveRDS(result_list, "../Figures/TukeyHSD/lm_models_importance.rda")
write.csv(result_list, "../Figures/TukeyHSD/lm_models_importance.csv", row.names = F)

result_list$posit<-NULL
result_list$value<-NULL

result_list_df<-unique(result_list)
saveRDS(result_list_df, "../Figures/TukeyHSD/lm_models.rda")
write.csv(result_list_df, "../Figures/TukeyHSD/lm_models.csv", row.names = F)

t.test_result_list[species_evo_type==2 & directional_speed==0.1 & var=="N_SPECIES"]
t.test_result_list[species_evo_type==2 & directional_speed==0.1 & var=="R_SPECIATION_SPECIES"]
t.test_result_list[species_evo_type==2 & directional_speed==0.1 & var=="R_EXTINCTION_SPECIES"]
t.test_result_1<-t.test_result_list[env_var=="Debiased_Maximum_Monthly_Precipitation"]
t.test_result_1$env_var<-NULL
t.test_result_1$label<-paste(t.test_result_1$species_evo_type, t.test_result_1$directional_speed)
t.test_result_1$p_str<-round(t.test_result_1$p_value*100)/100
ggplot(t.test_result_1[p_label!=""])+geom_tile(aes(x=var, y=label, fill=alternative))+
  geom_text(aes(x=var, y=label, label=alternative))

df_result$alternative<-""
df_result[diff>0]$alternative<-"less"
df_result[diff<0]$alternative<-"greater"
table(df_result$alternative)

p<-ggplot(df_result[p_label!=""])+geom_tile(aes(x=type, y=label, fill=alternative))+
  geom_text(aes(x=type, y=label, label=alternative))
ggsave(p, filename="../Figures/TukeyHSD/TukeyHSD.png", width=10, height=6)

lm_p<-unique(result_list_df[, c("directional_speed", "species_evo_type", 
                                  "env_var", "cor", "var", "p_label")])
ggplot(lm_p)+geom_point(aes(x=cor, y=var, color=p_label))+
  facet_wrap(~env_var)

plot(result_list_df$cor, result_list_df$r.squared)
