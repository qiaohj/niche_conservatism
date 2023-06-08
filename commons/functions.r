bind<-function(df1, df2){
  if (is.null(df1)){
    df2
  }else{
    rbindlist(list(df1, df2))
  }
}

#1: conservatism
#2: shift-directional
#3: expansion-directional
#4: expansion-omnidirectional
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
evo_types<-c("conservatism",
             "shift-directional",
             "expansion-directional",
             "expansion-omnidirectional",
             "random-central",
             "random-symmetrical",
             "random-asymmetrical",
             "unknown")
evo_type_amp<-data.frame(type=c("conservatism",
             "shift-directional", "shift-directional", "shift-directional",
             "expansion-directional", "expansion-directional", "expansion-directional",
             "expansion-omnidirectional", "expansion-omnidirectional", "expansion-omnidirectional",
             "random-central",
             "random-symmetrical",
             "random-asymmetrical"),
             amp=c(0.00, 0.01, 0.10, 0.50, 0.01, 0.10, 0.50, 0.01, 0.10, 0.50,
                   0.01, 0.01, 0.01))

format_evoType<-function(index){
  evo_type<-rep(evo_types[1], length(index))
  evo_type[index==2]<-evo_types[2]
  evo_type[index==3]<-evo_types[3]
  evo_type[index==4]<-evo_types[4]
  evo_type[index==5]<-evo_types[5]
  evo_type[index==6]<-evo_types[6]
  evo_type[index==7]<-evo_types[7]
  evo_type[index==0]<-evo_types[8]
  evo_type<-factor(evo_type, levels=evo_types)
}

format_evoLabel<-function(evoType, speed){
  if (evoType %in% c("conservatism", "random-central",
                     "random-symmetrical", "random-asymmetrical",
                     "unknown")){
    evoType
  }else{
    sprintf("%s (%s)", evoType, as.character(speed))
  }
}

format_evoType_amplitude<-function(evoType, amplitude, order=1){
  evo_type<-sprintf(sprintf("%s (%.2f)", evoType, amplitude))
  evo_type_amps<-sprintf(sprintf("%s (%.2f)", evo_type_amp$type, evo_type_amp$amp))
  if (order==-1){
    evo_type_amps<-rev(evo_type_amps)
  }
  evo_type<-factor(evo_type, levels=evo_type_amps)
}

safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
                             
colorBlindGrey8   <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

bg<-"#f6f6f6"


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


lm_fun<-function(null_model, df_item, y, y_short, x, species_evo_type, directional_speed, V_L){
  model<-glm(as.formula(sprintf("%s~%s", y, paste(x, collapse = "+"))),
             data=df_item)
  
  cor<-cor(model$fitted.values, df_item[, get(y)])
  if (is.null(null_model)){
    null_model<-model
  }
  anova<-anova(null_model, model, test = "Chisq")
  #Residual sum of squares:
  RSS <- c(crossprod(model$residuals))
  #Mean squared error:
  MSE <- RSS / length(model$residuals)
  #Root MSE:
  RMSE <- sqrt(MSE)
  #Pearson estimated residual variance
  
  sig2 <- RSS / model$df.residual
  #calculate McFadden's R-squared for model
  r2<-with(summary(model), 1 - deviance/null.deviance)
  AIC<-model$aic
  anova_p<-anova$`Pr(>Chi)`[2]
  anova_p_label<-""
  if (!is.na(anova_p)){
    if (anova_p<=0.05){
      anova_p_label<-"*"
    }
    if (anova_p<=0.01){
      anova_p_label<-"**"
    }
    if (anova_p<=0.001){
      anova_p_label<-"***"
    }
  }
  evaluation<-data.table(x=paste(x, collapse = "+"), y=y, y_short=y_short,
                         cor=cor, RSS=RSS, MSE=MSE, RMSE=RMSE,
                         sig2=sig2, R2=r2, AIC=AIC, 
                         anova_p=anova_p, anova_p_label=anova_p_label,
                         species_evo_type=species_evo_type, 
                         directional_speed=directional_speed, V_L=V_L)
  result<-list(null_model=null_model, model=model, anova=anova, evaluation=evaluation)
  result
}

  