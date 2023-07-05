library(Rmisc)
x_label<-"Years before present (kyr)"
change_rate<-"change rate"
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

evo_types_label_item<-c("Conservatism",
                        "Directional",
                        "Directional",
                        "Omnidirectional",
                        "Shift",
                        "Change",
                        "Change and shift",
                        "Birds",
                        "Mammals")

evo_types_label_x<-c("Conservatism & No change", 
                     "Directional 10% & Shift", 
                     "Directional 50% & Shift",
                     "Central & Random", 
                     "Symmetrical & Random", 
                     "Asymmetrical & Random",
                     "Directional 10% & Expansion", 
                     " Directional 50% & Expansion",
                     "Omnidirectional 10% & Expansion", 
                     "Omnidirectional 50% & Expansion")

evo_types_label_line<-c("Niche Conservatism",
                        "Directional niche shift (10%)",
                        "Directional niche shift (50%)",
                        "Random niche shift",
                        "Random niche expansion/reduction",
                        "Random niche change and shift",
                        "Directional niche expansion (10%)",
                        "Directional niche expansion (50%)",
                        "Omnidirectional niche expansion (10%)",
                        "Omnidirectional niche expansion (50%)"
                        )

evo_types_label_group<-c("No change",
                         "Niche shift",
                         "Niche expansion",
                         "Niche expansion",
                         "Random change",
                         "Random change",
                         "Random change",
                         "IUCN",
                         "IUCN")
evo_types_label_color<-c("Niche Conservatism",
                         "Directional niche shift",
                         "Directional niche expansion",
                         "Omnidirectional niche expansion",
                         "Random niche shift",
                         "Random niche expansion/reduction",
                         "Random niche change and shift",
                         "Birds",
                         "Mammals")

evo_type_color <- c('Niche Conservatism'='#000000', 
                'Directional niche shift (10%)'='#228833', 
                'Directional niche shift (50%)'='#228833', 
                'Random niche shift'='#AA3377', 
                'Random niche expansion/reduction'='#EE6677', 
                'Random niche change and shift'='#CCBB44',
                'Directional niche expansion (10%)'='#4477AA', 
                'Directional niche expansion (50%)'='#4477AA', 
                'Omnidirectional niche expansion (10%)'='#66CCEE',
                'Omnidirectional niche expansion (50%)'='#66CCEE')

evo_type_color2 <- c('Niche Conservatism'='#000000', 
                    'Directional niche shift'='#228833', 
                    'Random niche shift'='#AA3377', 
                    'Random niche expansion/reduction'='#EE6677', 
                    'Random niche change and shift'='#CCBB44',
                    'Directional niche expansion'='#4477AA', 
                    'Omnidirectional niche expansion'='#66CCEE',
                    'Birds'='#DC050C',
                    'Mammals'='#CAE0AB')


evo_type_line <- c('Niche Conservatism'=1, 
                    'Directional niche shift (10%)'=1, 
                    'Directional niche shift (50%)'=2, 
                    'Random niche shift'=1, 
                    'Random niche expansion/reduction'=1, 
                    'Random niche change and shift'=1,
                    'Directional niche expansion (10%)'=1, 
                    'Directional niche expansion (50%)'=2, 
                    'Omnidirectional niche expansion (10%)'=1,
                    'Omnidirectional niche expansion (50%)'=2)

#evo_type_color <- c('#444444', 
#                    '#228833', 
#                    '#AA3377', 
#                    '#EE6677', 
#                    '#CCBB44',
#                    '#4477AA',  
#                    '#66CCEE')


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"


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

create_fig<-function(item_y, label="", barwidth=10, with_label=T, 
                     legend_label="", polygon=NULL){
  if (is.null(polygon)){
    polygon<-readRDS("../Figures/Movie2.Example/polygon.rda")
  }
  threshold<-round(mean(item_y$N_SPECIES)+3*sd(item_y$N_SPECIES))
  #threshold<-round(quantile(item_y$N_SPECIES, 0.75)+1.5*IQR(item_y$N_SPECIES))
  threshold<-7000
  max_n_sp<-max(item_y$N_SPECIES)
  min_n_sp<-min(item_y$N_SPECIES)
  if (threshold>max_n_sp){
    midpoint<-round(max_n_sp/2)
    breakss<-c(min_n_sp, midpoint, max_n_sp)
    labelss<-c("0", "", as.character(max_n_sp))
  }else{
    midpoint<-round(threshold/2)
    breakss<-c(min_n_sp, midpoint, threshold)
    labelss<-c("0", "", sprintf(">%d, up to %d", threshold, max_n_sp))
  }
  
  
  item_y[N_SPECIES>threshold]$N_SPECIES<-threshold
  item_y<-merge(polygon, item_y, by.x="Name", by.y="global_id")
  
  p_asia<-ggplot(item_y, aes(colour=N_SPECIES)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf()+
    scale_color_gradient2(low  = "#4477AA", high="#EE6677",
                          mid = "#DDDDDD", midpoint=midpoint,
                          breaks=breakss, 
                          labels=labelss)+
    
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_asia))+
    labs(colour=legend_label)+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", 
                                          linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank(),
          legend.position = "bottom")+
    guides(color = guide_colourbar(barwidth = barwidth, barheight = NULL,
                                   title.position = "left", title.hjust = 1)) 
  #legend<-get_legend(p_asia)
  #p_asia<-p_asia+theme(legend.position = "none")
  
  p_america<-ggplot(item_y, aes(colour=N_SPECIES)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf()+
    scale_color_gradient2(low  = "#4477AA", high="#EE6677",
                          mid = "#DDDDDD", midpoint=midpoint,
                          breaks=breakss, 
                          labels=labelss)+
    
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_america))+
    labs(colour=legend_label)+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", 
                                          linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank(),
          legend.position = "bottom")+
    guides(color = guide_colourbar(barwidth = barwidth, barheight = NULL,
                                   title.position = "left", title.hjust = 1)) 
  
  p<-ggarrange(p_asia, p_america, common.legend = TRUE, legend="bottom")
  if (with_label){
    p<-annotate_figure(p, top = sprintf("%s", label))
  }
  return(p)
}
my_CI<-function(v, ci=0.95){
  cis<-Rmisc::CI(v, ci=ci)
  cis[2] - cis[3]
}
formatLabels<-function(d_item){
  d_item$evo_type<-format_evoType(d_item$species_evo_type)
  d_item$evo_types_label_item<-evo_types_label_item[d_item$species_evo_type]
  d_item$evo_types_label_color<-evo_types_label_color[d_item$species_evo_type]
  d_item$evo_types_label_group<-evo_types_label_group[d_item$species_evo_type]
  d_item$evo_type_color<-evo_type_color[d_item$species_evo_type]
  d_item$evo_speed<-""
  d_item[directional_speed %in% c(0.1, 0.5)]$evo_speed<-sprintf("%s%s",
    as.character(d_item[directional_speed %in% c(0.1, 0.5)]$directional_speed * 100), "%")
  d_item$evo_types_label_item_label<-d_item$evo_types_label_item
  d_item[directional_speed %in% c(0.1, 0.5)]$evo_types_label_item_label<-
    sprintf("%s (%s)", d_item[directional_speed %in% c(0.1, 0.5)]$evo_types_label_item, 
            d_item[directional_speed %in% c(0.1, 0.5)]$evo_speed)
  d_item$evo_line_type<-"no change, random or 50%"
  d_item[directional_speed %in% c(0.1)]$evo_line_type<-"10%"
  d_item[, label:=format_evoLabel(evo_type, directional_speed), 
             by=seq_len(nrow(d_item))]
  
  d_item<-formatLabelX(d_item)
  d_item<-formatLabel_line(d_item)
  d_item
}
formatLabelX<-function(d_item){
  d_item$label_x<-evo_types_label_x[1]
  d_item[label=="shift-directional (0.1)"]$label_x<-evo_types_label_x[2]
  d_item[label=="shift-directional (0.5)"]$label_x<-evo_types_label_x[3]
  d_item[label=="random-central"]$label_x<-evo_types_label_x[4]
  d_item[label=="random-symmetrical"]$label_x<-evo_types_label_x[5]
  d_item[label=="random-asymmetrical"]$label_x<-evo_types_label_x[6]
  d_item[label=="expansion-directional (0.1)"]$label_x<-evo_types_label_x[7]
  d_item[label=="expansion-directional (0.5)"]$label_x<-evo_types_label_x[8]
  d_item[label=="expansion-omnidirectional (0.1)"]$label_x<-evo_types_label_x[9]
  d_item[label=="expansion-omnidirectional (0.5)"]$label_x<-evo_types_label_x[10]
  d_item$label_x<-factor(d_item$label_x, levels=evo_types_label_x)
  
  d_item
}

formatLabel_line<-function(d_item){
  d_item$label_line<-evo_types_label_line[1]
  d_item[label=="shift-directional (0.1)"]$label_line<-evo_types_label_line[2]
  d_item[label=="shift-directional (0.5)"]$label_line<-evo_types_label_line[3]
  d_item[label=="random-central"]$label_line<-evo_types_label_line[4]
  d_item[label=="random-symmetrical"]$label_line<-evo_types_label_line[5]
  d_item[label=="random-asymmetrical"]$label_line<-evo_types_label_line[6]
  d_item[label=="expansion-directional (0.1)"]$label_line<-evo_types_label_line[7]
  d_item[label=="expansion-directional (0.5)"]$label_line<-evo_types_label_line[8]
  d_item[label=="expansion-omnidirectional (0.1)"]$label_line<-evo_types_label_line[9]
  d_item[label=="expansion-omnidirectional (0.5)"]$label_line<-evo_types_label_line[10]
  d_item$label_line<-factor(d_item$label_line, levels=evo_types_label_line)
  d_item
}

format_digits <- function(x, digits=3) {
  formatC(x, format = "f", digits = digits)
}
