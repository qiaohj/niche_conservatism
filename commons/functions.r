library(Rmisc)
library(heatmaply)
library(rnaturalearth)
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
                'Random niche shift'='#66CCEE', 
                'Random niche expansion/reduction'='#EE6677', 
                'Random niche change and shift'='#CCBB44',
                'Directional niche expansion (10%)'='#4477AA', 
                'Directional niche expansion (50%)'='#4477AA', 
                'Omnidirectional niche expansion (10%)'='#AA3377',
                'Omnidirectional niche expansion (50%)'='#AA3377')

evo_type_color2 <- c('Niche Conservatism'='#000000', 
                    'Directional niche shift'='#228833', 
                    'Random niche shift'='#66CCEE', 
                    'Random niche expansion/reduction'='#EE6677', 
                    'Random niche change and shift'='#CCBB44',
                    'Directional niche expansion'='#4477AA', 
                    'Omnidirectional niche expansion'='#AA3377',
                    'Birds'='#CC3311',
                    'Mammals'='#BBBBBB')


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
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
mean_CI <- function(x) {
  ci<-my_CI(x)
  mean<-mean(x, na.rm=T)
  quantile <- quantile(x, probs=c(0.05, 0.95))
  r<-c(quantile[1], mean-ci, mean, mean+ci, quantile[2])
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

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


t.test_my2<-function(item1, item2, altern, com){
  
  t.test<-t.test(item1, 
                 item2, 
                 paired=T, 
                 alternative=altern)
  p_value<-t.test$p.value
  
  
  com$p_value<-p_value
  com$alternative<-altern
  com$p_label<-""
  com[p_value<0.05]$p_label<-"*"
  com[p_value<0.01]$p_label<-"**"
  com[p_value<0.001]$p_label<-"***"
  com
}
TukeyHSD_B<-function(var, df, tested_label="conservatism"){
  plant.lm <- lm(as.formula(sprintf("%s ~ label", var)), data = df)
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
  tukey.test.df[p_adj<0.05]$p_label<-"*"
  tukey.test.df[p_adj<0.01]$p_label<-"**"
  tukey.test.df[p_adj<0.001]$p_label<-"***"
  tukey.test.df$type<-var
  tukey.test.df$N<-nrow(df)
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
                     legend_label="", polygon=NULL, world=NULL,
                     is_uniform=F){
  if (is.null(polygon)){
    polygon<-readRDS("../Figures/Movie2.Example/polygon.rda")
  }
  if (is.null(world)){
    world <- ne_countries(scale = "small", returnclass = "sf")
    
  }
  threshold<-7000
  item_y[N_SPECIES>threshold]$N_SPECIES<-threshold
  item_y<-merge(polygon, item_y, by.x="Name", by.y="global_id")
  #threshold<-round(mean(item_y$N_SPECIES)+3*sd(item_y$N_SPECIES))
  #threshold<-round(quantile(item_y$N_SPECIES, 0.75)+1.5*IQR(item_y$N_SPECIES))
  
  
  if (is_uniform==F){
    if ("max_species" %in% colnames(item_y)){
      max_n_sp<-max(item_y$max_species)
      min_n_sp<-min(item_y$N_SPECIES)
      #print(item_y)
      geom.sf<-geom_sf(data=item_y[which(item_y$N_SPECIES>0),][1,], aes(colour=max_species))
      
    }else{
      max_n_sp<-max(item_y$N_SPECIES)
      min_n_sp<-min(item_y$N_SPECIES)
      geom.sf<-NULL
    }
    if (threshold>max_n_sp){
      midpoint<-round(max_n_sp/2)
      breakss<-c(min_n_sp, midpoint, max_n_sp)
      labelss<-c("0", "", as.character(max_n_sp))
    }else{
      midpoint<-round(threshold/2)
      breakss<-c(min_n_sp, midpoint, threshold)
      labelss<-c("0", "", sprintf(">%d, up to %d", threshold, max_n_sp))
    }
    gradient_colors<-scale_color_gradient2(low  = "#4477AA", high="#EE6677",
                          mid = "#DDDDDD", midpoint=midpoint,
                          breaks=breakss, 
                          labels=labelss)
  }else{
    gradient_colors<-scale_color_gradient2(low  = "#4477AA", high="#4477AA",
                                           mid = "#4477AA")
  }
  
  
  
  
  
  p_asia<-ggplot(item_y, aes(colour=N_SPECIES)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3")
  
  if (!is.null(geom.sf)){
    p_asia<-p_asia+geom.sf
  }
  p_asia<-  p_asia+geom_sf()+
    gradient_colors+
    
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
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") 
  if (!is.null(geom.sf)){
    p_america<-p_america+geom.sf
  }
  p_america<-  p_america+geom_sf()+
    gradient_colors+
    
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
create_fig_map<-function(item_y, label="", barwidth=10, with_label=T, 
                         legend_label="", polygon=NULL, world=NULL,
                         is_uniform=F, with_lat=T){
  if (is.null(polygon)){
    polygon<-readRDS("../Figures/Movie2.Example/polygon.rda")
  }
  if (is.null(world)){
    world <- ne_countries(scale = "small", returnclass = "sf")
    
  }
  
  coord<-data.table(st_coordinates(polygon))
  
  coord_se<-coord[, .(max_x=max(X),
                      min_x=min(X)),
                  by=list(L2)]
  coord_se$range<-coord_se$max_x-coord_se$min_x
  
  coord_se$is_valid<-ifelse(coord_se$range>50, "F", "T")
  polygon<-merge(polygon, coord_se, by.x="Name", by.y="L2")
  polygon<-polygon[which(polygon$range<50),]
  polygon$Name<-as.numeric(polygon$Name)
  threshold<-7000
  item_y[N_SPECIES>threshold]$N_SPECIES<-threshold
  #item_y<-item_y[which(item_y$global_id %in% polygon$Name),]
  #print(item_y)
  item_y$lat_band<-round(item_y$lat/5) * 5
  item_y_l<-item_y[, .(N_SPECIES=mean(N_SPECIES),
                       CI_N_SPECIES=my_CI(N_SPECIES)),
                   by=list(lat_band)]
  item_y_l$N_SPECIES_fix<-item_y_l$N_SPECIES
  item_y_l$N_SPECIES_fix<-item_y_l$N_SPECIES_fix - min(item_y_l$N_SPECIES_fix)
  range<-max(item_y_l$N_SPECIES_fix) - min(item_y_l$N_SPECIES_fix)
  if (range>0){
    item_y_l$N_SPECIES_fix<-item_y_l$N_SPECIES_fix/range * 50
    item_y_l$CI_N_SPECIES<-item_y_l$CI_N_SPECIES/range * 50
  }
  item_y_l$N_SPECIES_fix<-item_y_l$N_SPECIES_fix+170
  item_y_l<-item_y_l[between(lat_band, -50, 70)]
  item_y_l$lat_band<-item_y_l$lat_band/120 * 135
  setorderv(item_y_l, "lat_band")
  
  item_y<-data.frame(item_y)
  item_y<-merge(polygon, item_y, by.x="Name", by.y="global_id")
  #threshold<-round(mean(item_y$N_SPECIES)+3*sd(item_y$N_SPECIES))
  #threshold<-round(quantile(item_y$N_SPECIES, 0.75)+1.5*IQR(item_y$N_SPECIES))
  
  
  if (is_uniform==F){
    if ("max_species" %in% colnames(item_y)){
      max_n_sp<-max(item_y$max_species)
      min_n_sp<-min(item_y$N_SPECIES)
      #print(item_y)
      geom.sf<-geom_sf(data=item_y[which(item_y$N_SPECIES>0),][1,], aes(colour=max_species))
      
    }else{
      max_n_sp<-max(item_y$N_SPECIES)
      min_n_sp<-min(item_y$N_SPECIES)
      geom.sf<-NULL
    }
    if (threshold>max_n_sp){
      midpoint<-round(max_n_sp/2)
      breakss<-c(min_n_sp, midpoint, max_n_sp)
      labelss<-c("0", "", as.character(max_n_sp))
    }else{
      midpoint<-round(threshold/2)
      breakss<-c(min_n_sp, midpoint, threshold)
      labelss<-c("0", "", sprintf(">%d, up to %d", threshold, max_n_sp))
    }
    gradient_colors<-scale_color_gradient2(low  = "#4477AA", high="#EE6677",
                                           mid = "#DDDDDD", midpoint=midpoint,
                                           breaks=breakss, 
                                           labels=labelss)
    gradient_fill<-  scale_fill_gradient(guide="none")
  }else{
    gradient_colors<-scale_color_gradient2(low  = "#4477AA", high="#4477AA",
                                           mid = "#4477AA")
  }
  
  
  
  p_asia<-ggplot(item_y, aes(color=N_SPECIES, fill=N_SPECIES)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3")+
    geom_line(data=data.frame(x=c(-180, 180), y=0, N_SPECIES=0),
              aes(x=x, y=y), color="grey", linetype=2, linewidth=0.5)
  
  if (!is.null(geom.sf)){
    p_asia<-p_asia+geom.sf
  }
  
  p_asia<-  p_asia+geom_sf()+
    gradient_colors+gradient_fill+
    labs(color=legend_label, fill=legend_label)
  if (with_lat){
    p_asia<-p_asia+geom_ribbon(data=item_y_l, 
                               aes(xmin=N_SPECIES_fix-CI_N_SPECIES,
                                   xmax=N_SPECIES_fix+CI_N_SPECIES, 
                                   y=lat_band), color="white", fill="black", alpha=0.2)+
      
      geom_path(data=item_y_l, aes(x=N_SPECIES_fix, y=lat_band), 
                position="identity", color="black")
  }
  #xlim(-12e6, 12e6)+
  p_asia<-p_asia+ylim(-55, 80)+
    theme(#panel.grid.major = element_line(color = "#d4d4d4", 
          #                                linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank(),
          legend.position = c(0.1, 0.3),
          legend.title = element_blank(),
          plot.margin = margin(t = 0,  # Top margin
                               r = 0,  # Right margin
                               b = 0,  # Bottom margin
                               l = 0))+
    guides(linewidth = "none") 
  if (with_label){
    p_asia<-p_asia + ggtitle(sprintf("%s", label))
  }
  p_asia
  
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
evo_types_label_xy<-c("Directional 10% vs 50% & Shift",
                      "Directional 10% vs 50% & Expansion",
                      "Omnidirectional 10% vs 50% & Expansion")
formatLabelXY<-function(d_item){
  d_item$label_x<-evo_types_label_xy[1]
  d_item[label=="shift-directional (0.1)-shift-directional (0.5)"]$label_x<-evo_types_label_xy[1]
  
  d_item[label=="expansion-directional (0.1)-expansion-directional (0.5)"]$label_x<-evo_types_label_xy[2]
  
  d_item[label=="expansion-omnidirectional (0.1)-expansion-omnidirectional (0.5)"]$label_x<-evo_types_label_xy[3]
  
  d_item$label_x<-factor(d_item$label_x, levels=evo_types_label_xy)
  
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
