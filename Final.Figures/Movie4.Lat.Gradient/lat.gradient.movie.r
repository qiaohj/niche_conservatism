library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(DBI)
library(sf)
library(Rmisc)
library(ggpubr)
setDTthreads(20)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
mask<-readRDS("../Data/mask_lonlat.rda")

mammals<-st_read("../Shape/IUCN_Richness/Mammals/richness.shp")
birds<-st_read("../Shape/IUCN_Richness/Birds/richness.shp")

birds_st<-data.table(global_id=birds$global_id, lon=birds$lon, lat=birds$lat, mean_N_SPECIES=birds$richness,
                     label="birds", species_evo_type=8)
birds_st$lat_band<-round(birds_st$lat/5) * 5
birds_st$mean_N_SPECIES_scaled<-birds_st$mean_N_SPECIES/max(birds_st$mean_N_SPECIES)
mammals_st<-data.table(global_id=mammals$global_id, lon=mammals$lon, lat=mammals$lat, mean_N_SPECIES=mammals$richness,
                       label="mammals", species_evo_type=9)
mammals_st$lat_band<-round(mammals_st$lat/5) * 5
mammals_st$mean_N_SPECIES_scaled<-mammals_st$mean_N_SPECIES/max(mammals_st$mean_N_SPECIES)
iucn<-rbindlist(list(birds_st, mammals_st), use.names = T, fill=T)
setorderv(iucn, "lat_band")
y=1198
for (y in c(0:1198)){
  print(y)
  n_splist_df<-readRDS(sprintf("../Data/diversity/diversity_bootstrap_years/%d.rda", y))
  
  n_splist_df<-merge(n_splist_df, mask, by="global_id")
  n_splist_df$lat_band<-round(n_splist_df$lat/5) * 5
  setorderv(n_splist_df, "lat_band")
  
  n_splist_df$evo_type<-format_evoType(n_splist_df$species_evo_type)
  n_splist_df$label<-format_evoType_amplitude(n_splist_df$evo_type, n_splist_df$directional_speed, order=-1)
  #n_splist_df$mean_N_SPECIES_scaled<-n_splist_df$mean_N_SPECIES/
  #  max(n_splist_df[group=="conservatism"]$mean_N_SPECIES)
  
  n_splist_df$mean_N_SPECIES_scaled<-n_splist_df$mean_N_SPECIES/3000
  
  n_splist_df[, .(maxxx=max(mean_N_SPECIES)),
              by=c("group")]
  
  n_splist_df$group<-n_splist_df$evo_type
  
  n_splist_df[grepl("random", evo_type)]$group<-"random"
  n_splist_df_conservatism<-n_splist_df[group=="conservatism"]
  
  
  n_splist_df_0.1<-n_splist_df[directional_speed %in% c(0.1)]
  n_splist_df_conservatism$directional_speed<-0.1
  iucn$directional_speed<-0.1
  
  n_splist_df_0.1<-rbindlist(list(n_splist_df_0.1, n_splist_df_conservatism, iucn), fill=T, use.names = T)
  n_splist_df_0.1$change_rate<-sprintf("%s = %s", change_rate, "10%")
  
  n_splist_df_0.5<-n_splist_df[directional_speed %in% c(0.5)]
  n_splist_df_conservatism$directional_speed<-0.5
  iucn$directional_speed<-0.5
  
  n_splist_df_0.5<-rbindlist(list(n_splist_df_0.5, n_splist_df_conservatism, iucn), fill=T, use.names = T)
  n_splist_df_0.5$change_rate<-sprintf("%s = %s", change_rate, "50%")
  
  n_splist_df_random<-n_splist_df[group=="random"]
  n_splist_df_random<-rbindlist(list(n_splist_df_random, n_splist_df_conservatism, iucn), fill=T, use.names = T)
  
  n_splist_df_ratio<-rbindlist(list(n_splist_df_0.1, n_splist_df_0.5))
  
  
  n_splist_df_ratio[species_evo_type==1]
  n_splist_df_0.1[species_evo_type==1]
  n_splist_df_ratio_lat<-n_splist_df_ratio[, .(mean_N_SPECIES=mean(mean_N_SPECIES),
                                               mean_N_SPECIES_scaled=mean(mean_N_SPECIES_scaled),
                                               sd_mean_N_SPECIES_scaled=sd(mean_N_SPECIES_scaled),
                                               CI_mean_N_SPECIES_scaled=my_CI(mean_N_SPECIES_scaled)),
                                           by=list(species_evo_type, directional_speed, 
                                                   group, lat_band, evo_type, label,
                                                   change_rate)]
  
  
  unique(n_splist_df_ratio_lat$label)
  table(n_splist_df_ratio_lat$group)
  n_splist_df_ratio_lat$color<-as.character(n_splist_df_ratio_lat$directional_speed)
  n_splist_df_ratio_lat[label %in% c("birds", "mammals")]$color<-
    n_splist_df_ratio_lat[label %in% c("birds", "mammals")]$label
  table(n_splist_df_ratio_lat$color)
  n_splist_df_ratio_lat<-formatLabels(n_splist_df_ratio_lat)
  
  
  n_splist_df_random$group<-"random"
  
  n_splist_df_random_lat<-n_splist_df_random[, .(mean_N_SPECIES=mean(mean_N_SPECIES),
                                                 mean_N_SPECIES_scaled=mean(mean_N_SPECIES_scaled),
                                                 sd_mean_N_SPECIES_scaled=sd(mean_N_SPECIES_scaled),
                                                 CI_mean_N_SPECIES_scaled=my_CI(mean_N_SPECIES_scaled)),
                                             by=list(species_evo_type, directional_speed, group, 
                                                     lat_band, evo_type, label)]
  n_splist_df_random_lat<-formatLabels(n_splist_df_random_lat)
  
  
  n_splist_df_random_lat$change_rate<-"random"
  n_splist_df_full<-rbindlist(list(n_splist_df_ratio_lat, n_splist_df_random_lat), use.names = T, fill=T)
  
  n_splist_df_full$change_rate <- factor(n_splist_df_full$change_rate,
                                         levels = c("change rate = 10%", "change rate = 50%", "random"),
                                         labels=c("change rate = 10%", "change rate = 50%", "random niche change"))
  p<-ggplot(n_splist_df_full)+
    
    #geom_point(aes(x=N_Species, y=mid, color=label))+
    geom_path(aes(x=mean_N_SPECIES_scaled, y=lat_band, color=evo_types_label_color), 
              position="identity")+
    geom_ribbon(aes(xmin=mean_N_SPECIES_scaled-CI_mean_N_SPECIES_scaled,
                    xmax=mean_N_SPECIES_scaled+CI_mean_N_SPECIES_scaled, 
                    y=lat_band, fill=evo_types_label_color), alpha=0.2)+
    theme_bw()+
    labs(x="", y="Latitudinal band", color="Evolution scenario",
         fill="Evolution scenario")+
    facet_wrap(~change_rate)+
    #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
    xlim(0, 0.9)+
    ylim(-50, 70)+
    ggtitle(sprintf("%s %s", as.character(y/10), x_label))+
    scale_color_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
    scale_fill_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
    guides(fill=guide_legend(nrow=2,byrow=TRUE),
           color=guide_legend(nrow=2,byrow=TRUE))+
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom")
  #p
  
  
  ggsave(p, filename=sprintf("../Figures/Movie4.Lat.Gradient/years/Lat_gradient_boot-%d.png", y),
         width=12, height=6, bg="white")
  
}
#cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Movie4.Lat.Gradient/years
#ffmpeg -r 20 -start_number -1200 -i Lat_gradient_boot%d.png -y ../lat_gradient.mp4
