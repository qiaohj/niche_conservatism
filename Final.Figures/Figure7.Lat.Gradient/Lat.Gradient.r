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
                     label="birds")
birds_st$lat_band<-round(birds_st$lat/5) * 5
birds_st$mean_N_SPECIES_scaled<-birds_st$mean_N_SPECIES/max(birds_st$mean_N_SPECIES)
mammals_st<-data.table(global_id=mammals$global_id, lon=mammals$lon, lat=mammals$lat, mean_N_SPECIES=mammals$richness,
                       label="mammals")
mammals_st$lat_band<-round(mammals_st$lat/5) * 5
mammals_st$mean_N_SPECIES_scaled<-mammals_st$mean_N_SPECIES/max(mammals_st$mean_N_SPECIES)
iucn<-rbindlist(list(birds_st, mammals_st), use.names = T, fill=T)
setorderv(iucn, "lat_band")

n_splist_df<-readRDS("../Data/diversity/diversity_bootstrap.rda")

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

n_splist_df_ratio<-n_splist_df[group %in% c("shift-directional", 
                                            "expansion-directional",
                                            "expansion-omnidirectional")]
n_splist_df_conservatism$group<-"shift-directional"
iucn$group<-"shift-directional"
n_splist_df_ratio<-rbindlist(list(n_splist_df_ratio, n_splist_df_conservatism, iucn), fill=T, use.names = T)

n_splist_df_conservatism$group<-"expansion-directional"
iucn$group<-"expansion-directional"
n_splist_df_ratio<-rbindlist(list(n_splist_df_ratio, n_splist_df_conservatism, iucn), fill=T, use.names = T)

n_splist_df_conservatism$group<-"expansion-omnidirectional"
iucn$group<-"expansion-omnidirectional"
n_splist_df_ratio<-rbindlist(list(n_splist_df_ratio, n_splist_df_conservatism, iucn), fill=T, use.names = T)

n_splist_df_random<-n_splist_df[group=="random"]
n_splist_df_random<-rbindlist(list(n_splist_df_random, n_splist_df_conservatism, iucn), fill=T, use.names = T)

n_splist_df_ratio$directional_speed<-as.character(n_splist_df_ratio$directional_speed)
n_splist_df_ratio[directional_speed=="0"]$directional_speed<-"0"
unique(n_splist_df_ratio$directional_speed)
n_splist_df_ratio$directional_speed<-factor(n_splist_df_ratio$directional_speed,
                                            c("0", "0.01", "0.1", "0.5"))
n_splist_df_ratio<-n_splist_df_ratio[((directional_speed %in% c(0) & species_evo_type==1) |
                                        (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                                        (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)) |
                                        label %in% c("birds", "mammals"))]
n_splist_df_ratio_lat<-n_splist_df_ratio[, .(mean_N_SPECIES=mean(mean_N_SPECIES),
                                             mean_N_SPECIES_scaled=mean(mean_N_SPECIES_scaled),
                                             sd_mean_N_SPECIES_scaled=sd(mean_N_SPECIES_scaled),
                                             CI_mean_N_SPECIES_scaled=my_CI(mean_N_SPECIES_scaled)),
                                         by=list(species_evo_type, directional_speed, 
                                                 group, lat_band, evo_type, label)]


unique(n_splist_df_ratio_lat$label)
table(n_splist_df_ratio_lat$group)
n_splist_df_ratio_lat$color<-as.character(n_splist_df_ratio_lat$directional_speed)
n_splist_df_ratio_lat[label %in% c("birds", "mammals")]$color<-
  n_splist_df_ratio_lat[label %in% c("birds", "mammals")]$label
table(n_splist_df_ratio_lat$color)
p1<-ggplot(n_splist_df_ratio_lat)+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=mean_N_SPECIES_scaled, y=lat_band, color=color), position="identity")+
  geom_ribbon(aes(xmin=mean_N_SPECIES_scaled-CI_mean_N_SPECIES_scaled,
                     xmax=mean_N_SPECIES_scaled+CI_mean_N_SPECIES_scaled, 
                     y=lat_band, fill=color), alpha=0.2)+
  theme_bw()+
  labs(x="species diversity", y="Latitudinal band", color="Evolution ratio",
       fill="Evolution ratio")+
  facet_wrap(~group)+
  scale_color_manual(
    breaks=c("0", "0.1", "0.5", "birds", "mammals"),
    values=c("black", "#CCBB44", "#4477AA", "#228833", "#EE6677"), 
    labels=c("conservatism", "0.1", "0.5", "bird", "mammal"))+
  scale_fill_manual(
    breaks=c("0", "0.1", "0.5", "birds", "mammals"),
    values=c("black", "#CCBB44", "#4477AA", "#228833", "#EE6677"), 
    labels=c("conservatism", "0.1", "0.5", "bird", "mammal"))+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 0.75)+
  ylim(-50, 70)+
  theme(legend.position = c(0.25, 0.75),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.01, 0, 0, 0.01), "null"),
        legend.background = element_rect(fill=bg))
p1

n_splist_df_random$group<-"random"
n_splist_df_random<-n_splist_df_random[((directional_speed %in% c(0) & species_evo_type==1) |
                                          (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
                                          (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7))|
                                          label %in% c("birds", "mammals"))]
n_splist_df_random_lat<-n_splist_df_random[, .(mean_N_SPECIES=mean(mean_N_SPECIES),
                                               mean_N_SPECIES_scaled=mean(mean_N_SPECIES_scaled),
                                               sd_mean_N_SPECIES_scaled=sd(mean_N_SPECIES_scaled),
                                               CI_mean_N_SPECIES_scaled=my_CI(mean_N_SPECIES_scaled)),
                                           by=list(species_evo_type, directional_speed, group, 
                                                   lat_band, evo_type, label)]
table(n_splist_df_random_lat$label)
n_splist_df_random_lat$color<-as.character(n_splist_df_random_lat$evo_type)
n_splist_df_random_lat[label %in% c("birds", "mammals")]$color<-
  n_splist_df_random_lat[label %in% c("birds", "mammals")]$label
unique(n_splist_df_random_lat$color)
p2<-ggplot(n_splist_df_random_lat)+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=mean_N_SPECIES_scaled, y=lat_band, color=color), 
            position="identity")+
  geom_ribbon(aes(xmin=mean_N_SPECIES_scaled-CI_mean_N_SPECIES_scaled,
                  xmax=mean_N_SPECIES_scaled+CI_mean_N_SPECIES_scaled, 
                  y=lat_band, fill=color), alpha=0.2)+
  theme_bw()+
  labs(x="", y="Latitudinal band", color="Evolution scenario",
       fill="Evolution scenario")+
  facet_wrap(~group)+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 0.9)+
  ylim(-50, 70)+
  scale_color_manual(
    breaks=c("conservatism", "random-asymmetrical", "random-central", "random-symmetrical", "birds", "mammals"),
    values=c("black", "#CCBB44", "#4477AA", "#AA3377", "#228833", "#EE6677"),
    labels=c("conservatism", "random-asymmetrical", "random-central", "random-symmetrical", "birds", "mammals"))+
  scale_fill_manual(
    breaks=c("conservatism", "random-asymmetrical", "random-central", "random-symmetrical", "birds", "mammals"),
    values=c("black", "#CCBB44", "#4477AA", "#AA3377", "#228833", "#EE6677"),
    labels=c("conservatism", "random-asymmetrical", "random-central", "random-symmetrical", "birds", "mammals"))+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.65, 0.75),
        legend.background = element_rect(fill=bg),
        plot.margin = unit(c(0.01, 0.01, 0, 0.01), "null"))

p2
ggarrange(p1, p2, widths=c(3,1))
p<-ggarrange(p1, p2, widths=c(3,1))
#p
p<-annotate_figure(p, bottom = "Number of species")

ggsave(p, filename="../Figures/Figure7.Lat.Gradient/Lat_gradient_boot.png",
       width=12, height=6, bg="white")



