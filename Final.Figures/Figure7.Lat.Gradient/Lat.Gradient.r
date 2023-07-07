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

if (F){
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
}

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
p1<-ggplot(n_splist_df_ratio_lat)+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=mean_N_SPECIES_scaled, y=lat_band, color=evo_types_label_color), position="identity")+
  geom_ribbon(aes(xmin=mean_N_SPECIES_scaled-CI_mean_N_SPECIES_scaled,
                     xmax=mean_N_SPECIES_scaled+CI_mean_N_SPECIES_scaled, 
                     y=lat_band, fill=evo_types_label_color), alpha=0.2)+
  theme_bw()+
  labs(x="Species diversity", y="Latitudinal band", color="",
       fill="")+
  facet_wrap(~change_rate)+
  scale_color_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
  scale_fill_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 0.75)+
  ylim(-50, 70)+
  theme(legend.title = element_blank(),
    legend.position = c(0.5, 0.25),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.01, 0, 0, 0.01), "null"),
        legend.background = element_rect(fill=bg))
p1

n_splist_df_random$group<-"random"

n_splist_df_random_lat<-n_splist_df_random[, .(mean_N_SPECIES=mean(mean_N_SPECIES),
                                               mean_N_SPECIES_scaled=mean(mean_N_SPECIES_scaled),
                                               sd_mean_N_SPECIES_scaled=sd(mean_N_SPECIES_scaled),
                                               CI_mean_N_SPECIES_scaled=my_CI(mean_N_SPECIES_scaled)),
                                           by=list(species_evo_type, directional_speed, group, 
                                                   lat_band, evo_type, label)]
n_splist_df_random_lat<-formatLabels(n_splist_df_random_lat)


p2<-ggplot(n_splist_df_random_lat)+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=mean_N_SPECIES_scaled, y=lat_band, color=evo_types_label_color), 
            position="identity")+
  geom_ribbon(aes(xmin=mean_N_SPECIES_scaled-CI_mean_N_SPECIES_scaled,
                  xmax=mean_N_SPECIES_scaled+CI_mean_N_SPECIES_scaled, 
                  y=lat_band, fill=evo_types_label_color), alpha=0.2)+
  theme_bw()+
  labs(x="", y="Latitudinal band", color="Evolution scenario",
       fill="Evolution scenario")+
  facet_wrap(~group)+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0, 0.9)+
  ylim(-50, 70)+
  scale_color_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
  scale_fill_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.65, 0.75),
        legend.background = element_rect(fill=bg),
        plot.margin = unit(c(0.01, 0.01, 0, 0.01), "null"))

p2
ggarrange(p1, p2, widths=c(2,1))
p<-ggarrange(p1, p2, widths=c(2,1))
#p
p<-annotate_figure(p, bottom = "Number of species")

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
  scale_color_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
  scale_fill_manual(values=evo_type_color2, breaks=names(evo_type_color2))+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom")
p


ggsave(p, filename="../Figures/Figure7.Lat.Gradient/Lat_gradient_boot.png",
       width=12, height=6, bg="white")

ggsave(p, filename="../Figures/Figure7.Lat.Gradient/Lat_gradient_boot.pdf",
       width=12, height=6, bg="white")


conservatism<-n_splist_df_full[species_evo_type==1 & change_rate=="change rate = 10%"]
niche_shift_0.1<-n_splist_df_full[species_evo_type==2 & change_rate=="change rate = 10%"]
niche_shift_0.5<-n_splist_df_full[species_evo_type==2 & change_rate=="change rate = 50%"]
directional_niche_expansion_0.1<-n_splist_df_full[species_evo_type==3 & change_rate=="change rate = 10%"]
directional_niche_expansion_0.5<-n_splist_df_full[species_evo_type==3 & change_rate=="change rate = 50%"]
omnidirectional_niche_expansion_0.1<-n_splist_df_full[species_evo_type==4 & change_rate=="change rate = 10%"]
omnidirectional_niche_expansion_0.5<-n_splist_df_full[species_evo_type==4 & change_rate=="change rate = 50%"]
random_niche_shift<-n_splist_df_full[species_evo_type==5 & change_rate=="random"]
random_niche_expansion<-n_splist_df_full[species_evo_type==6 & change_rate=="random"]
random_niche_change_shift<-n_splist_df_full[species_evo_type==7 & change_rate=="random"]

birds<-n_splist_df_full[evo_types_label_color=="Birds" & change_rate=="change rate = 10%"]
mammals<-n_splist_df_full[evo_types_label_color=="Mammals" & change_rate=="change rate = 10%"]

cor_matrix<-data.table(mammals=mammals$mean_N_SPECIES_scaled,
                       birds=birds$mean_N_SPECIES_scaled,
                       conservatism=conservatism$mean_N_SPECIES_scaled, 
                       niche_shift_0.1=niche_shift_0.1$mean_N_SPECIES_scaled,
                       niche_shift_0.5=niche_shift_0.5$mean_N_SPECIES_scaled,
                       directional_niche_expansion_0.1=directional_niche_expansion_0.1$mean_N_SPECIES_scaled,
                       directional_niche_expansion_0.5=directional_niche_expansion_0.5$mean_N_SPECIES_scaled,
                       omnidirectional_niche_expansion_0.1=omnidirectional_niche_expansion_0.1$mean_N_SPECIES_scaled,
                       omnidirectional_niche_expansion_0.5=omnidirectional_niche_expansion_0.5$mean_N_SPECIES_scaled,
                       random_niche_shift=random_niche_shift$mean_N_SPECIES_scaled,
                       random_niche_change_shift=random_niche_change_shift$mean_N_SPECIES_scaled,
                       random_niche_expansion=random_niche_expansion$mean_N_SPECIES_scaled)
cor(cor_matrix)
corrplot::corrplot(cor(cor_matrix), method = 'number')

