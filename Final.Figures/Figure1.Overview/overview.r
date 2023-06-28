library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(ggspatial)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
#library(plotKML)
library(ggtree)
library(phylobase)
library(ggpubr)
library(heatmaply)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
polygon<-readRDS("../Figures/Figure1.Overview/Data/polygon.rda")

world <- ne_countries(scale = "small", returnclass = "sf")
env_df<-readRDS("../Figures/Figure1.Overview/Data/env_yearly_avg.rda")
wc_col<-c("#4477AA", "#EE6677")
v_tmax_mean<-env_df[var=="Debiased_Maximum_Monthly_Temperature"]
v_tmin_mean<-env_df[var=="Debiased_Minimum_Monthly_Temperature"]
v_prcp_mean<-env_df[var=="Debiased_Maximum_Monthly_Precipitation"]

p <- ggplot(v_tmax_mean, aes(x = year * -0.1))+
  geom_line(aes(y = mean_v), colour = wc_col[2])+
  #geom_line(data=v_tmin_mean, aes(y = mean_v), colour = wc_col[1])+
  geom_line(data=v_prcp_mean, aes(y = mean_v * 8 - 35), colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~(.+35)/8, name = "Precipitation (mm/day)"))+
  labs(y = "Maximum Temperature (degree)",
       x = "K years before present")+
  xlim(-120, 0)+
  theme_bw()+
  theme(legend.position = c(0.8, 0.9))

p
ggsave(p, filename="../Figures/Figure1.Overview/env_change.pdf", width=12, height=4)




base_db<-sprintf("%s/Configuration/env_Hadley3D.sqlite", "../")
envdb <- dbConnect(RSQLite::SQLite(), base_db)

v_prcp<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Precipitation")
v_tmax<-dbReadTable(envdb, "Debiased_Maximum_Monthly_Temperature")
v_tmin<-dbReadTable(envdb, "Debiased_Minimum_Monthly_Temperature")
dbDisconnect(envdb)
v_prcp<-data.table(v_prcp)
v_tmax<-data.table(v_tmax)
v_tmin<-data.table(v_tmin)
v_prcp$var<-"Debiased_Maximum_Monthly_Precipitation"
v_tmax$var<-"Debiased_Maximum_Monthly_Temperature"
v_tmin$var<-"Debiased_Minimum_Monthly_Temperature"

y<-0
v_prcp[v>50]$v<-50
v_prcp[v<0]$v<-0
min_tmax<-round(min(c(v_tmax$v, v_tmin$v)))
max_tmax<-round(max(c(v_tmax$v, v_tmin$v)))
min_tmin<-round(min(c(v_tmax$v, v_tmin$v)))
max_tmin<-round(max(c(v_tmax$v, v_tmin$v)))
min_prcp<-0
max_prcp<-50
y=1200

  polygon_tmax<-merge(polygon, v_tmax[year==y], by.x="Name", by.y="global_id")
  
  fake_colors<-data.table(x=c(13e6, 13e6), y=c(13e6, 13e6), v=c(min_tmax-1, max_tmax+1))
  mycol <- cool_warm(max_tmax - min_tmax + 1)
  polygon_tmax$color<-mycol[floor(polygon_tmax$v) - min_tmax + 1]
  
  #subcols<-mycol[c(min(polygon_max_temp$temp_int):max(polygon_max_temp$temp_int)) - min_tmax + 1]
  
  p_asia_tmax<-ggplot(polygon_tmax, aes(colour=v)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf() + 
    geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
    scale_colour_gradientn(colours  = mycol)+
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_asia))+
    labs(colour="TMAX")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  #p_asia_tmax
  
  p_america_tmax<-ggplot(polygon_tmax, aes(colour=v)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf() + 
    geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
    scale_colour_gradientn(colours  = mycol)+
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_america))+
    labs(colour="TMAX")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  
  #p_america
  p_tmax<-ggarrange(p_asia_tmax, p_america_tmax, common.legend = TRUE, legend="right")
  
  ####tmin
  
  polygon_tmin<-merge(polygon, v_tmin[year==y], by.x="Name", by.y="global_id")
  fake_colors<-data.table(x=c(13e6, 13e6), y=c(13e6, 13e6), v=c(min_tmin-1, max_tmin+1))
  mycol <- cool_warm(max_tmin - min_tmin + 1)
  polygon_tmin$color<-mycol[floor(polygon_tmin$v) - min_tmin + 1]
  
  #subcols<-mycol[c(min(polygon_max_temp$temp_int):max(polygon_max_temp$temp_int)) - min_tmin + 1]
  
  p_asia_tmin<-ggplot(polygon_tmin, aes(colour=v)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf() + 
    geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
    scale_colour_gradientn(colours  = mycol)+
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_asia))+
    labs(colour="TMIN")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  #p_asia
  
  p_america_tmin<-ggplot(polygon_tmin, aes(colour=v)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf() + 
    geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
    scale_colour_gradientn(colours  = mycol)+
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_america))+
    labs(colour="TMIN")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  
  #p_america
  p_tmin<-ggarrange(p_asia_tmin, p_america_tmin, common.legend = TRUE, legend="right")
  
  
  
  ####prcp
  
  polygon_prcp<-merge(polygon, v_prcp[year==y], by.x="Name", by.y="global_id")
  
  fake_colors<-data.table(x=c(13e6, 13e6), y=c(13e6, 13e6), v=c(min_prcp, max_prcp))
  mycol <- Blues((max_prcp - min_prcp + 1) * 1.5)[((max_prcp - min_prcp + 1) * 0.5) : 
                                                    ((max_prcp - min_prcp + 1) * 1.5)]
  polygon_prcp$color<-mycol[floor(polygon_prcp$v) - min_prcp + 1]
  
  #subcols<-mycol[c(min(polygon_max_temp$temp_int):max(polygon_max_temp$temp_int)) - min_prcp + 1]
  
  p_asia_prcp<-ggplot(polygon_prcp, aes(colour=v)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf() + 
    geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
    scale_colour_gradientn(colours  = mycol)+
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_asia))+
    labs(colour="PRCP")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  #p_asia
  #hist(polygon_prcp$v)
  p_america_prcp<-ggplot(polygon_prcp, aes(colour=v)) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf() + 
    geom_point(data=fake_colors, aes(x=x, y=y, colour=v))+
    scale_colour_gradientn(colours  = mycol)+
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_america))+
    labs(colour="PRCP")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  
  #p_america
  p_prcp<-ggarrange(p_asia_prcp, p_america_prcp, common.legend = TRUE, legend="right")
  
  p_line<-p+geom_vline(xintercept = y * -0.1, linetype = "dashed")
  p_env<-ggarrange(plotlist = list(p_tmax, p_tmin, p_prcp), ncol = 1)
  p_env<-annotate_figure(p_env, top = text_grob(sprintf("%.1f K years before present", y * 0.1), 
                                                    color = "black", size = 14))


p_global<-ggplot(polygon_tmax) +
  #geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="grey", fill="white") + 
  coord_sf(crs = st_crs(crs_asia))+ xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank())
#ggsave(p_global, filename="../Figures/Configure/global.png", width=12, height=12, bg="white")


base_db<-sprintf("%s/Configuration/conf.sqlite", "../")
config <- dbConnect(RSQLite::SQLite(), base_db)

simulations<-dbReadTable(config, "simulations")

dbDisconnect(config)

seeds_index<-unique(simulations$global_id)

seeds<-polygon_tmax[which(polygon_tmax$Name %in% seeds_index),]
cols<-"#EE6677"
cols_blue<-'#4477AA'

outlier_ids<-readRDS("../Data/outliers/outliers_3SD.rda")
seeds$color<-"Normal"
seeds[which(seeds$Name %in% outlier_ids$global_id), "color"]<-"Outliers"
p_seeds1<-ggplot(polygon_tmax) +
  #geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="#e3e3e3", fill="white", size=0.1) +
  geom_sf(data=seeds[which(seeds$color=="Normal"),], aes(color=color, fill=color), size=0.1) + 
  scale_color_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  scale_fill_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  coord_sf(crs = st_crs(crs_asia))+ xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "none")

p_seeds2<-ggplot(polygon_tmax) +
  #geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="#e3e3e3", fill="white", size=0.1) +
  geom_sf(data=seeds[which(seeds$color=="Normal"),], aes(color=color, fill=color), size=0.1) + 
  scale_color_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  scale_fill_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  coord_sf(crs = st_crs(crs_america))+ xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "none")
p_seeds<-ggarrange(p_seeds1, p_seeds2, nrow=1)
ggsave(p_seeds, filename="../Figures/Figure1.Overview/seeds.pdf", width=12, height=6, bg="white")
ggsave(p_seeds, filename="../Figures/Figure1.Overview/seeds.png", width=12, height=6, bg="white")

#species richness

diversity_final<-readRDS("../Data/diversity/diversity_bootstrap.rda")
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_final<-merge(diversity_final, ll, by="global_id", all=T)
diversity_final[is.na(mean_N_SPECIES)]$mean_N_SPECIES<-0
diversity_final[is.na(medium_N_SPECIES)]$medium_N_SPECIES<-0

diversity_final
diversity_final[group=="conservatism" & medium_N_SPECIES==max(diversity_final[group=="conservatism"]$medium_N_SPECIES)]
diversity_final$N_SPECIES<-round(diversity_final$mean_N_SPECIES)
#diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
#  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
p.richness<-create_fig(diversity_final[group=="conservatism"], "conservatism", 
                       with_label=T, legend_label="species numbers")
ggsave(p.richness, filename="../Figures/Figure1.Overview/species.richness.niche.conservatism.png",
       width=6, height=6, bg="white")

##lat gradient
mammals<-st_read("../Shape/IUCN_Richness/Mammals/richness.shp")
birds<-st_read("../Shape/IUCN_Richness/Birds/richness.shp")
mask<-readRDS("../Data/mask_lonlat.rda")

n_splist_df<-readRDS("../Data/diversity/diversity_bootstrap.rda")
n_splist_df<-n_splist_df[group=="conservatism"]
n_splist_df<-merge(n_splist_df, mask, by="global_id")
n_splist_df$lat_band<-round(n_splist_df$lat/5) * 5
n_splist_df$mean_N_SPECIES_scaled<-n_splist_df$mean_N_SPECIES/max(n_splist_df$mean_N_SPECIES)
birds_st<-data.table(global_id=birds$global_id, lon=birds$lon, lat=birds$lat, mean_N_SPECIES=birds$richness,
                     group="birds")
birds_st$lat_band<-round(birds_st$lat/5) * 5
birds_st$mean_N_SPECIES_scaled<-birds_st$mean_N_SPECIES/max(birds_st$mean_N_SPECIES)
mammals_st<-data.table(global_id=mammals$global_id, lon=mammals$lon, lat=mammals$lat, mean_N_SPECIES=mammals$richness,
                     group="mammals")
mammals_st$lat_band<-round(mammals_st$lat/5) * 5
mammals_st$mean_N_SPECIES_scaled<-mammals_st$mean_N_SPECIES/max(mammals_st$mean_N_SPECIES)
all_dt<-rbindlist(list(birds_st, mammals_st, n_splist_df), use.names = T, fill=T)


setorderv(all_dt, "lat_band")

n_splist_df_ratio_lat<-all_dt[, .(mean_N_SPECIES=mean(mean_N_SPECIES),
                                  mean_N_SPECIES_scaled=mean(mean_N_SPECIES_scaled)),
                                         by=list(lat_band, group)]
p_lat_band<-ggplot(n_splist_df_ratio_lat)+
  
  #geom_point(aes(x=N_Species, y=mid, color=label))+
  geom_path(aes(x=mean_N_SPECIES_scaled, y=lat_band, color=group), position="identity")+
  #geom_errorbarh(aes(xmin=N_Species-N_Species_SD, xmax=N_Species+N_Species_SD, y=mid))+
  theme_bw()+
  labs(x="Number of species", y="Latitudinal band", color="Evolution ratio",
       fill="Evolution ratio")+
  #scale_color_manual(values=c("black", colorBlindGrey8[2:4]))+
  #scale_x_continuous(breaks=c(500, 1000, 1500, 2000), labels=c(500, 1000, 1500, 2000))+
  xlim(0.08, 0.75)+
  ylim(-50, 70)+
  scale_color_manual(values=c("#228833", "#EE6677", "#4477AA"), 
                     breaks = c("birds", "mammals", "conservatism"))+
  theme(legend.position = c(0.7, 0.7),
        axis.title.x = element_blank(),
        plot.margin = unit(c(0.01, 0, 0, 0.01), "null"))

p_seeds2_part<-ggplot(polygon_tmax) +
  #geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="#e3e3e3", fill="white", size=0.3) +
  geom_sf(data=seeds, color=cols, fill=cols, size=0.3) + 
  coord_sf(crs = st_crs(crs_america))+ 
  ylim(0, 1e6)+
  xlim(0e6, 1.5e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank())
p_seeds2_part
ggsave(p_seeds2_part, filename="../Figures/Figure1.Overview/seeds2_part.pdf", width=4, height=3, bg="white")
#p_lat_band
#p_env
#p_seeds
#p.richness
#p_line
pp1<-ggarrange(plotlist=list(p_seeds, p_env), ncol=2, nrow=1, widths=c(3, 1))
pp2<-ggarrange(plotlist=list(p.richness, p_lat_band), ncol=2, nrow=1, widths=c(3, 1))
pp<-ggarrange(plotlist=list(pp1, p_line, pp2), ncol=1, nrow=3, heights=c(2, 1, 2))
ggsave(pp, filename="../Figures/Figure1.Overview/overview.png",
       width=12, height=12, bg="white")


ggsave(pp, filename="../Figures/Figure1.Overview/overview.pdf",
       width=12, height=12, bg="white")

