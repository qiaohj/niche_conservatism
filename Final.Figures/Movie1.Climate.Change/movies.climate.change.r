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
env_df<-readRDS("../Figures/Figure1.Overview/Data/env_yearly_avg.rda")
wc_col<-c("#4477AA", "#EE6677")
v_tmax_mean<-env_df[var=="Debiased_Maximum_Monthly_Temperature"]
v_tmin_mean<-env_df[var=="Debiased_Minimum_Monthly_Temperature"]
v_prcp_mean<-env_df[var=="Debiased_Maximum_Monthly_Precipitation"]

p <- ggplot(v_tmax_mean, aes(x = year * -0.1))+
  geom_line(aes(y = mean_v), colour = wc_col[2])+
  geom_line(data=v_tmin_mean, aes(y = mean_v), colour = wc_col[1])+
  geom_line(data=v_prcp_mean, aes(y = mean_v * 20 - 150), colour = "black")+
  scale_y_continuous(sec.axis = sec_axis(~(.+150)/20, name = "Precipitation (mm/day)"))+
  labs(y = "Temperature (degree)",
       x = "K years before present")+
  xlim(-120, 0)+
  theme_bw()+
  theme(legend.position = c(0.8, 0.9))

p

polygon<-readRDS("../Figures/Figure1.Overview/Data/polygon.rda")

world <- ne_countries(scale = "small", returnclass = "sf")
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
for (y in c(1200:0)){
  print(y)
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
  p_final<-ggarrange(plotlist = list(p_tmax, p_tmin, p_prcp, p_line), ncol=1, heights = c(1.5, 1.5, 1.5, 1))
  p_final<-annotate_figure(p_final, top = text_grob(sprintf("%.1f K years before present", y * 0.1), 
                                                    color = "black", size = 14))
  ggsave(p_final, filename=sprintf("../Figures/Figure1.Overview/Env_Change/by_year/%d.png", y), width=6, height=9, bg="white")
  #ggsave(p_final, filename=sprintf("../Figures/Figure1.Overview/Env_Change/by_year/%d.pdf", y), width=6, height=9, bg="white")
  
}

