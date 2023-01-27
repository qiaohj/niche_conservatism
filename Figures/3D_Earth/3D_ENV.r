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
library(plotKML)
library(ggtree)
library(phylobase)
library(ggpubr)
library(heatmaply)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
#source("commons/diverging_map.r")
polygon<-readRDS("../Figures/Example/polygon.rda")

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
v_prcp_mean<-v_prcp[, .(mean_v=mean(v),
                        sd_v=sd(v)),
                    by=list(year)]
v_tmax_mean<-v_tmax[, .(mean_v=mean(v),
                        sd_v=sd(v)),
                    by=list(year)]
v_tmin_mean<-v_tmin[, .(mean_v=mean(v),
                        sd_v=sd(v)),
                    by=list(year)]
wc_col<-cool_warm(2)
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


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"


y<-0
v_prcp[v>50]$v<-50
v_prcp[v<0]$v<-0
min_tmax<-round(min(c(v_tmax$v, v_tmin$v)))
max_tmax<-round(max(c(v_tmax$v, v_tmin$v)))
min_tmin<-round(min(c(v_tmax$v, v_tmin$v)))
max_tmin<-round(max(c(v_tmax$v, v_tmin$v)))
min_prcp<-0
max_prcp<-50
y
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
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  #p_asia
  
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
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
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
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
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
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
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
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
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
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank())
  
  #p_america
  p_prcp<-ggarrange(p_asia_prcp, p_america_prcp, common.legend = TRUE, legend="right")
  
  p_line<-p+geom_vline(xintercept = y * -0.1, linetype = "dashed")
  p_final<-ggarrange(plotlist = list(p_tmax, p_tmin, p_prcp, p_line), ncol=1, heights = c(1.5, 1.5, 1.5, 1))
  p_final<-annotate_figure(p_final, top = text_grob(sprintf("%.1f K years before present", y * 0.1), 
                                                    color = "black", size = 14))
  ggsave(p_final, filename=sprintf("../Figures/Env_Change/by_year/%d.png", y), width=6, height=9, bg="white")
  
}

next()
p_global<-ggplot(polygon_tmax) +
  #geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="grey", fill="white") + 
  coord_sf(crs = st_crs(crs_asia))+ xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank())
ggsave(p_global, filename="../Figures/Configure/global.png", width=12, height=12, bg="white")


base_db<-sprintf("%s/Configuration/conf.sqlite", "../")
config <- dbConnect(RSQLite::SQLite(), base_db)

simulations<-dbReadTable(config, "simulations")

dbDisconnect(config)

seeds_index<-unique(simulations$global_id)

seeds<-polygon_tmax[which(polygon_tmax$Name %in% seeds_index),]
cols<-Reds(100)[80]
cols_blue<-Blues(100)[80]

outlier_ids<-readRDS("../Data/outliers/outliers_IQR.rda")
seeds$color<-"Normal"
seeds[which(seeds$Name %in% outlier_ids), "color"]<-"Outliers"
p_seeds1<-ggplot(polygon_tmax) +
  #geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="#e3e3e3", fill="white", size=0.1) +
  geom_sf(data=seeds, aes(color=color, fill=color), size=0.1) + 
  scale_color_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  scale_fill_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  coord_sf(crs = st_crs(crs_asia))+ xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "none")

p_seeds2<-ggplot(polygon_tmax) +
  #geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(color="#e3e3e3", fill="white", size=0.1) +
  geom_sf(data=seeds, aes(color=color, fill=color), size=0.1) + 
  scale_color_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  scale_fill_manual(values=c(cols, cols_blue), breaks=c("Normal", "Outliers"))+
  coord_sf(crs = st_crs(crs_america))+ xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "none")
p_seeds<-ggarrange(p_seeds1, p_seeds2, nrow=1)
ggsave(p_seeds, filename="../Figures/Configure/seeds.png", width=12, height=6, bg="white")


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
ggsave(p_seeds, filename="../Figures/Configure/seeds.pdf", width=24, height=12, bg="white")
ggsave(p_seeds2_part, filename="../Figures/Configure/seeds2_part.pdf", width=4, height=3, bg="white")

#cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Env_Change/by_year
#ls | xargs -I {} mv {} y-{}

#ffmpeg -r 20 -start_number -1200 -i y%d.png -y ../Env_Change.mp4

