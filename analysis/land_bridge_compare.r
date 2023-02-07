library(dplyr)
library(rgdal)
library(raster)
library(RSQLite)
library(DBI)
library(data.table)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

base_new<-"../Results"
base_old<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"

sp<-"19980_POOR_BROAD_1_0_0"
log_new<-sprintf("%s/%s/%s.log", base_new, sp, sp)
print(paste(Sys.time(), file.size(log_new)/1024/1024))
df<-fread(log_new)


colnames(df)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
df<-df[suitable==1]
j=500

diversity_df_new<-df[, .(N_SPECIES=length(unique(sp_id)),
                         N_INDIVIDUAL=sum(n)), 
                     by=list(year, global_id)]

diversity_df_old<-readRDS(sprintf("%s/%s/%s.diversity.rda", base_old, sp, sp))
diversity_df<-merge(diversity_df_old, diversity_df_new, by=c("year", "global_id"), all=T)
diversity_df[is.na(N_SPECIES.x)]$N_SPECIES.x<-0
diversity_df[is.na(N_SPECIES.y)]$N_SPECIES.y<-0
diversity_df$N_SPECIES_Differ<-diversity_df$N_SPECIES.y-diversity_df$N_SPECIES.x
world <- ne_countries(scale = "small", returnclass = "sf")
polygon<-readRDS("../Figures/Example/polygon.rda")

crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

item_y<-merge(polygon, diversity_df[year==0], by.x="Name", by.y="global_id")
item_seed<-merge(polygon, diversity_df[year==1198], by.x="Name", by.y="global_id")
p_asia<-ggplot(item_y) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(aes(color=N_SPECIES.x, fill=N_SPECIES.x))+
  #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
  coord_sf(crs = st_crs(crs_asia))+
  xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                 title.position = "left", title.hjust = 1)) 

p_asia

p_asia2<-ggplot(item_y[which(item_y$N_SPECIES_Differ>=0),]) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(aes(color=N_SPECIES_Differ, fill=N_SPECIES_Differ))+
  #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
  coord_sf(crs = st_crs(crs_asia))+
  xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                 title.position = "left", title.hjust = 1)) 

p_asia2
ggarrange(p_asia, p_asia2)
#legend<-get_legend(p_asia)
#p_asia<-p_asia+theme(legend.position = "none")

p_america<-ggplot(item_y) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf(aes(color=N_SPECIES_Differ, fill=N_SPECIES_Differ))+
  geom_sf(data=item_seed, color="red", fill="red")+
  #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
  coord_sf(crs = st_crs(crs_america))+
  xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                 title.position = "left", title.hjust = 1)) 

p_america
p<-ggarrange(p_asia, p_america, common.legend = TRUE,legend="bottom")


