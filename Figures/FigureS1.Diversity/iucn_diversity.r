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
setDTthreads(20)
#source("commons/diverging_map.r")
polygon<-readRDS("../Figures/Example/polygon.rda")
world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

year<-0
create_fig<-function(item_y, label, barwidth=10){
  threshold<-round(mean(item_y$N_SPECIES)+3*sd(item_y$N_SPECIES))
  #threshold<-round(quantile(item_y$N_SPECIES, 0.75)+1.5*IQR(item_y$N_SPECIES))
  threshold<-4000
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
    scale_color_gradient2(low  = "#3B4CC0", high="#B40426",
                          mid = "#DDDDDD", midpoint=midpoint,
                          breaks=breakss, 
                          labels=labelss)+
    
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_asia))+
    labs(colour="")+
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
    scale_color_gradient2(low  = "#3B4CC0", high="#B40426",
                          mid = "#DDDDDD", midpoint=midpoint,
                          breaks=breakss, 
                          labels=labelss)+
    
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_america))+
    labs(colour="")+
    xlim(-12e6, 12e6)+
    ylim(-12e6, 12e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", 
                                          linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank(),
          legend.position = "bottom")+
    guides(color = guide_colourbar(barwidth = barwidth, barheight = NULL,
                                   title.position = "left", title.hjust = 1)) 
  
  p<-ggarrange(p_asia, p_america, common.legend = TRUE,legend="bottom")
  
  p<-annotate_figure(p, top = sprintf("%s", label))
  return(p)
}



diversity_final<-read_sf("../Shape/IUCN_Richness/Birds/richness.shp")
diversity_final$geometry<-NULL
diversity_final<-data.table(diversity_final)
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_final<-merge(diversity_final, ll, by="global_id", all=F)
diversity_final[is.na(richness)]$richness<-0

diversity_final$N_SPECIES<-diversity_final$richness
#diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
#  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
p1<-create_fig(diversity_final, "Bird")
hist(diversity_final$N_SPECIES)
p1
ggsave(p1, filename="../Figures/Diversity/Diversity.bird.png", bg="white", width=8, height=4)

diversity_final<-read_sf("../Shape/IUCN_Richness/Mammals/richness.shp")
diversity_final$geometry<-NULL
diversity_final<-data.table(diversity_final)
ll<-readRDS("../Data/mask_lonlat.rda")
diversity_final<-merge(diversity_final, ll, by="global_id", all=F)
diversity_final[is.na(richness)]$richness<-0

diversity_final$N_SPECIES<-diversity_final$richness
#diversity_final[lon<(-20) & lat<11]$N_SPECIES<-
#  round(diversity_final[lon<(-20) & lat<11]$N_SPECIES * 2)
p2<-create_fig(diversity_final, "Mammal")
ggsave(p2, filename="../Figures/Diversity/Diversity.mammal.png", bg="white", width=8, height=4)
p<-ggarrange(p1, p2, nrow=2, ncol=1)
ggsave(p, filename="../Figures/Diversity/Diversity.iucn.png", bg="white", width=8, height=8)
