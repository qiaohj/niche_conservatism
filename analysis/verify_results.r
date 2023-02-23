
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
df<-fread("/media/huijieqiao/Butterfly/Niche_Conservatism/Results/20956_GOOD_BROAD_1_0_0/20956_GOOD_BROAD_1_0_0.log")
colnames(df)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
polygon<-readRDS("../Figures/Example/polygon.rda")
world <- ne_countries(scale = "small", returnclass = "sf")
df<-df[suitable==1]

crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
y=100

for (y in c(1190:0)){
  #unique(df[year==y]$sp_id)
  my.age <- readline(prompt=sprintf("current year is %d: ", y))
  if (my.age=="x"){
    break()
  }
  #item_y<-df[year==y & sp_id %in% unique(df[year==y]$sp_id)[1:2]]
  item_y<-df[year==y]
  item_y<-merge(polygon, item_y, by.x="Name", by.y="global_id")
  
  
  p_asia<-ggplot(item_y, aes(colour=factor(group_id))) +
    geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
    geom_sf()+
    
    #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
    coord_sf(crs = st_crs(crs_asia))+
    labs(colour="Species richness")+
    xlim(-10e6, 1e6)+
    ylim(-3e6, 6e6)+
    theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.title = element_blank(),
          legend.position = "bottom")+
    facet_wrap(~sp_id)
  print(p_asia)
}
#legend<-get_legend(p_asia)
#p_asia<-p_asia+theme(legend.position = "none")

p_america<-ggplot(item_y, aes(colour=N_SPECIES)) +
  geom_sf(data = world, color="#e3e3e3", fill="#e3e3e3") +
  geom_sf()+
  scale_color_gradient2(low  = mycol[1], high=mycol[length(mycol)],
                        mid = "#DDDDDD", midpoint=midpoint,
                        breaks=breakss, 
                        labels=labelss)+
  
  #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
  coord_sf(crs = st_crs(crs_america))+
  labs(colour="Species richness")+
  xlim(-12e6, 12e6)+
  ylim(-12e6, 12e6)+
  theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.title = element_blank(),
        legend.position = "bottom")+
  guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                 title.position = "left", title.hjust = 1)) 

p<-ggarrange(p_asia, p_america, common.legend = TRUE,legend="bottom")

p<-annotate_figure(p, top = sprintf("%.1f kyb", y/10))
}