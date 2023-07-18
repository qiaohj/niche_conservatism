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
#diversity<-readRDS("../Data/diversity.rda")
nb<-"BROAD"
da<-"GOOD"
#diversity<-readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Data/diversity_items/%s_%s.rda", nb, da))
diversity<-readRDS("../Data/diversity/diversity.rda")
world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

year<-0
diversity[N_SPECIES==max(diversity$N_SPECIES)]
outlier_ids<-readRDS("../Data/outliers.rda")
diversity<-diversity[!(global_id %in% outlier_ids)]
#diversity_polygon<-merge(polygon, diversity, by.x="Name", by.y="global_id")
cols<-c("evo_type", "species_evo_type",
        "directional_speed", "species_evo_level")
combinations<-unique(diversity[, ..cols])
i<-1
y=0
args = commandArgs(trailingOnly=TRUE)

ii<-as.numeric(args[1])
if (is.na(ii)){
  ii=1
}
#for (i in c(1:nrow(combinations))){
for (i in c(ii:ii)){
  ccc<-combinations[i]
  item<-diversity[evo_type==ccc$evo_type &
                    species_evo_type==ccc$species_evo_type &
                    directional_speed==ccc$directional_speed &
                    species_evo_level==ccc$species_evo_level]
    
  for (y in c(0:1198)){
    print(paste(y, ccc$species_evo_type, ccc$directional_speed))
    item_y<-item[year==y]
    
    threshold<-round(mean(item_y$N_SPECIES)+3*sd(item_y$N_SPECIES))
    mycol <- cool_warm(threshold + 1)
    
    max_n_sp<-max(item_y$N_SPECIES)
    min_n_sp<-min(item_y$N_SPECIES)
    if (threshold>max_n_sp){
      midpoint<-round(max_n_sp/2)
      breakss<-c(min_n_sp, midpoint, max_n_sp)
      labelss<-c("0", "", max_n_sp)
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
      scale_color_gradient2(low  = mycol[1], high=mycol[length(mycol)],
                            mid = "#DDDDDD", midpoint=midpoint,
                            breaks=breakss, 
                            labels=labelss)+
      
      #scale_fill_gradientn(colors = mycol, values=seq(from=min_temp, to=max_temp, by=1))+
      coord_sf(crs = st_crs(crs_asia))+
      labs(colour="Species richness")+
      xlim(-12e6, 12e6)+
      ylim(-12e6, 12e6)+
      theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", linewidth = 0.5), 
            panel.background = element_rect(fill = "#FFFFFF"),
            axis.title = element_blank(),
            legend.position = "bottom")+
      guides(color = guide_colourbar(barwidth = 20, barheight = NULL,
                                     title.position = "left", title.hjust = 1)) 
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
    
    #ggsave(p, filename=sprintf("../Figures/Diversity/by_year_%s_%s/%d_%.2f.y-%d.png", 
    #                           nb, da, ccc$species_evo_type, ccc$directional_speed, y), 
    #       width=12, height=6, bg = "white")
    ggsave(p, filename=sprintf("../Figures/Diversity/by_year/%d_%.2f.y-%d.png", 
                               ccc$species_evo_type, ccc$directional_speed, y), 
            width=12, height=6, bg = "white")
  }
}


#cd /media/huijieqiao/Butterfly/Niche_Conservatism/Figures/Diversity/by_year
cmd<-(sprintf("ffmpeg -r 20 -start_number -1198 -i %d_%.2f.y%%d.png -y ../sp_richness_type_%d_ratio_%.2f.mp4",
              combinations$species_evo_type, combinations$directional_speed,
              combinations$species_evo_type, combinations$directional_speed))
write.table(cmd, "../Figures/Diversity/by_year/movies.sh", row.names = F, col.names = F, quote = F)

