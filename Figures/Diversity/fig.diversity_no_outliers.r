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

world <- ne_countries(scale = "small", returnclass = "sf")


crs_asia<-"+proj=laea +lat_0=30 +lon_0=90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"
crs_america<-"+proj=laea +lat_0=30 +lon_0=-90 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs"

year<-0
#diversity[N_SPECIES==max(diversity$N_SPECIES)]

if (F){
  n_species<-readRDS("../Data/N_speciation_extinction.rda")
  if (F){
    n_species_final<-n_species[year==0 & species_evo_level==0 & species_evo_type %in% c(1,2,3,4)]
    threshold<-floor(mean(n_species_final$N_SPECIES) + 3 * sd(n_species_final$N_SPECIES))
    threshold<-2000
    n_species_final$is_outlier<-F
    n_species_final[N_SPECIES>threshold]$is_outlier<-T
  }
  if (F){
    rm("threshold")
    n_species_final<-n_species[year==0 & species_evo_level==0 & species_evo_type %in% c(1,2,3,4)]
    thresholds<-n_species_final[species_evo_type %in% c(1,2,3,4), 
                                .(threshold=floor(mean(N_SPECIES) + 3 * sd(N_SPECIES))),
                                by=list(nb, da, species_evo_type, directional_speed)]
    n_species_final<-merge(n_species_final, thresholds, 
                           by=c("nb", "da", "species_evo_type", "directional_speed"))
    n_species_final$is_outlier<-F
    n_species_final[N_SPECIES>threshold]$is_outlier<-T
  }
  n_species_final[, .(N=.N), by=list(nb, da, species_evo_type, directional_speed, is_outlier)]
  
  seeds<-unique(n_species_final$global_id)
  outliers<-unique(n_species_final[is_outlier==T]$global_id)
  saveRDS(outliers, "../Data/outliers.rda")
  polygon_outlier<-polygon[which(polygon$Name %in% outliers),]
  polygon_outlier<-merge(polygon_outlier, n_species_final[is_outlier==T], , by.x="Name", by.y="global_id")
  st_write(polygon_outlier, "../Shape/outliers/outliers.kml", append=FALSE)
  polygon_outlier[which(polygon_outlier$Name==1518),]
}



if (F){
  diversity<-readRDS("../Data/diversity.rda")
  cols<-c("evo_type", "species_evo_type",
          "directional_speed", "species_evo_level")
  combinations<-unique(diversity[, ..cols])
  outliers<-readRDS("../Data/outliers.rda")
  nb<-"BROAD"
  da<-"GOOD"
  seed<-outliers[1]
  i=5
  diversity_outliers<-list()
  for (seed in outliers){
    print(seed)
    for (nb in c("BROAD", "NARROW")){
      for (da in c("GOOD", "POOR")){
        for (i in c(1:nrow(combinations))){
          
          ccc<-combinations[i]
          diversity_item<-readRDS(sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%d_%s_%s_%d_%s_0/%d_%s_%s_%d_%s_0.diversity.rda",
                                          seed, da, nb, ccc$species_evo_type, as.character(ccc$directional_speed),
                                          seed, da, nb, ccc$species_evo_type, as.character(ccc$directional_speed)))
          diversity_item$species_evo_type<-ccc$species_evo_type
          diversity_item$directional_speed<-ccc$directional_speed
          diversity_item$species_evo_level<-ccc$species_evo_level
          diversity_outliers[[length(diversity_outliers)+1]]<-diversity_item
        }
      }
    }
  }
  diversity_outliers1<-rbindlist(diversity_outliers[1:1000])
  diversity_outliers_se1<-diversity_outliers1[, .(N_SPECIES=sum(N_SPECIES),
                                                N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                            by=list(year, global_id, species_evo_type,
                                                    directional_speed, species_evo_level)]
  
  diversity_outliers2<-rbindlist(diversity_outliers[1001:length(diversity_outliers)])
  diversity_outliers_se2<-diversity_outliers2[, .(N_SPECIES=sum(N_SPECIES),
                                                  N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                              by=list(year, global_id, species_evo_type,
                                                      directional_speed, species_evo_level)]
  
  diversity_outliers_se<-rbindlist(list(diversity_outliers_se1, diversity_outliers_se2))
  diversity_outliers_se<-diversity_outliers_se[, .(N_SPECIES=sum(N_SPECIES),
                                                   N_INDIVIDUAL=sum(N_INDIVIDUAL)),
                                               by=list(year, global_id, species_evo_type,
                                                       directional_speed, species_evo_level)]
  saveRDS(diversity_outliers_se, "../Data/outlier.diversity.rda")
  
  
  colnames(diversity_outliers_se)[c(6,7)]<-c("N_SPECIES_OUTLIER", "N_INDIVIDUAL_OUTLIER")
  diversity_with_outlier<-merge(diversity, diversity_outliers_se,
                                by=c("year", "global_id", "species_evo_type",
                                     "directional_speed", "species_evo_level"),
                                all=T)
  diversity_with_outlier[is.na(N_SPECIES_OUTLIER)]$N_SPECIES_OUTLIER<-0
  diversity_with_outlier[is.na(N_INDIVIDUAL_OUTLIER)]$N_INDIVIDUAL_OUTLIER<-0
  diversity_with_outlier$N_SPECIES<-diversity_with_outlier$N_SPECIES - diversity_with_outlier$N_SPECIES_OUTLIER
  diversity_with_outlier$N_INDIVIDUAL<-diversity_with_outlier$N_INDIVIDUAL - diversity_with_outlier$N_INDIVIDUAL_OUTLIER
  dim(diversity_with_outlier)
  dim(diversity)
  diversity_with_outlier[N_SPECIES<0]
  diversity_with_outlier<-diversity_with_outlier[N_SPECIES>0]
  diversity_with_outlier[is.na(N_SPECIES)]
  saveRDS(diversity_with_outlier, "../Data/diversity_without_outliers.rda")
}
diversity<-readRDS("../Data/diversity_without_outliers.rda")
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
      theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
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
      theme(panel.grid.major = element_line(color = "#d4d4d4", linetype = "dashed", size = 0.5), 
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
    ggsave(p, filename=sprintf("../Figures/Diversity/by_year_without_outliers/%d_%.2f.y-%d.png", 
                               ccc$species_evo_type, ccc$directional_speed, y), 
           width=12, height=6, bg = "white")
  }
}



