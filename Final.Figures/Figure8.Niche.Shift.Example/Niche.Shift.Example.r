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
library(Rmisc)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

global_id<-27464
da<-"GOOD"
nb<-"NARROW"
base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
coms<-data.table(species_evo_type=c(2, 2, 3, 3, 4, 4, 5, 6, 7),
                 directional_speed=c(0.1, 0.5, 0.1, 0.5, 0.1, 0.5, 0.01, 0.01, 0.01))

template<-"%s/%d_%s_%s_%d_%s_0/%d_%s_%s_%d_%s_0.niche_traits.rda"
template_db<-"%s/%d_%s_%s_%d_%s_0/%d_%s_%s_%d_%s_0.sqlite"
i=3
for (i in c(1:nrow(coms))){
  com<-coms[i]
  df<-readRDS(sprintf(template, base, global_id, da, nb, com$species_evo_type, as.character(com$directional_speed),
                      global_id, da, nb, com$species_evo_type, as.character(com$directional_speed)))
  unique(df$V_L)
  df<-df[V_L %in% c("Debiased_Maximum_Monthly_Temperature", "Debiased_Maximum_Monthly_Precipitation")]
  df$VAR<-"Temperature"
  df[V_L=="Debiased_Maximum_Monthly_Precipitation"]$VAR<-"Precipitation"
  df$year<-(df$year - 1200)/10
  
  
  
  logdb<-sprintf(template_db, base, global_id, da, nb, com$species_evo_type, as.character(com$directional_speed),
                 global_id, da, nb, com$species_evo_type, as.character(com$directional_speed))
  mydb <- dbConnect(RSQLite::SQLite(), logdb)
  trees<-dbReadTable(mydb, "trees")
  dbDisconnect(mydb)
  text.string<-trees[1,2]
  text.string<-gsub("\\]", "#", gsub("\\[", "#", text.string))
  vert.tree<-read.tree(text=text.string)
  #plot(vert.tree)
  #nodelabels()
  
  g4<-ggtree(vert.tree, root.position = vert.tree$root.edge)+theme_tree2()+xlim(0, 1200)+
    theme(axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(),
          axis.line.x = element_blank())
  g4
  gg<-ggplot(df)+
    geom_ribbon(aes(x=year, ymin=V_min, ymax=V_max, fill=SP_ID), alpha=0.2)+
    geom_line(aes(x=year, y=V_mean, color=SP_ID))+
    facet_wrap(~VAR, nrow=2, scale="free")+
    labs(x="k ybp")+
    theme_bw()+
    theme(legend.position = "none", 
          axis.title.y = element_blank())
  p<-ggarrange(plotlist=list(g4, gg), nrow=2, ncol=1, heights=c(1, 2))
  if (com$species_evo_type %in% c(2:4)){
    title<-sprintf("%s (%d%%)", evo_types_label_color[com$species_evo_type], com$directional_speed * 100)
  }else{
    title<-evo_types_label_color[com$species_evo_type]
  }
  pp<-annotate_figure(p, top = text_grob(title, size = 14))
  ggsave(pp, filename=sprintf("../Figures/Figure8.Niche.Shift.Example/%d_%s.png",
                              com$species_evo_type, as.character(com$directional_speed)),
         width=8, height=10, bg="white")
}
