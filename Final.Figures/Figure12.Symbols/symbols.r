library(ggplot2)
library(data.table)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

env_curve<-readRDS("../Figures/Figure1.Overview/Data/env_yearly_avg.rda")
env_curve<-env_curve[var=="Debiased_Minimum_Monthly_Temperature"]
env_curve<-env_curve[year %in% c(seq(1, 1200, by=10)) ]

range<-0.3

minv<-min(env_curve$mean_v)
maxv<-max(env_curve$mean_v)
ratio<-(range * 2) / (maxv - minv)
env_curve$fixed_mean_v<-((env_curve$mean_v - minv) * ratio - range) * 0.7 - 0.1

env_curve$fixed_year<-((1200 - env_curve$year) * 3/1200 - 1.5)
points<-data.frame(x=c(-2, -1, 1, 2, 1, -1, -2),
                   y=c(0, sqrt(3) * -1, sqrt(3) * -1, 0, sqrt(3), sqrt(3), 0))

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circle<-circleFun(diameter=3.5)
line1<-data.frame(x=c(-1.5, 1.5), y=c(0, 0))
shadow1<-data.frame(ymin=line1$y-range * 2, ymax=line1$y+range* 2, x=line1$x)

species_evo_types<-data.table(species_evo_type=c(2,2,5,6,7,3,3,4,4),
                              directional_speed=c(0.1,0.5,
                                                  0.01,0.01,0.01,
                                                  0.1,0.5,
                                                  0.1,0.5))

lines<-list(line1)
shadows<-list(shadow1)
i=2
for (i in c(1:nrow(species_evo_types))){

  nb<-readRDS(sprintf(
    "/media/huijieqiao/QNAS/Niche_Conservatism/Results/27464_GOOD_NARROW_%d_%s_0/27464_GOOD_NARROW_%d_%s_0.niche_traits.rda",
    species_evo_types[i]$species_evo_type, as.character(species_evo_types[i]$directional_speed),
    species_evo_types[i]$species_evo_type, as.character(species_evo_types[i]$directional_speed)))
  
  nb<-nb[,.(V_min=mean(V_min),
            V_max=mean(V_max),
            V_mean=mean(V_mean),
            V_range=mean(V_range)),
         by=list(year, V_L)]
  nb<-nb[V_L=="Debiased_Minimum_Monthly_Temperature"]
  nb<-nb[year %in% c(seq(1, 1200, by=10)) ]
  
  minv<-min(nb$V_mean)
  maxv<-max(nb$V_mean)
  ratio<-(range * 2) / (maxv - minv)
  if (species_evo_types[i]$species_evo_type==2){
    nb$fixed_V_mean<-((round(nb$V_mean - minv, 3) * ratio) - range )* 0.7 * 
      species_evo_types[i]$directional_speed * 2
    range(nb$V_mean)
    range(nb$fixed_V_mean)
    offside<-ifelse(species_evo_types[i]$directional_speed==0.1, 1.5, 1)
    nb$fixed_V_min<-((round(nb$V_min - minv, 3) * ratio ) - range) * 0.7 * 
      species_evo_types[i]$directional_speed * 2 + offside
    range(nb$fixed_V_min)
    
    nb$fixed_V_max<-((round(nb$V_max - minv, 3) * ratio) - range) * 0.7 * 
      species_evo_types[i]$directional_speed * 2 - offside
    range(nb$fixed_V_max)
  }
  
  if (species_evo_types[i]$species_evo_type==3){
    nb$fixed_V_mean<-((round(nb$V_mean - minv, 3) * ratio) - range )* 0.7 * 
      species_evo_types[i]$directional_speed * 2
    range(nb$V_mean)
    range(nb$fixed_V_mean)
    offside<-ifelse(species_evo_types[i]$directional_speed==0.1, 1.5, 4)
    nb$fixed_V_min<-((round(nb$V_min - minv, 3) * ratio ) - range) * 0.7 * 
      species_evo_types[i]$directional_speed * 2 + offside
    nb$fixed_V_min<-nb$fixed_V_min/3
    range(nb$fixed_V_min)
    
    nb$fixed_V_max<-((round(nb$V_max - minv, 3) * ratio) - range) * 0.7 * 
      species_evo_types[i]$directional_speed * 2 - offside
    nb$fixed_V_max<-nb$fixed_V_max/3
    range(nb$fixed_V_max)
  }
  if (species_evo_types[i]$species_evo_type==4){
    ratio=1
    nb$fixed_V_mean<-0
    range(nb$V_mean)
    range(nb$fixed_V_mean)
    offside<-ifelse(species_evo_types[i]$directional_speed==0.1, 1.5, 8)
    zoom<-ifelse(species_evo_types[i]$directional_speed==0.1, 3, 25)
    nb$fixed_V_min<-((round(nb$V_min - minv, 3) * ratio ) - range) * 0.7 * 
      species_evo_types[i]$directional_speed * 2 + offside
    nb$fixed_V_min<-nb$fixed_V_min/zoom
    range(nb$fixed_V_min)
    
    nb$fixed_V_max<-((round(nb$V_max - minv, 3) * ratio) - range) * 0.7 * 
      species_evo_types[i]$directional_speed * 2 - offside
    nb$fixed_V_max<-nb$fixed_V_max/zoom
    range(nb$fixed_V_max)
  }
  
  if (species_evo_types[i]$species_evo_type==5){
    nb$fixed_V_mean<-((round(nb$V_mean - minv, 3) * ratio) - range )* 0.7 
    range(nb$V_mean)
    range(nb$fixed_V_mean)
    offside<-0.5
    nb$fixed_V_min<-((round(nb$V_min - minv, 3) * ratio ) - range) * 0.7  + offside
    range(nb$fixed_V_min)
    
    nb$fixed_V_max<-((round(nb$V_max - minv, 3) * ratio) - range) * 0.7  - offside
    range(nb$fixed_V_max)
  }
  
  if (species_evo_types[i]$species_evo_type==6){
    ratio=1
    nb$fixed_V_mean<-0
    range(nb$V_mean)
    range(nb$fixed_V_mean)
    offside<-21
    zoom<-8
    nb$fixed_V_min<-((round(nb$V_min - minv, 3) * ratio ) - range) * 0.7  + offside
    nb$fixed_V_min<-nb$fixed_V_min/zoom
    range(nb$fixed_V_min)
    
    nb$fixed_V_max<-((round(nb$V_max - minv, 3) * ratio) - range) * 0.7  - offside
    nb$fixed_V_max<-nb$fixed_V_max/zoom
    range(nb$fixed_V_max)
  }
  
  if (species_evo_types[i]$species_evo_type==7){
    #ratio=1
    nb$fixed_V_mean<-((round(nb$V_mean - minv, 3) * ratio) - range )* 0.7 
    range(nb$V_mean)
    range(nb$fixed_V_mean)
    offside<-0
    zoom<-2
    nb$fixed_V_min<-((round(nb$V_min - minv, 3) * ratio ) - range) * 0.7  + offside
    nb$fixed_V_min<-nb$fixed_V_min/zoom
    range(nb$fixed_V_min)
    
    nb$fixed_V_max<-((round(nb$V_max - minv, 3) * ratio) - range) * 0.7  - offside
    nb$fixed_V_max<-nb$fixed_V_max/zoom
    range(nb$fixed_V_max)
  }
  
  
  nb$fixed_year<--1 * ((1200 - nb$year) * 3/1200 - 1.5)
  
  line2_1<-data.frame(x=nb$fixed_year, y=nb$fixed_V_mean)
  shadow2_1<-data.frame(ymin=nb$fixed_V_min, ymax=nb$fixed_V_max, x=nb$fixed_year)
  
  if (F){
    print(ggplot()+
      geom_polygon(data=points, aes(x=x, y=y), fill=type<-evo_type_color[i+1])+
      geom_ribbon(data=shadow2_1, aes(x=x, ymin=ymin, ymax=ymax), fill="white")+
      geom_path(data=line2_1, aes(x=x, y=y), color=type<-evo_type_color[i+1], linewidth=0.5)+
      geom_line(data=env_curve, aes(x=fixed_year, y=fixed_mean_v), linetype=2, linewidth=0.5, color="#CC3311")+
      ggtitle(sprintf("%d %s", 
                      species_evo_types[i]$species_evo_type, 
                      species_evo_types[i]$directional_speed)))
  }
  
  lines[[length(lines)+1]]<-line2_1
  shadows[[length(shadows)+1]]<-shadow2_1
  
  
}

plist<-list()
plist2<-list()
plist_g2<-list()
i=1
for (i in c(1:length(lines))){
  
  type<-evo_type_color[i]
  p1<-ggplot()+
    
    geom_polygon(data=circle, aes(x=x, y=y), fill=type)+
    geom_ribbon(data=shadows[[i]], aes(x=x, ymin=ymin, ymax=ymax), fill="white")+
    geom_path(data=lines[[i]], aes(x=x, y=y), color=type, linewidth=0.5)+
    geom_line(data=env_curve, aes(x=fixed_year, y=fixed_mean_v), linewidth=0.5, color="#CC3311")+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank())
  p2<-ggplot()+
    
    geom_polygon(data=points, aes(x=x, y=y), fill=type)+
    geom_ribbon(data=shadows[[i]], aes(x=x, ymin=ymin, ymax=ymax), fill="white")+
    geom_path(data=lines[[i]], aes(x=x, y=y), color=type, linewidth=0.5)+
    geom_line(data=env_curve, aes(x=fixed_year, y=fixed_mean_v), linewidth=0.5, color="#CC3311")+
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank())
  
  p3<-ggplot()+
    geom_ribbon(data=shadows[[i]], aes(x=x, ymin=ymin, ymax=ymax), fill=type, alpha=0.5)+
    geom_path(data=lines[[i]], aes(x=x, y=y), color=type, linewidth=0.5)+
    geom_line(data=env_curve, aes(x=fixed_year, y=fixed_mean_v), linewidth=0.5, color="white")+
    theme(#axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank())
  
  plist[[length(plist)+1]]<-p1
  plist2[[length(plist2)+1]]<-p2
  plist_g2[[length(plist_g2)+1]]<-p3
}

p<-ggarrange(plotlist=c(plist, plist2, plist_g2), nrow=3, ncol=10)
saveRDS(plist_g2, "../Figures/Figure12.symbols/type3.rda")
ggsave(p, filename="../Figures/Figure12.symbols/symbol.pdf", width=30, height=6)

