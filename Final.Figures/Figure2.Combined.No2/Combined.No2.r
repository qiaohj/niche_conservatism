library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
library(ggstance)
library(ggh4x)
library(PupillometryR)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

d<-readRDS("../Data/tslm_and_glm/d_ndr.rda")
d<-formatLabels(d)
item<-d[from>=-1000]
item$net_dr
colnames(item)
dim(item)
item[net_dr==-1]
range(item$R_SPECIATION_SPECIES)
item_e<-item[R_EXTINCTION_SPECIES>0]
item_e<-item
item_extinction<-item_e[, .(v=mean(R_EXTINCTION_SPECIES)/1e3,
                            label="R_EXTINCTION_SPECIES"),
                by=list(from, label_x, label_line)]
item_speciation<-item_e[, .(v=mean(R_SPECIATION_SPECIES)/1e3,
                            label="R_SPECIATION_SPECIES"),
                        by=list(from, label_x, label_line)]
item_ndr<-item_e[, .(v=mean(net_dr),
                            label="net_dr"),
                        by=list(from, label_x, label_line)]
item_sd<-rbindlist(list(item_ndr, item_speciation, item_extinction))
type.labs <- c("net_dr"= "net per capita diversification rate",
               "R_EXTINCTION_SPECIES"=  "net extinction",
               "R_SPECIATION_SPECIES" ="net speciation")
p_boxplot<-ggplot(item_sd, aes(x=v, y=label_x, fill=label_line, color=label_line))+ 
  
  #ggdist::stat_halfeye(
  #  adjust = 0.9,
  #  justification = -0.15,
  #  .width = 0,
  #  point_colour = NA) +
  geom_boxplot(
    width = 0.2,
    outlier.colour = NA,
    alpha = 0.5)+
  ggdist::stat_dots(
    side = "left",
    justification = 1.18,
    binwidth = 0.0005)+
  scale_color_manual(values=evo_type_color)+
  scale_fill_manual(values=evo_type_color)+
  scale_y_discrete(limits=rev)+
  guides(y = ggh4x::guide_axis_nested(delim = "&"))+
  theme_bw()+
  theme(legend.position = "none",
        ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
        ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
        
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(5, "mm"),
        strip.text = element_text(size=12))+
  facet_wrap(~label, scale="free_x", labeller = 
               labeller(label = type.labs), nrow=1, ncol=3)

mean_CI(item_e$R_EXTINCTION_SPECIES)
ggsave(p_boxplot, filename="../Figures/Figure2.Combined.No2/item2.png")


df_result<-readRDS("../Figures/20230616/TukeyHSD/TukeyHSD_by_species.rda")
df_result$label<-gsub("-conservatism", "", df_result$label)
df_result<-formatLabelX(df_result)
df_result<-df_result[is.na(NB) & is.na(DA)]

df_result<-df_result[type!="N_SPECIES"]
unique(df_result$type)
type.labs <- c("net_dr"= "net per capita diversification rate",
               "R_EXTINCTION_SPECIES"=  "net extinction",
               "R_SPECIATION_SPECIES" ="net speciation")

df_result[type!="net_dr"]$diff <- df_result[type!="net_dr"]$diff/1000
df_result[type!="net_dr"]$lwr <- df_result[type!="net_dr"]$lwr/1000
df_result[type!="net_dr"]$upr <- df_result[type!="net_dr"]$upr/1000

df_result$diff_str<-round(df_result$diff, 3)
df_result$alternative<-""
df_result[diff>0]$alternative<-"Greater"
df_result[diff<0]$alternative<-"Less"
df_result[p_label==""]$alternative<-"No sig diff"



table(df_result$alternative)
df_result$label<-gsub("-conservatism", "", df_result$label)
df_result$diff_label<-sprintf("%.3f %s", df_result$diff_str, df_result$p_label)
df_result<-formatLabelX (df_result)
p_tukey<-ggplot(df_result)+
  geom_errorbarh(aes(y=label_x, xmin=lwr, xmax=upr, color=alternative, width=0.2))+
  geom_vline(aes(xintercept=0), linetype=2, color="#444444")+
  geom_point(aes(y=label_x, x=diff, color=alternative), size=0.5)+
  geom_text(aes(y=label_x, x=upr, label=p_label), hjust=0.5, vjust=-0.2, size=2)+
  scale_y_discrete(limits=rev)+
  facet_wrap(~type, scale="free_x", labeller = 
               labeller(type = type.labs), nrow=1, ncol=3)+
  scale_color_manual(values=c("#EE6677", "#000000", "#4477AA"), 
                     breaks=c("Greater", "No sig diff", "Less"))+
  theme_bw()+
  guides(y = ggh4x::guide_axis_nested(delim = "&"))+
  theme(legend.position = c(0.95, 0.25),
        legend.title = element_blank(),
        legend.background = element_rect(fill=bg),
        legend.key.size = unit(5, "mm"),
        legend.text = element_text(size=7, margin = margin(t = 5)),
        legend.box.spacing = unit(1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(1.0, 'mm'),
        ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
        ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(5, "mm"),
        strip.text = element_text(size=12))

#scale_x_discrete(guide = guide_axis(n.dodge = 2))
p_tukey

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
    labs(title=names(evo_type_line)[i])+
    scale_x_continuous(breaks=range(shadows[[i]]$x), labels=c("120kyr", "0kyr"))
  if (i<=5){
    p3<-p3+theme(axis.title.y = element_blank(),
                 axis.title.x=element_blank(),
                 axis.text = element_blank(),
                 #panel.grid.major = element_blank(), 
                 #panel.grid.minor = element_blank(),
                 panel.border = element_rect(color="grey", fill=NA),
                 panel.background = element_blank(),
                 plot.title = element_text(hjust = 0.5, size=9),
                 axis.ticks.y = element_blank()
    )
  }else{
    p3<-p3+theme(axis.title.y = element_blank(),
                 axis.title.x=element_blank(),
                 axis.text.y = element_blank(),
                 #panel.grid.major = element_blank(), 
                 #panel.grid.minor = element_blank(),
                 panel.border = element_rect(color="grey", fill=NA),
                 panel.background = element_blank(),
                 plot.title = element_text(hjust = 0.5, size=9),
                 axis.ticks.y = element_blank()
    )
  }
    
  
  plist[[length(plist)+1]]<-p1
  plist2[[length(plist2)+1]]<-p2
  plist_g2[[length(plist_g2)+1]]<-p3
}

#p<-ggarrange(plotlist=c(plist, plist2, plist_g2), nrow=3, ncol=10)

p_symbol<-ggarrange(plotlist=plist_g2, nrow=2, ncol=5)
p_symbol<-annotate_figure(p_symbol,
                left = text_grob("niche change", 
                                size = 10, face = "bold", rot=90))


p_all<-ggarrange(plotlist=list(p_symbol, p_boxplot, p_tukey), 
                 nrow=3, ncol=1, heights = c(0.8, 1.2, 1),
                 labels=c("a", "b", "c"))

ggsave(p_all, filename="../Figures/Figure2.Combined.No2/Figure2.combined.No2.pdf",
       width=12.5, height=9)

