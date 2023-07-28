library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
library(ggh4x)
library(ggdist)
library(tidyquant)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
if (F){
  d<-readRDS("../Data/niche_traits/niche_traits_fn.rda")
  outliers<-readRDS("../Data/outliers/outliers_3SD.rda")
  d<-d[!(global_id %in% unique(outliers$global_id))]
  d<-d[((directional_speed %in% c(0) & species_evo_type==1) |
          (directional_speed %in% c(0.1, 0.5) & species_evo_type %in% c(2, 3, 4)) |
          (directional_speed %in% c(0.01) & species_evo_type %in% c(5, 6, 7)))]
}
d<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data_raw.rda")
d<-formatLabels(d)
d$N_EXTINCTION_DIFF<-d$N_EXTINCTION - d$N_EXTINCTION_null
d<-d[species_evo_type!=1]
hist(d$N_EXTINCTION_DIFF)
d$N_SPECIATION_DIFF<-d$N_SPECIATION - d$N_SPECIATION_null
hist(d$N_SPECIATION_DIFF)


d_se_all<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data.rda")
d_se_all<-formatLabels(d_se_all)
d_se_all[is.na(nb)]$nb<-"ALL"
d_se_all[is.na(da)]$da<-"ALL"
d_se_all<-d_se_all[nb=="ALL" & da=="ALL"]

d_n<-d[, .(N=.N), by=list(N_SPECIATION, N_EXTINCTION, label_line, species_evo_type, directional_speed)]
range(d$year)
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

d$density <- get_density(d$N_SPECIATION_DIFF, d$N_EXTINCTION_DIFF, h = c(10, 1), n = 100)
ggplot(d) + geom_point(aes(N_SPECIATION_DIFF, N_EXTINCTION_DIFF, color = density)) + scale_color_viridis()
ggplot(d[N_SPECIATION>1], aes(x=log(N_SPECIATION))) + geom_density()

ggplot(d, aes(x=N_SPECIATION_DIFF, y=N_EXTINCTION_DIFF, color=label_line))+
  #geom_point()+
  #stat_ellipse()+
  geom_density_2d(h=c(1, 1))+
  xlim(-10, 10)+
  ylim(-10, 10)+
  facet_wrap(~label_line, nrow=5, ncol=2)

d[N_SPECIATION>0 & N_EXTINCTION>0][, .(max_e=max(N_EXTINCTION),
      min_e=min(N_EXTINCTION),
      max_p=max(N_SPECIATION),
      min_p=min(N_SPECIATION),
      N=.N),
  by=list(label)]
