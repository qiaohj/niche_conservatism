library(ggplot2)
library(ggthemes)
library(data.table)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
if (F){
  d1<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_BROAD_GOOD_0.rda")
  d1$species_evo_level<-0
  
  d2<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_BROAD_POOR_0.rda")
  d2$species_evo_level<-0
  
  d3<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_GOOD_0.rda")
  d3$species_evo_level<-0
  
  d4<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_POOR_0.rda")
  d4$species_evo_level<-0
  
  d5<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_GOOD_1.rda")
  d5$species_evo_level<-1
  
  d6<-readRDS("../Data/N_speciation_extinction_items/N_speciation_extinction_NARROW_POOR_1.rda")
  d6$species_evo_level<-1
  
  d<-rbindlist(list(d1, d2, d3, d4, d5, d6))
  saveRDS(d, "../Data/N_speciation_extinction.rda")
  
  
  d1<-readRDS("../Data/distribution_traits/distribution_traits_BROAD_GOOD_0.rda")
  d1$species_evo_level<-0
  
  d2<-readRDS("../Data/distribution_traits/distribution_traits_BROAD_POOR_0.rda")
  d2$species_evo_level<-0
  colnames(d2)[17]<-"global_id"
  d3<-readRDS("../Data/distribution_traits/distribution_traits_NARROW_GOOD_0.rda")
  d3$species_evo_level<-0
  
  d4<-readRDS("../Data/distribution_traits/distribution_traits_NARROW_POOR_0.rda")
  d4$species_evo_level<-0
  
  d5<-readRDS("../Data/distribution_traits/distribution_traits_NARROW_GOOD_1.rda")
  d5$species_evo_level<-1
  
  d6<-readRDS("../Data/distribution_traits/distribution_traits_NARROW_POOR_1.rda")
  d6$species_evo_level<-1
  
  d<-rbindlist(list(d1, d2, d3, d4, d5, d6))
  
  saveRDS(d, "../Data/distribution_traits.rda")
  
}
if (F){
  d<-readRDS("../Data/N_speciation_extinction.rda")
  
  #cols<-c("nb", "da", "global_id", "species_evo_type", "directional_speed", "species_evo_level")
  #d[, min_year:=min(year), by=cols]
  
  #d_unique<-unique(d[, ..cols])
  d_end<-d[year==0]
  range(d_end$N_SPECIES)
  d_se<-d[, .(N=.N, N_SPECIES=sum(N_SPECIES), SD_N_SPECIES=sd(N_SPECIES)),
          by=list(nb, da, species_evo_level, species_evo_type, directional_speed, year)]
  
  d_quantile<-d[year==0, .(N_99=quantile(N_SPECIES, 0.99)),
                by=list(nb, da, species_evo_level, species_evo_type, directional_speed)]
  d_with_quantile<-merge(d_quantile, d_end, by=c("nb", "da", "species_evo_level", "species_evo_type", "directional_speed"))
  outliers<-d_with_quantile[N_SPECIATION>N_99]
  
  d_se_without_ourliers<-d[!(global_id %in% outliers$global_id), .(N=.N, N_SPECIES=sum(N_SPECIES), SD_N_SPECIES=sd(N_SPECIES)),
                           by=list(nb, da, species_evo_level, species_evo_type, directional_speed, year)]
  
  d_se_without_ourliers$label<-paste(d_se_without_ourliers$nb, d_se_without_ourliers$da)
  d_se_without_ourliers$label2<-paste(d_se_without_ourliers$species_evo_type, d_se_without_ourliers$directional_speed)
  
  p<-ggplot(d_se_without_ourliers[species_evo_level==0])+geom_bar(aes(x=label2, y=N_SPECIES, fill=label), 
                                                                  stat = "identity", position = position_dodge(0.9))+
    scale_fill_colorblind()
  ggsave(p, filename="../Figures/N_Species/N_Species_end_no_outliers.png", width=12, height=10)
  d$R_SPECIATION_SPECIES<-d$N_SPECIATION_YEAR/d$N_SPECIES
  d[is.nan(R_SPECIATION_SPECIES)]$R_SPECIATION_SPECIES<-0
  d$R_EXTINCTION_SPECIES<-d$N_EXTINCTION_YEAR/d$N_SPECIES
  d[is.nan(R_EXTINCTION_SPECIES)]$R_EXTINCTION_SPECIES<-0
  
  d_se<-d[, .(N=.N, 
              N_SPECIES=sum(N_SPECIES), 
              N_SPECIATION=sum(N_SPECIATION),
              N_EXTINCTION=sum(N_EXTINCTION),
              N_SPECIATION_YEAR=sum(N_SPECIATION_YEAR),
              N_EXTINCTION_YEAR=sum(N_EXTINCTION_YEAR),
              R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES, na.rm=T),
              R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES, na.rm=T),
              SD_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES, na.rm=T),
              SD_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES, na.rm=T)
  ),
  by=list(nb, da, species_evo_level, species_evo_type, directional_speed, year)]
  
  d_se_seed<-d[N_SPECIES>0, .(N_SEED=length(unique(global_id))),
    by=list(nb, da, species_evo_level, species_evo_type, directional_speed, year)]
  
  range(d$N_SPECIES)
  d_se$label<-paste(d_se$nb, d_se$da)
  d_se$label2<-paste(d_se$species_evo_type, d_se$directional_speed)
  
  d_se_seed$label<-paste(d_se_seed$nb, d_se_seed$da)
  d_se_seed$label2<-paste(d_se_seed$species_evo_type, d_se_seed$directional_speed)
  
  saveRDS(d_se, "../Figures/N_Species/figure_data.rda")
  saveRDS(d_se_seed, "../Figures/N_Species/d_se_seed.rda")
}



d_se<-readRDS("../Figures/N_Species/figure_data.rda")
d_se_seed<-readRDS("../Figures/N_Species/d_se_seed.rda")
source("commons/functions.r")
#1: conservatism
#2: shift-directional
#3: expansion-directional
#4: expansion-omnidirectional
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
d_se<-d_se[species_evo_level==0]
d_se$evo_type<-format_evoType(d_se$species_evo_type)

d_se_all<-d_se[, .(N_SPECIES=sum(N_SPECIES),
                   N_SPECIATION=sum(N_SPECIATION),
                   N_EXTINCTION=sum(N_EXTINCTION)),
               by=list(species_evo_level, species_evo_type, directional_speed, year, evo_type)]
cor(d_se$N_SPECIES, d_se$N_SPECIATION)

cols<-c("N_SPECIES", "species_evo_level", "species_evo_type", "directional_speed", "year", "evo_type")
d_se_all_fig1<-d_se_all[, ..cols]
colnames(d_se_all_fig1)[1]<-"N"
d_se_all_fig1$type<-"Number of species"

cols<-c("N_SPECIATION", "species_evo_level", "species_evo_type", "directional_speed", "year", "evo_type")
d_se_all_fig2<-d_se_all[, ..cols]
colnames(d_se_all_fig2)[1]<-"N"
d_se_all_fig2$type<-"Number of speciation"

cols<-c("N_EXTINCTION", "species_evo_level", "species_evo_type", "directional_speed", "year", "evo_type")
d_se_all_fig3<-d_se_all[, ..cols]
colnames(d_se_all_fig3)[1]<-"N"
d_se_all_fig3$type<-"Number of extinction"
d_se_all_fig<-rbindlist(list(d_se_all_fig1, d_se_all_fig2, d_se_all_fig3))
d_se_all_fig$type<-factor(d_se_all_fig$type, 
                          levels=rev(c("Number of species", "Number of speciation", "Number of extinction")))
d_se_all_fig$label<-format_evoType_amplitude(d_se_all_fig$evo_type, d_se_all_fig$directional_speed, order=-1)


p<-ggplot(d_se_all_fig[year==0])+
  geom_bar(aes(y=label, x=N, fill=type), position="dodge2", stat = "identity")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0))+
  labs(fill="", y="")+
  scale_fill_colorblind()
p
ggsave("../Figures/N_Species/n_species_speciation_extinction.png", width=8, height=6)

d_se$label2<-format_evoType_amplitude(d_se$evo_type, d_se$directional_speed, order=-1)
#d_se$label<-factor(d_se$label, levels = c("B"))
p<-ggplot(d_se[year==0&species_evo_level==0])+
  geom_bar(aes(y=label2, x=N_SPECIES, fill=label), 
           stat = "identity", position = position_dodge(0.9))+
  labs(x="", y="Number of species", fill="niche breadth & dispersal ability")+
  scale_fill_colorblind()+
  
  theme_bw()

p
ggsave(p, filename="../Figures/N_Species/N_Species_end.png", width=8, height=6)

p<-ggplot(d_se[year<1198&species_evo_level==0])+
  geom_line(aes(x=year*-0.1, y=N_SPECIES, color=evo_type, linetype=factor(directional_speed)))+
  labs(x="X k years before present", y="Number of species", color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  facet_grid(nb~da, scale="free")+
  scale_y_log10()

p


ggsave(p, filename="../Figures/N_Species/N_Species.png", width=12, height=6)

p<-ggplot(d_se_seed)+
  geom_line(aes(x=year*-1, y=N_SEED, color=factor(species_evo_type), 
                linetype=factor(directional_speed)))+
  theme_bw()+
  scale_fill_colorblind()+
  facet_wrap(~label, scale="free")

p
ggsave(p, filename="../Figures/N_Species/N_Seed.png", width=12, height=10)

p<-ggplot(d_se[year<1198&species_evo_level==0])+
  geom_line(aes(x=year*-0.1, y=N_SPECIATION, color=evo_type, linetype=factor(directional_speed)))+
  labs(x="X k years before present", y="Number of speciations", color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  facet_grid(nb~da, scale="free")+
  scale_y_log10()

p
ggsave(p, filename="../Figures/N_Species/N_SPECIATION.png", width=12, height=6)

p<-ggplot(d_se[year<1198&species_evo_level==0])+
  geom_line(aes(x=year*-0.1, y=N_EXTINCTION, color=evo_type, linetype=factor(directional_speed)))+
  labs(x="X k years before present", y="Number of extinctions", color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  facet_grid(nb~da, scale="free")+
  scale_y_log10()


p


ggsave(p, filename="../Figures/N_Species/N_EXTINCTION.png", width=12, height=6)


p<-ggplot(d_se[year<1198&species_evo_level==0])+
  geom_line(aes(x=year*-0.1, y=N_EXTINCTION * 1000 / (N_SPECIES + N_EXTINCTION), color=evo_type, linetype=factor(directional_speed)))+
  labs(x="X k years before present", y="Number of extinctions per 1k species", color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  facet_grid(nb~da, scale="free")

#d_se[year<1198&species_evo_level==0&N_EXTINCTION>N_SPECIES]
p


ggsave(p, filename="../Figures/N_Species/N_EXTINCTION_Rate.png", width=12, height=6)

p<-ggplot(d_se[year<1198&species_evo_level==0])+
  geom_line(aes(x=year*-0.1, y=N_SPECIATION * 1000 / (N_SPECIES + N_EXTINCTION), 
                color=evo_type, linetype=factor(directional_speed)))+
  labs(x="X k years before present", y="Number of speciations per 1k species", color="Evolution type", linetype="Evolution rate")+
  theme_bw()+
  scale_color_colorblind()+
  facet_grid(nb~da, scale="free")

#d_se[year<1198&species_evo_level==0&N_EXTINCTION>N_SPECIES]
p


ggsave(p, filename="../Figures/N_Species/N_SPECIATION_Rate.png", width=12, height=6)

d_se$label3<-format_evoType_amplitude(d_se$evo_type, d_se$directional_speed, order=1)
p<-ggplot(d_se[year==0&species_evo_level==0])+
  geom_point(aes(x=label3, y=N_SPECIATION * 1000 / (N_SPECIES + N_EXTINCTION), color=label), 
             position = position_dodge2(0.1))+
  labs(x="Evolution type", y="Number of speciations per 1k species", 
       color="niche breadth & dispersal ability")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0))+
  scale_color_colorblind()


#d_se[year<1198&species_evo_level==0&N_EXTINCTION>N_SPECIES]
p


ggsave(p, filename="../Figures/N_Species/N_SPECIATION_Rate_end.png", width=12, height=6)

p<-ggplot(d_se[year==0&species_evo_level==0])+
  geom_point(aes(x=label3, y=N_EXTINCTION * 1000 / (N_SPECIES + N_EXTINCTION), color=label), 
             position = position_dodge2(0.1))+
  labs(x="Evolution type", y="Number of extionctions per 1k species", 
       color="niche breadth & dispersal ability")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=-90, hjust=0, vjust=0))+
  scale_color_colorblind()


#d_se[year<1198&species_evo_level==0&N_EXTINCTION>N_SPECIES]
p


ggsave(p, filename="../Figures/N_Species/N_EXTINCTION_Rate_end.png", width=12, height=6)




p<-ggplot(d_se[year<1198&species_evo_level==0])+
  geom_line(aes(x=year*-1, y=N_SPECIATION_YEAR, 
                color=factor(species_evo_type), linetype=factor(directional_speed)))+
  theme_bw()+
  scale_fill_colorblind()+
  facet_wrap(~label, scale="free")

p
ggsave(p, filename="../Figures/N_Species/N_SPECIATION_YEAR.png", width=12, height=10)

p<-ggplot(d_se[year<1198&species_evo_level==0])+
  geom_line(aes(x=year*-1, y=N_EXTINCTION_YEAR, color=factor(species_evo_type), linetype=factor(directional_speed)))+
  theme_bw()+
  scale_fill_colorblind()+
  facet_wrap(~label, scale="free")

p
ggsave(p, filename="../Figures/N_Species/N_EXTINCTION_YEAR.png", width=12, height=10)

p<-ggplot(d_se[year<1000&species_evo_level==0])+
  geom_boxplot(aes(x=label2, y=R_SPECIATION_SPECIES*100, fill=label))+
  theme_bw()+
  scale_fill_colorblind()+
  scale_y_log10()

p
ggsave(p, filename="../Figures/N_Species/R_SPECIATION_SPECIES.png", width=12, height=10)

p<-ggplot(d_se[year<1000&species_evo_level==0])+
  geom_boxplot(aes(x=label2, y=R_EXTINCTION_SPECIES*100, fill=label))+
  theme_bw()+
  scale_fill_colorblind()+
  scale_y_log10()

p
ggsave(p, filename="../Figures/N_Species/R_EXTINCTION_SPECIES.png", width=12, height=10)



