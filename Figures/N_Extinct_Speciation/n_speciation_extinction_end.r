library(ggplot2)
library(ggthemes)
library(data.table)
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
}

d<-readRDS("../Data/N_speciation_extinction.rda")

cols<-c("nb", "da", "global_id", "species_evo_type", "directional_speed", "species_evo_level")
d[, min_year:=min(year), by=cols]

d_unique<-unique(d[, ..cols])
d_end<-d[year==0]
range(d_end$N_SPECIES)
d_end_se<-d_end[, .(N=.N, N_SPECIES=sum(N_SPECIES), SD_N_SPECIES=sd(N_SPECIES)),
                by=list(nb, da, species_evo_level, species_evo_type, directional_speed)]
d_end_se$label<-paste(d_end_se$nb, d_end_se$da)
d_end_se$label2<-paste(d_end_se$species_evo_type, d_end_se$directional_speed)

ggplot(d_end_se[species_evo_level==0])+geom_bar(aes(x=label2, y=N_SPECIES, fill=label), 
                                                stat = "identity", position = position_dodge(0.9))+
  scale_fill_colorblind()
d_last<-d[year==min_year]
d_last[min_year>0&nb=="BROAD"&da=="GOOD"]

test<-d[nb=="NARROW"&da=="POOR"&global_id==57715&species_evo_type==7&directional_speed==0.01]
all_df[!(global_id %in% d_unique$global_id)]
