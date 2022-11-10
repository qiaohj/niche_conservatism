library(data.table)
library(ggplot2)
library(ggthemes)
library(RSQLite)
library(DBI)
library(sf)
setDTthreads(20)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")

if (F){
  df<-readRDS("../Data/N_speciation_extinction.rda")
  
  cols<-c("species_evo_type", "directional_speed", "species_evo_level")
  groups<-unique(df[, ..cols])
  groups<-groups[species_evo_level==0]
  i=1
  j=4
  format_p_value<-function(v){
    if (v>0.05){
      v_str<-as.character(round(v, 2))
    }
    if (v<=0.05 & v>0.01){
      v_str<-sprintf("%s*", as.character(round(v, 2)))
      v_str<-""
    }
    if (v<=0.01 & v>0.001){
      v_str<-sprintf("%s**", as.character(round(v, 3)))
      v_str<-""
    }
    if (v<=0.001){
      v_str<-"<0.001***"
      v_str<-""
    }
    v_str
  }
  
  t.test.result<-list()
  for (i in c(1:(nrow(groups)))){
    v1<-df[year==0 & species_evo_level==0 & 
             species_evo_type==groups[i]$species_evo_type & 
             directional_speed==groups[i]$directional_speed]
    for (j in c(1:(nrow(groups)))){
      if (i==j){
        next()
      }
      v2<-df[year==0 & species_evo_level==0 & 
               species_evo_type==groups[j]$species_evo_type & 
               directional_speed==groups[j]$directional_speed]
      v<-merge(v1, v2, by=c("nb", "da", "global_id", "year"), all=T)
      #N Species
      tt<-t.test(v$N_SPECIES.x, v$N_SPECIES.y, alternative="two.sided", paired=T)
      t.test<-"no differences"
      if (tt$p.value<=0.05){
        tt<-t.test(v$N_SPECIES.x, v$N_SPECIES.y, alternative="less", paired=T)
        if (tt$p.value<=0.05){
          t.test<-"less"
        }else{
          tt<-t.test(v$N_SPECIES.x, v$N_SPECIES.y, alternative="greater", paired=T)
          t.test<-"greater"
        }
      }
      item<-data.table(species_evo_type1=groups[i]$species_evo_type, directional_speed1=groups[i]$directional_speed,
                       species_evo_type2=groups[j]$species_evo_type, directional_speed2=groups[j]$directional_speed, 
                       t.test=t.test, p=tt$p.value, p.str=format_p_value(tt$p.value), type="N_SPECIES")
      t.test.result[[length(t.test.result)+1]]<-item
      
      #N Speciation
      tt<-t.test(v$N_SPECIATION.x, v$N_SPECIATION.y, alternative="two.sided", paired=T)
      t.test<-"no differences"
      if (tt$p.value<=0.05){
        tt<-t.test(v$N_SPECIATION.x, v$N_SPECIATION.y, alternative="less", paired=T)
        if (tt$p.value<=0.05){
          t.test<-"less"
        }else{
          tt<-t.test(v$N_SPECIATION.x, v$N_SPECIATION.y, alternative="greater", paired=T)
          t.test<-"greater"
        }
      }
      item<-data.table(species_evo_type1=groups[i]$species_evo_type, directional_speed1=groups[i]$directional_speed,
                       species_evo_type2=groups[j]$species_evo_type, directional_speed2=groups[j]$directional_speed, 
                       t.test=t.test, p=tt$p.value, p.str=format_p_value(tt$p.value), type="N_SPECIATION")
      t.test.result[[length(t.test.result)+1]]<-item
      
      #N Extinction
      tt<-t.test(v$N_EXTINCTION.x, v$N_EXTINCTION.y, alternative="two.sided", paired=T)
      t.test<-"no differences"
      if (tt$p.value<=0.05){
        tt<-t.test(v$N_EXTINCTION.x, v$N_EXTINCTION.y, alternative="less", paired=T)
        if (tt$p.value<=0.05){
          t.test<-"less"
        }else{
          tt<-t.test(v$N_EXTINCTION.x, v$N_EXTINCTION.y, alternative="greater", paired=T)
          t.test<-"greater"
        }
      }
      item<-data.table(species_evo_type1=groups[i]$species_evo_type, directional_speed1=groups[i]$directional_speed,
                       species_evo_type2=groups[j]$species_evo_type, directional_speed2=groups[j]$directional_speed, 
                       t.test=t.test, p=tt$p.value, p.str=format_p_value(tt$p.value), type="N_EXTINCTION")
      t.test.result[[length(t.test.result)+1]]<-item
      
    }
  }
  t.test.result<-rbindlist(t.test.result)
  saveRDS(t.test.result, "../Data/t.test.species.speciation.extinction.rda")
  
  
}
t.test.result<-readRDS("../Data/t.test.species.speciation.extinction.rda")
t.test.result$label1<-paste(t.test.result$species_evo_type1, t.test.result$directional_speed1)
t.test.result$label2<-paste(t.test.result$species_evo_type2, t.test.result$directional_speed2)
ggplot(t.test.result, aes(x = label2, y = label1, fill = t.test))+
  geom_tile()+
  geom_text(aes(label=p.str))+
  facet_wrap(~type)

