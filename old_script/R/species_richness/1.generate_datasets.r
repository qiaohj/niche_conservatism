library(dplyr)
setwd("~/git/ees_3d/R/species_richness")
args = commandArgs(trailingOnly=TRUE)
nb<-args[1]
da<-args[2]

if (is.na(nb)){
  nb<-"NARROW"
} 
if (is.na(da)){
  da<-"POOR"
}

target<-"/home/huijieqiao/git/ees_3d_data/TEST/Results"
simulations<-list.dirs(target, full.names = F)
i=2
result<-list()
for (i in c(1:length(simulations))){
 
  sim <- simulations[i]
  if (!(grepl(nb, sim)&grepl(da, sim))){
    next
  }
  print(i)
  map<-read.table(sprintf("%s/%s/%s.log", target, sim, sim), head=F, sep=",", stringsAsFactors = F)
  colnames(map)<-c("YEAR", "ID", "group_id", "sp_id")
  map$sp_id<-as.character(map$sp_id)
  #class(map$sp_id)
  map<-as_tibble(map)
  
  year=1198
  for (year in c(1199:0)){
    y_str<-as.character(year)
    item<-map %>% filter(YEAR==year)
    if (nrow(item)==0){
      next
    }
    if (y_str %in% names(result)){
      result[[y_str]]<-bind_rows(result[[y_str]], item)
    }else{
      result[[y_str]]<-item
    }
  }
  
}
saveRDS(result, sprintf("%s/../Tables/%s.%s.rda", target, nb, da))
