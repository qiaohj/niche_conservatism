library("dplyr")
library("DBI")

setwd("~/git/ees_3d/R/smart_species")
args = commandArgs(trailingOnly=TRUE)
iii=9
iii = args[1]
base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"

mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf_%s.sqlite", base, iii))  
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb) 
simulations<-simulations %>% filter(nb!="BROAD")
simulations<-simulations %>% filter(is_run==1)

target<-sprintf("%s/Tables/individual_ratio_nb_%s.rda", base, iii)
result<-data.frame()

base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"

for (i in c(1:nrow(simulations))){
  s<-simulations[i,]
  
  print(paste(i, nrow(simulations), s$label))
  nb_rda<-sprintf("%s/RESULTS/%s/%s_nb.rda", base2, s$label, s$label)
  nb_log_rda<-sprintf("%s/RESULTS/%s/%s_log_nb.rda", base2, s$label, s$label)
  if (!file.exists(nb_rda)){
    next()
  }
  sp_niche<-readRDS(nb_rda)
  if (nrow(result)==0){
    result<-sp_niche
  }else{
    result<-bind_rows(result, sp_niche)
  }
}
saveRDS(result, target)



