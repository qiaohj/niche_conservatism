base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"
db_base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
base2<-"/media/huijieqiao/Butterfly/SMART_SPECIES"
args = commandArgs(trailingOnly=TRUE)
start=as.numeric(args[1])
end=as.numeric(args[2])

t_f<-sprintf("%s/Data/items_distribution/distribution_%d_%d.rda", base, start, end)
skip<-T
if (file.exists(t_f)){
  skip<-T
  if (F){
    test_df<-readRDS(t_f)
    if (is.na(test_df)){
      skip<-F
    }else{
      if (length(unique(test_df$global_id))<5){
        skip<-F
      }
    }
  }
  
}else{
  skip<-F
}
if (skip){
  skipasdfasdf
}
saveRDS(NA, t_f)

library("dplyr")
library("DBI")
library("phytools")
library("tidyr")
library("data.table")
library("tibble")
library("raster")
library("sp")
library("ggplot2")
library("sgeostat")


simulations<-NULL
for (i in c(1,2,7)){
  print(i)
  mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/conf_%s.sqlite", db_base, i))  
  simulation<-dbReadTable(mydb, "simulations")
  dbDisconnect(mydb) 
  if (is.null(simulations)){
    simulations<-simulation
  }else{
    simulations<-bind_rows(simulation, simulations)
  }
}
simulations<-simulations %>% filter(nb!="BROAD")
simulations<-simulations %>% filter(is_run==1)
cmd<-c()


distribution<-NULL
#simulations[which(simulations$global_id==12246),]

for (i in c(start:end)){
  print(paste(i, start, end))
  s<-simulations[i,]
  nb<-strsplit(s$nb_v, "\\|")[[1]]
  nb_temp<-as.numeric(strsplit(nb[1], ",")[[1]])
  nb_prec<-as.numeric(strsplit(nb[3], ",")[[1]])
  log<-sprintf("%s/RESULTS/%s/%s.log.env.rda", base2, s$label, s$label)
  log_df<-readRDS(log)
  
  
  dis<-log_df%>%dplyr::filter(SUITABLE==1)
  dis$global_id<-s$global_id
  dis$da<-s$da
  dis$nb<-s$nb
  dis$evo_type<-s$evo_type
  dis$evo_ratio<-s$niche_envolution_individual_ratio
  if (nrow(dis)>0){
    if (is.null(distribution)){
      distribution<-dis
    }else{
      distribution<-bind_rows(distribution, dis)
    }
  }
  
  #saveRDS(stat, sprintf("%s/Data/stat.rda", base))
  #saveRDS(detail, sprintf("%s/Data/detail.rda", base))
  #saveRDS(sp_character, sprintf("%s/Data/sp_character.rda", base))
  #saveRDS(extinction_df, sprintf("%s/Data/extinction_df.rda", base))
  #saveRDS(speciation_df, sprintf("%s/Data/speciation_df.rda", base))
  
  
}


saveRDS(distribution, t_f)
if (F){
  cc<-c()
  for (f in seq(from=1, to=20000, by=100)){
    cc<-c(cc, sprintf("Rscript all_distribution.r %d %d", f, f+99))
  }
  setwd("~/git/ees_3d/R/niche_conservatism/Species_Richness")
  write.table(cc, "all_distribution.sh", quote = F, row.names=F, col.names = F)
  
  tt<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism/Data/items_distribution/distribution_10001_10100.rda"
  df_test<-readRDS(tt)
  tail(distribution)
  distribution%>%dplyr::filter(global_id==12246)
  unique(distribution$global_id)
}
