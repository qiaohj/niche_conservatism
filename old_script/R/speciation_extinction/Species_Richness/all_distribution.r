base<-"/media/huijieqiao/Speciation_Extin/Speciation_Extinction"

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


mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/Configurations/conf.sqlite", base))  
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb) 
simulations<-simulations %>% dplyr::filter(evo_type==1)

cmd<-c()


distribution<-NULL
#simulations[which(simulations$global_id==12246),]

for (i in c(start:end)){
  print(paste(i, start, end))
  s<-simulations[i,]
  nb<-strsplit(s$nb_v, "\\|")[[1]]
  nb_temp<-as.numeric(strsplit(nb[1], ",")[[1]])
  nb_prec<-as.numeric(strsplit(nb[3], ",")[[1]])
  log<-sprintf("%s/Results/%s/%s.log.env.rda", base, s$label, s$label)
  log_df<-readRDS(log)
  
  
  dis<-log_df%>%dplyr::filter(SUITABLE==1)
  
  dis$SEED_ID<-s$global_id
  dis$DA<-s$da
  dis$NB<-s$nb
  dis$EVO_TYPE<-s$evo_type
  dis$EVO_RATIO<-s$niche_envolution_individual_ratio
  dis$START_POINT<-s$from
  dis$ID<-s$id
  
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
