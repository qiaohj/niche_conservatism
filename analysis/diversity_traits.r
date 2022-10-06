library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
print(sprintf("Number of core(s) is(are) %d.", getDTthreads()))


base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
#mydb <- dbConnect(RSQLite::SQLite(), "/media/huijieqiao/SSD_Fast/conf.sqlite")
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=233

#simulations<-simulations[which(!((simulations$nb=="BROAD" & simulations$da=="GOOD"))),]
simulations<-simulations[which(((simulations$nb=="BROAD" & simulations$da=="GOOD"))),]
simulations<-simulations[which(simulations$species_evo_level==0),]
simulations<-simulations[which(simulations$is_run==1),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(4)),]

table(simulations$species_evo_type)

table(simulations[, c("nb", "da")])


template<-"%d_%s_%s_%d_%s_%d"
#all_df<-simulations[which(simulations$global_id %in% sub_seeds),]
all_df<-simulations
all_df<-all_df[sample(nrow(all_df), nrow(all_df)),]
all_df<-data.table(all_df)

for (i in c(1:nrow(all_df))){
  item<-all_df[i,]
  sp<-sprintf(template, item$global_id, item$da, item$nb, item$species_evo_type, item$directional_speed, item$species_evo_level)
  
  print(paste(i, nrow(all_df), sp))
  if ((item$species_evo_level==1)&(item$nb=="BROAD")){
    print("1 broad skip")
    next()
  }
  if ((item$species_evo_level==1)&(item$species_evo_type==1)){
    print("1 1 skip")
    next()
  }
  
  if (item$species_evo_level==1){
    base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results_1"
  }else{
    if ((item$nb=="BROAD")&(item$da=="GOOD")){

      base<-"../Results"
    }else{
      base<-"/media/huijieqiao/QNAS/Niche_Conservatism/Results"
    }
  }
  ttt<-sprintf("%s/%s/%s.diversity.rda", base, sp, sp)
  
  if (file.exists(ttt)){
    #size<-file.size(ttt)
    #if (size<100){
    #  print(sprintf("rm -rf %s", ttt))
    #}
    print("skip")
    next()
  }
  log<-sprintf("%s/%s/%s.log", base, sp, sp)
  #log_check<-sprintf("%s/%s/%s.log", base_check, sp, sp)
  
  
  if (!file.exists(log)){
    print("not exist, skip")
    next()
  }
  
  
  print(paste(Sys.time(), file.size(log)/1024/1024))
  if ((file.size(log)/1024/1024)>100){
    print("too big, skip")
    next()
  }
  
  saveRDS(NULL, ttt)
  df<-fread(log)
  
  
  colnames(df)<-c("year", "global_id", "group_id", "n", "sp_id", "suitable")
  df<-df[suitable==1]
  j=500
  
  diversity_df<-df[, .(N_SPECIES=length(unique(sp_id)),
                                    N_INDIVIDUAL=sum(n)), 
                                by=list(year, global_id)]
  
  saveRDS(diversity_df, ttt)
  
}
