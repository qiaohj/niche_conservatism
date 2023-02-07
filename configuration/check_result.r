library(dplyr)
library(ggplot2)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)


i=1

if (F){
  
  simulations<-simulations[
    which(simulations$global_id %in% c(27790, 25837, 24109,
                                       54786, 19759, 13651,
                                       17569, 18517, 4497,
                                       4847, 9898, 11847,
                                       40440, 23724)),]
}
cmd_rm<-c()
cmd_mv<-c()
simulations<-simulations[which(simulations$is_run==1),]

#simulations<-simulations[which(simulations$nb=="NARROW"),]
simulations<-simulations[which(simulations$nb=="NARROW"),]
simulations<-simulations[which(simulations$da=="POOR"),]
#simulations<-simulations[which(!((simulations$da=="GOOD")&(simulations$nb=="BROAD"))),]

simulations<-simulations[which(simulations$species_evo_level==0),]
#simulations<-simulations[which(simulations$species_evo_type %in% c(2,3,4)),]

for (i in c(1:nrow(simulations))){
  
  item<-simulations[i,]
  folder<-sprintf("/media/huijieqiao/Butterfly/Niche_Conservatism/Results/%s", item$label)
  
  target<-"/media/huijieqiao/Butterfly/Niche_Conservatism/Results"
  
  print(paste(i, nrow(simulations), folder))
  
  if (file.exists(folder)){
    log<-sprintf("%s/%s.log", folder, item$label)
    if (!file.exists(log)){
      passed<-F
    }else{
      log<-sprintf("%s/%s.sqlite", folder, item$label)
      passed<-T
      if (file.exists(log)){
        mydb <- dbConnect(RSQLite::SQLite(), log)
        trees<-dbReadTable(mydb, "trees")
        dbDisconnect(mydb)
        if (nrow(trees)==2){
          tree_str<-trees[1, "CONTENT"]
          if (nchar(tree_str)<10){
            asdfasdf
          }
          sp.log<-sprintf("%s/%s.sp.log", folder, item$label)
          if (file.exists(sp.log)){
          }else{
            passed<-F
          }
        }else{
          passed<-F
          
        }
      }else{
        passed<-F
      }
    }
    if (!passed){
      cmd_rm<-c(cmd_rm, sprintf("rm -rf %s", folder))
    }else{
      #if (item$is_run==0){
        cmd_mv<-c(cmd_mv, sprintf("mv %s %s", folder, target))
      #}
      #if (item$species_evo_level==1){
      #  cmd_mv<-c(cmd_mv, sprintf("mv %s %s", folder, target))
      #}
      
    }
  }
}
write.table(cmd_rm, "../Data/temp/rm.sh", row.names = F, quote=F, col.names = F)
write.table(cmd_mv, "../Data/temp/mv.sh", row.names = F, quote=F, col.names = F)

