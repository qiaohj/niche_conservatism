library(dplyr)
library(ggplot2)
library(RSQLite)
library(DBI)
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)

#simulations<-simulations[which(simulations$is_run==1),]
i=1
for (i in c(1:simulations)){
  print(paste(i, nrow(simulations)))
  item<-simulations[i,]
  folder<-sprintf("../Results/%s", item$label)
  if (file.exists(folder)){
    log<-sprintf("%s/%s.sqlite", folder, item$label)
    passed<-T
    if (file.exists(log)){
      mydb <- dbConnect(RSQLite::SQLite(), log)
      trees<-dbReadTable(mydb, "trees")
      dbDisconnect(mydb)
      if (nrow(trees)==2){
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
}