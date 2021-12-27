library(data.table)
library(ggplot2)
library(RSQLite)
library(DBI)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
base_db<-"../Configuration/conf.sqlite"
mydb <- dbConnect(RSQLite::SQLite(), base_db)
simulations<-dbReadTable(mydb, "simulations")
dbDisconnect(mydb)
simulations<-data.table(simulations)

i=1




simulations<-simulations[species_evo_level==0]
simulations<-simulations[directional_speed!=1]


#seed<-unique(simulations[, c("global_id", "da", "nb", "species_evo_type",                       
#                             "directional_speed", "species_evo_level")])
cols<-c("global_id", "da", "nb")
seed<-unique(simulations[, ..cols])
#seed<-seed[(!((da=="GOOD")&(nb=="BROAD")))]
#seed<-seed[(((da=="GOOD")&(nb=="BROAD")))]


seed<-seed[sample(nrow(seed), nrow(seed))]
#seed<-seed[global_id==31500]
cmd_rm<-c()
for (i in c(1:nrow(seed))){
  print(paste(i, nrow(seed)))
  item<-seed[i]
  is_QNAS<-T
  if ((item$da=="GOOD")&(item$nb=="BROAD")){
    is_QNAS<-F
  }
  item_sim<-simulations[(global_id==item$global_id)&(nb==item$nb)&(da==item$da)]
  filesize<-list()
  
  j=1
  for (j in c(1:nrow(item_sim))){
    item_sim2<-item_sim[j]
    if (is_QNAS){
      folder<-sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s/%s.log", 
                    item_sim2$label, item_sim2$label)  
    }else{
      folder<-sprintf("/media/huijieqiao/Butterfly/Niche_Conservatism/Results/%s/%s.log", 
                      item_sim2$label, item_sim2$label)
    }
    df_item<-data.table(label=item_sim2$label, size=file.size(folder))
    filesize[[j]]<-df_item
  }
  filesize<-rbindlist(filesize)
  filesize<-filesize[!is.na(filesize$size)]
  isbig<-nrow(filesize[size>1e6])>0
  if (isbig){
    
    problems<-filesize[size<2e3]
    if (nrow(problems)>0){
      if (nrow(problems)==0){
        next()
      }

      for (k in c(1:nrow(problems))){
        if (is_QNAS){
          cmd_rm<-c(cmd_rm, sprintf("rm -rf %s", 
                                sprintf("/media/huijieqiao/QNAS/Niche_Conservatism/Results/%s", 
                                        problems[k]$label) ))
        }else{
          cmd_rm<-c(cmd_rm, sprintf("rm -rf %s", 
                                  sprintf("/media/huijieqiao/Butterfly/Niche_Conservatism/Results/%s", 
                                          problems[k]$label) ))
        }
      }
    }
  }
  
  
}
write.table(cmd_rm, "../Data/temp/rm.sh", row.names = F, quote=F, col.names = F)


