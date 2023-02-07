library(data.table)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
folder<-"/media/huijieqiao/Butterfly/Niche_Conservatism/Results"
folders<-list.files(folder)
folders_0<-folders[grepl("GOOD_BROAD_1_0_0", folders)>0]
i=1
j=1
coms<-data.table(expand.grid(type=c(2:4), ratio=c("0.01", "0.1", "0.5")))
commands<-c()
for (i in c(1:length(folders_0))){
  label<-folders_0[i]
  f<-sprintf("%s/%s/%s.log", folder, label, label)
  if (!file.exists(f)){
    next()
  }
  f_size<-file.size(f)
  if (f_size>8e9){
    for(j in c(1:nrow(coms))){
      com<-coms[j]
      target_label<-gsub("_1_", sprintf("_%d_", com$type), label)
      target_label<-gsub("_0_", sprintf("_%s_", com$ratio), target_label)
      log<-sprintf("%s/%s/%s.log", folder, target_label, target_label)
      if (file.exists(log)){
        print(sprintf("%s. Existing, skip", log))
      }
      commands<-c(commands, sprintf("cp -R %s/%s %s/%s", folder, label, folder, target_label))
      commands<-c(commands, sprintf("mv %s/%s/%s.log %s/%s/%s.log", 
                                    folder, target_label, label, 
                                    folder, target_label, target_label))
      commands<-c(commands, sprintf("mv %s/%s/%s.delta.log %s/%s/%s.delta.log", 
                                    folder, target_label, label, 
                                    folder, target_label, target_label))
      commands<-c(commands, sprintf("mv %s/%s/%s.sp.log %s/%s/%s.sp.log", 
                                    folder, target_label, label, 
                                    folder, target_label, target_label))
      commands<-c(commands, sprintf("mv %s/%s/%s.sqlite %s/%s/%s.sqlite", 
                                    folder, target_label, label, 
                                    folder, target_label, target_label))
      
    }
  }
}

write.table(commands, "../mv.sh", quote=F, row.names = F, col.names = F)
