target<-"/home/huijieqiao/git/ees_3d_data/TEST/Results"
folder<-list.dirs(target, full.names=F)
f<-folder[3]
cmd<-c()
for (f in folder){
  fff<-sprintf("%s/%s/%s.log", target, f, f)
  if (!file.exists(fff)){
    cmd<-c(cmd, sprintf("rm -rf %s", sprintf("%s/%s", target, f)))
  }
}
write.table(cmd, "/home/huijieqiao/git/ees_3d_data/TEST/rm.sh", quote=F, col.names = F, row.names = F)
