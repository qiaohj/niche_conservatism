library(RSQLite)
library(DBI)
library(dplyr)
library(stringr)

folders<-c("Debiased_Maximum_Monthly_Precipitation",
           "Debiased_Maximum_Monthly_Temperature",
           "Debiased_Mean_Annual_Precipitation",
           "Debiased_Mean_Annual_Temperature",
           "Debiased_Minimum_Monthly_Temperature")

base<-"/home/huijieqiao/git/ees_3d_data/ISEA3H8"
args = commandArgs(trailingOnly=TRUE)
folder<-folders[1]
folder<-folders[as.numeric(args[1])]
# Create an ephemeral in-memory RSQLite database
mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/SQLITE/env_Hadley3D.sqlite", base))
#dbReadTable(mydb, "mask")
mask<-read.csv(sprintf("%s/CSV/mask.csv", base), sep=" ", head=T)
mask$v<-1
dbWriteTable(mydb, "mask", mask, overwrite=T)

y=0

df_all<-NULL
for (y in c(0:1200)){
  print(y)
  df<-read.csv(sprintf("%s/CSV/%s/%s.csv", base, folder, str_pad(y, 4, pad="0")), sep=" ", head=T)
  colnames(df)<-c("global_id", "v")
  df$year<-y
  df<-as_tibble(df)
  if (is.null(df_all)){
    df_all<-df
  }else{
    df_all<-bind_rows(df_all, df)
  }
}
dbWriteTable(mydb, folder, df_all, overwrite=T)

neighbor_info<-read.csv("/home/huijieqiao/git/ees_3d_data/ISEA3H8/isea3h8neigpbor.nbr", sep=" ", head=F)
neighbor_info$neighbor<-paste(neighbor_info$V2, neighbor_info$V3, 
                              neighbor_info$V4, neighbor_info$V5, 
                              neighbor_info$V6, sep=",")
neighbor_info[which(!is.na(neighbor_info$V7)), "neighbor"]<-paste(neighbor_info[which(!is.na(neighbor_info$V7)), "neighbor"],
                                                                  neighbor_info[which(!is.na(neighbor_info$V7)), "V7"], sep=",")
neighbor_info<-neighbor_info[, c("V1", "neighbor")]
colnames(neighbor_info)[1]<-"global_id"
dbWriteTable(mydb, "neighbor", neighbor_info, overwrite=T)
dbDisconnect(mydb)

mydb <- dbConnect(RSQLite::SQLite(), sprintf("%s/SQLITE/env_Hadley3D.sqlite", base))
environments<-data.frame(names=folders)
environments$begin_year<-1200
environments$end_year<-0
environments$step<-1

dbWriteTable(mydb, "environments", environments, overwrite=T)
dbDisconnect(mydb)
