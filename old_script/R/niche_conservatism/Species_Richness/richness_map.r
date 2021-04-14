library(dplyr)
library(data.table)
base<-"/home/huijieqiao/Downloads"
i=2
#YEARS<-c(1199:0)
DAs<-c("GOOD", "POOR")
NBs<-c("MODERATE", "NARROW")
EVO_RATIOs<-c(1, 0.05, 0.005)
EVO_TYPEs<-c(1,2,7)
items<-expand.grid(DA=DAs, NB=NBs, EVO_RATIO=EVO_RATIOs, EVO_TYPE=EVO_TYPEs)
items$LABEL<-paste(items$DA, items$NB, items$EVO_RATIO, items$EVO_TYPE)

richness_all<-list()
for (i in c(1:nrow(items))){
  richness_all[[items[i, "LABEL"]]]<-NULL
}
for (i in c(1:200)){
  
  print(paste(Sys.time(), i, 200))
  target<-sprintf("%s/item_richness_rep/df_richness_%d.rda", base, i)
  print(paste(Sys.time(), "Reading Data"))
  df<-readRDS(target)
  
  df$LON<-NULL
  df$LAT<-NULL
  df$MIN_TEMP<-NULL
  df$MAX_TEMP<-NULL
  df$MAX_PREC<-NULL
  
  nrow<-nrow(df)
  if (is.null(nrow)){
    nrow<-0
  }
  if (nrow==0){
    next()
  }
  
  #YEARS<-df%>%dplyr::distinct(Y)
  DAs<-df%>%dplyr::distinct(DA)
  NBs<-df%>%dplyr::distinct(NB)
  EVO_RATIOs<-df%>%dplyr::distinct(EVO_RATIO)
  EVO_TYPEs<-df%>%dplyr::distinct(EVO_TYPE)
  items<-expand.grid(DA=pull(DAs), NB=pull(NBs), EVO_RATIO=pull(EVO_RATIOs), EVO_TYPE=pull(EVO_TYPEs))
  
  print(paste(Sys.time(), "Converting df data type"))
  df<-data.table(df)
  print(paste(Sys.time(), "Setting df keys"))
  setindexv(df, c("Y", "GLOBAL_ID", "DA", "NB", "EVO_TYPE", "EVO_RATIO",
                        "REP"))
  print(paste(Sys.time(), "Fixing EVO_TYPE"))
  if (0.1 %in% items$EVO_RATIO){
    df[df[EVO_RATIO==0.1, which=T], "EVO_RATIO"]<-0.05
    items[which(items$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05
  }
  if (0.01 %in% items$EVO_RATIO){
    df[df[EVO_RATIO==0.01, which=T], "EVO_RATIO"]<-0.005
    items[which(items$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005
  }
  items$LABEL<-paste(items$DA, items$NB, items$EVO_RATIO, items$EVO_TYPE)
  for (j in c(1:nrow(items))){
    item<-items[j,]
    print(paste(i,j,nrow(items)))
    richness<-richness_all[[item$LABEL]]
    df_item<-df[df[((DA==item$DA)&(NB==item$NB)&
                      (EVO_TYPE==item$EVO_TYPE)&(EVO_RATIO==item$EVO_RATIO)), which=T],]
    if (is.null(richness)){
      richness<-df_item
    }else{
      print(paste(Sys.time(), "Merging richness and df"))
      richness_t<-merge(richness, df_item, 
                        by=c("Y", "GLOBAL_ID", "DA", "NB", "EVO_TYPE", "EVO_RATIO", "REP"), all=T)
      print(paste(Sys.time(), "Fixing NA values"))
      richness_t[which(is.na(richness_t$N_SP.x)), "N_SP.x"]<-0
      richness_t[which(is.na(richness_t$N_SP.y)), "N_SP.y"]<-0
      richness_t$N_SP<-richness_t$N_SP.x+richness_t$N_SP.y
      print(paste(Sys.time(), "Generating new richness"))
      richness<-richness_t
      richness$N_SP.x<-NULL
      richness$N_SP.y<-NULL
      print(paste(Sys.time(), "nrow(richness):", nrow(richness)))
    }
    richness_all[[item$LABEL]]<-richness
  }
  print(names(richness_all))
}
saveRDS(richness_all, sprintf("%s/Data/richness_all.rda", base))
