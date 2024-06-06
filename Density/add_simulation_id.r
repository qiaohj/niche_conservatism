library(data.table)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
setDTthreads(1)
rdalist<-list.files("../Data/density/")

for (xxx in c(1:length(rdalist))){
  print(paste(xxx, length(rdalist), rdalist[xxx]))
  if (grepl("fixed", rdalist[xxx])){
    next()
  }
  target<-sprintf("../Data/density/%s_fixed.rda", rdalist[xxx])
  if (file.exists(target)){
    next()
  }
  saveRDS(NULL, target)
  df<-readRDS(sprintf("../Data/density/%s", rdalist[xxx])) 
  if (is.null(df)){
    next()
  }
  df$seed_id<-""
  steps<-1e9
  nrows<-nrow(df)
  for (i in seq(1, nrows, by=steps)){
    print(paste(i, nrows))
    if ((i+steps-1)>nrows){
      df[i:nrows]$seed_id<-stringr::str_split_fixed(df[i:nrows]$sp_id, "-", n=2)[,1]
    }else{
      df[i:(i+steps-1)]$seed_id<-stringr::str_split_fixed(df[i:(i+steps-1)]$sp_id, "-", n=2)[,1]
    }
  }
  #df$seed_id<-stringr::str_split_fixed(df$sp_id, "-", n=2)[,1]  
  df$simulation_id<-sprintf("%s_%s_%s", df$seed_id, df$nb, df$da)
  saveRDS(df, target)  
}
