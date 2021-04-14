library(Rmisc)
library(stringr)
library(ggplot2)
library(dplyr)
setwd("~/git/ees_3d/R/smart_species")

result_nb<-NULL
evo_type_c<-c(1,2,3,4,5,7,8,9)
base<-"/home/huijieqiao/git/ees_3d_data/SMART_SPECIES"
for (i in evo_type_c){
  print(i)
  result<-readRDS(sprintf("%s/Tables/individual_ratio_nb_%d.rda", base, i))
  result$Y<-result$Y*-1
  if (is.null(result_nb)){
    result_nb<-result
  }else{
    result_nb<-bind_rows(result_nb, result)
  }
}
result_nb<-result_nb%>%ungroup()


NULL_DF<-result_nb %>% filter(EVO_TYPE==1)
result_nb<-result_nb %>% filter(EVO_TYPE!=1)
NULL_DF$EVO_RATIO<-0.005
result_nb<-bind_rows(result_nb, NULL_DF)
NULL_DF$EVO_RATIO<-0.05
result_nb<-bind_rows(result_nb, NULL_DF)
result_nb[which(result_nb$EVO_RATIO==0.1), "EVO_RATIO"]<-0.05
result_nb[which(result_nb$EVO_RATIO==0.01), "EVO_RATIO"]<-0.005

unique(result_nb$EVO_TYPE)
unique(result_nb$EVO_RATIO)

saveRDS(result_nb, sprintf("%s/Tables/individual_ratio_nb.rda", base))

