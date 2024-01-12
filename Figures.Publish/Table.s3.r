library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(rempsyc)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))

i=1
coms<-coms[1]
for (i in c(1:nrow(coms))){
  com<-coms[i]
  print(i)
  t.test<-readRDS(sprintf("../Figures/20230616/t.test/t.test_by_species_%s_%s_paired_fixed_window.rda", com$nb, com$da))
  colnames(t.test)[1]<-"label"
  t.test<-formatLabel_line(t.test)
  
  
  t.test.item<-t.test
  if (is.na(com$nb)){
    nb<-"ALL"
  }else{
    nb<-com$nb
  }
  if (is.na(com$da)){
    da<-"ALL"
  }else{
    da<-com$da
  }
  title2<-sprintf("Niche breadth = %s, Dispersal ability = %s", nb, da)
  
  type.labs <- c("net_dr"= "net per capita diversification rate",
                 "R_EXTINCTION_SPECIES"=  "extinction rate",
                 "R_SPECIATION_SPECIES" ="speciation rate")
  
  
  #t.test.item<-t.test.item[p_value<=0.05]
  t.test.item<-t.test.item[side!="two.sided"]
  
  t.test.item$type.labs<-type.labs[t.test.item$var]
  t.test.item$label<-NULL
  t.test.item$V2<-NULL
  t.test.item$p_label<-NULL
  t.test.item$p_str<-NULL
  
  t.test.item<-t.test.item[, c("type.labs", "label_line", "p_value", "side")]
  setorderv(t.test.item, cols=c("type.labs", "label_line"))
  fwrite(t.test.item, "../Figures.Publish/Data/Table.S3/Table.S3.csv")
  my_table<-nice_table(t.test.item, 
                       title=c("Paired Samples T-test", title2),
                       stars=T,
                       col.format.p=3)
  flextable::save_as_docx(my_table, path = "../Figures.Publish/Tables/Table.S3/Table.S3.docx")
}
