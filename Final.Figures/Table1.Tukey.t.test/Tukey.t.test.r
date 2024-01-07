library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(rempsyc)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")
coms<-data.table(expand.grid(nb=c(NA, "BROAD", "NARROW"), da=c(NA, "GOOD", "POOR")))
df_result_list<-readRDS("../Figures/20230616/TukeyHSD/TukeyHSD_by_species.rda")
df_result_list$label<-gsub("-conservatism", "", df_result_list$label)
df_result_list<-formatLabel_line(df_result_list)
for (i in c(1:nrow(coms))){
  com<-coms[i]
  print(i)
  df_result<-df_result_list
  if (is.na(com$nb)){
    df_result<-df_result_list[is.na(NB)]
    nb<-"ALL"
  }else{
    df_result<-df_result_list[NB==com$nb]
    nb<-com$nb
  }
  if (is.na(com$da)){
    df_result<-df_result[is.na(DA)]
    da<-"ALL"
  }else{
    df_result<-df_result[DA==com$da]
    da<-com$da
  }
  title2<-sprintf("Niche breadth = %s, Dispersal ability = %s", nb, da)
  df_result<-df_result[type!="N_SPECIES"]
  
  unique(df_result$type)
  type.labs <- c("net_dr"= "net per capita diversification rate",
                 "R_EXTINCTION_SPECIES"=  "extinction rate",
                 "R_SPECIATION_SPECIES" ="speciation rate")
  
  df_result[type!="net_dr"]$diff <- df_result[type!="net_dr"]$diff/1000
  df_result[type!="net_dr"]$lwr <- df_result[type!="net_dr"]$lwr/1000
  df_result[type!="net_dr"]$upr <- df_result[type!="net_dr"]$upr/1000
  
  df_result$type.labs<-type.labs[df_result$type]
  df_result$label<-NULL
  df_result$type<-NULL
  df_result$p_label<-NULL
  df_result$diff_str<-NULL
  df_result$DA<-NULL
  df_result$NB<-NULL
  df_result<-df_result[, c("type.labs", "label_line", "diff", "lwr", "upr", "p_adj")]
  setorderv(df_result, cols=c("type.labs", "label_line"))
  my_table<-nice_table(df_result, 
                       title=c("Tukey’s Post Hoc Tests with One-Way ANOVA", title2),
                       stars=T,
                       col.format.p=6,
                       col.format.custom = 3:5,
                       format.custom = "format_digits")
  flextable::save_as_docx(my_table, path = 
                            sprintf("../Figures/Table1.Tukey.t.test/TukeyHSD_%s_%s.docx", com$nb, com$da))
}


i=1
for (i in c(1:nrow(coms))){
  com<-coms[i]
  print(i)
  t.test<-readRDS(sprintf("../Figures/Table1.Tukey.t.test/Data/t.test_by_species_%s_%s.rda", com$nb, com$da))
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
  
  
  t.test.item<-t.test.item[p_value<=0.05]
  t.test.item<-t.test.item[side!="two.sided"]
  
  t.test.item$type.labs<-type.labs[t.test.item$var]
  t.test.item$label<-NULL
  t.test.item$V2<-NULL
  t.test.item$p_label<-NULL
  t.test.item$p_str<-NULL
  
  t.test.item<-t.test.item[, c("type.labs", "label_line", "p_value", "side")]
  setorderv(t.test.item, cols=c("type.labs", "label_line"))
  fwrite(t.test.item, sprintf("../Figures/Table1.Tukey.t.test/t.test_%s_%s.csv", 
                 com$nb, com$da))
  my_table<-nice_table(t.test.item, 
                       title=c("Paired Samples T-test", title2),
                       stars=T,
                       col.format.p=3)
  flextable::save_as_docx(my_table, path = 
                            sprintf("../Figures/Table1.Tukey.t.test/t.test_%s_%s.docx", 
                                    com$nb, com$da))
}
