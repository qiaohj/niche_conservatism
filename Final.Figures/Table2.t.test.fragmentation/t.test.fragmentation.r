library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(rempsyc)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

t.test.item<-readRDS(sprintf("../Figures/20230616/t.test/t.test_by_species_distribution_%s_%s.rda", NA, NA))
colnames(t.test.item)[1]<-"label"
t.test.item<-formatLabel_line(t.test.item)
t.test.item<-t.test.item[side!="two.sided"]
t.test.item<-t.test.item[p_value<=0.05]

t.test.item<-t.test.item[var=="N_GROUP"]
t.test.item$var<-"Number of populations"

t.test.item<-t.test.item[, c("label_line", "p_value", "side")]
setorderv(t.test.item, cols=c("label_line"))
my_table<-nice_table(t.test.item, 
                     title=c("Paired Samples T-test"),
                     stars=T,
                     col.format.p=2)
flextable::save_as_docx(my_table, path = 
                          sprintf("../Figures/Table2.t.test.fragmentation/t.test.docx"))