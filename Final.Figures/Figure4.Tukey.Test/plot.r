library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(car)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
d<-readRDS("../Data/tslm_and_glm/d_ndr.rda")
d_null<-d[species_evo_type==1]
d_null<-d_null[, c("from", "to", "nb", "da", "global_id")]
d_null$tag<-1
d_with_null<-merge(d, d_null, 
                   by=c("from", "to", "nb", "da", "global_id"))
item<-d[from>=-1000]
item<-item[from %in% seq(from=-1000, to=-100, by=100)]

labels<-unique(item$label)
x_label<-labels[1]
i=2
result<-list()
for (i in c(2:length(labels))){
  df_item<-merge(item[label==x_label], item[label==labels[i]], by=c("nb", "da", "from", "global_id"))
  result[[length(result)+1]]<-df_item
}
result<-rbindlist(result)

result$net_dr.x.bin<-round(result$net_dr.x * 10)/10 
result$net_dr.y.bin<-round(result$net_dr.y * 10)/10
result_N<-result[, .(N=.N), by=list(label.y, net_dr.x.bin, net_dr.y.bin)]
hist(result_N[N!=max(result_N$N)]$N)
hist(result_N[N>1000]$N)
result_N[between(N, 500, 1000),]
p<-ggplot(result_N[N<=1000])+
  geom_tile(aes(x=net_dr.x.bin, y=net_dr.y.bin, fill=N))+
  geom_abline()+
  scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "darkred", midpoint = 500)+
  coord_equal()+
  facet_wrap(~label.y)+
  theme_bw()
p


range(result$net_dr.x)
range(result$net_dr.y)
ggsave(p, filename="../Figures/plot_ndr.png", width=10, height=10)

p<-ggplot(result_N[N<=1000 & net_dr.x.bin!=0 & net_dr.x.bin!=1 & net_dr.y.bin!=0 & net_dr.y.bin!=1])+
  geom_tile(aes(x=net_dr.x.bin, y=net_dr.y.bin, fill=N))+
  geom_abline()+
  scale_fill_gradient2(low = "darkgreen", mid = "yellow", high = "darkred", midpoint = 100)+
  coord_equal()+
  facet_wrap(~label.y)+
  theme_bw()
p
ggsave(p, filename="../Figures/plot_ndr_xx.png", width=10, height=10)

range(result$net_dr.x)
range(result$net_dr.y)
ggsave(p, filename="../Figures/plot_ndr.png", width=10, height=10)



p<-ggplot(item)+
  geom_histogram(aes(x=net_dr), bins=10)+
  facet_wrap(~label)+
  xlim(-1, 2)+
  theme_bw()
p

p<-ggplot(result)+geom_point(aes(x=R_SPECIATION_SPECIES.x, y=R_SPECIATION_SPECIES.y), size=0.3)+
  geom_abline()+
  coord_equal()+
  facet_wrap(~label.y)+
  theme_bw()
ggsave(p, filename="../Figures/plot_N_SPECIATION.png", width=10, height=10)


p<-ggplot(result)+geom_point(aes(x=R_EXTINCTION_SPECIES.x, y=R_EXTINCTION_SPECIES.y), size=0.3)+
  geom_abline()+
  coord_equal()+
  facet_wrap(~label.y)+
  theme_bw()
ggsave(p, filename="../Figures/plot_N_EXTINCTION.png", width=10, height=10)
