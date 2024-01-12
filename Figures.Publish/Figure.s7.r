library(forecast)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(rempsyc)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")



df_result<-readRDS(sprintf("../Figures/20230616/t.test/tukey_n.groups.by_species_distribution_%s_%s.rda", NA, NA))
df_result$diff_str<-round(df_result$diff, 3)
df_result$alternative<-""
df_result[diff>0]$alternative<-"Greater"
df_result[diff<0]$alternative<-"Less"
df_result[p_label==""]$alternative<-"No sig diff"



table(df_result$alternative)
df_result<-df_result[grepl("-conservatism", label)]
df_result$label<-gsub("-conservatism", "", df_result$label)
df_result$diff_label<-sprintf("%.3f %s", df_result$diff_str, df_result$p_label)
df_result<-formatLabelX(df_result)

p<-ggplot(df_result)+geom_errorbarh(aes(y=label_x, xmin=lwr, xmax=upr, color=alternative), height=0.1)+
  geom_vline(aes(xintercept=0), linetype=2, color="#444444")+
  geom_point(aes(y=label_x, x=diff, color=alternative), size=0.5)+
  geom_text(aes(y=label_x, x=upr, label=p_label), hjust=0.5, vjust=-0.2, size=2)+
  scale_y_discrete(limits=rev)+
  scale_color_manual(values=c("#EE6677", "#000000", "#4477AA"), 
                     breaks=c("Greater", "No sig diff", "Less"))+
  theme_bw()+
  guides(y = ggh4x::guide_axis_nested(delim = "&"))+
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        legend.background = element_rect(fill=bg),
        legend.key.size = unit(5, "mm"),
        legend.text = element_text(size=7),
        legend.box.spacing = unit(1, "mm"),
        legend.spacing.y = unit(1.0, 'mm'),
        ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
        ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(8, "mm"))

#scale_x_discrete(guide = guide_axis(n.dodge = 2))
p
df_result$N<-NULL
fwrite(df_result, "../Figures.Publish/Data/Figure.S7/Figure.S7.csv")
#ggsave(p, filename="../Figures.Publish/Figures/Figure.S7/Figure.S7.pdf",
#       width=6, height=4)

