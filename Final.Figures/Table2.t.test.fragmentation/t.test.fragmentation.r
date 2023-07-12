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
        legend.text = element_text(size=7, margin = margin(t = 5)),
        legend.box.spacing = unit(1, "mm"),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.spacing.y = unit(1.0, 'mm'),
        ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
        ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.spacing.x = unit(8, "mm"))

#scale_x_discrete(guide = guide_axis(n.dodge = 2))
p


ggsave(p, filename=sprintf("../Figures/Figure10.Tukey.Test.fragmentation/TukeyHSD_%s_%s.png", com$nb, com$da),
       width=6, height=4)

ggsave(p, filename=sprintf("../Figures/Figure10.Tukey.Test.fragmentation/TukeyHSD_%s_%s.pdf", com$nb, com$da),
       width=6, height=4)

df_result<-formatLabel_line(df_result)
df_result2<-df_result[, c("label_line", "diff", "lwr", "upr", "p_adj")]
setorderv(df_result2, cols=c("label_line"))
my_table<-nice_table(df_result2, 
                     title=c("Tukey’s Post Hoc Tests with One-Way ANOVA"),
                     stars=T,
                     col.format.p=5,
                     col.format.custom = 2:4,
                     format.custom = "format_digits")
flextable::save_as_docx(my_table, path = 
                          sprintf("../Figures/Table5.Tukey.fragmentation/TukeyHSD_%s_%s.docx", "NA", "NA"))
