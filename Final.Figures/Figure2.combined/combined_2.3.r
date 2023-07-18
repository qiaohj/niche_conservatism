library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
library(ggstance)
library(ggh4x)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

d<-readRDS("../Data/tslm_and_glm/d_ndr.rda")
d<-d[floor((d$to+0)/100)==(d$to+0)/100]
coms<-data.table(expand.grid(nb=c(NA, "NB"), da=c(NA, "DA")))
i=1
var="N_GROUP"
d_se<-d[, .(R_SPECIATION_SPECIES=mean(R_SPECIATION_SPECIES),
            sd_R_SPECIATION_SPECIES=sd(R_SPECIATION_SPECIES),
            R_EXTINCTION_SPECIES=mean(R_EXTINCTION_SPECIES),
            sd_R_EXTINCTION_SPECIES=sd(R_EXTINCTION_SPECIES),
            net_dr=mean(net_dr),
            sd_net_dr=sd(net_dr)),
        by=list(from, to, evo_type, species_evo_type, directional_speed, label)]
d_se<-formatLabels(d_se)
p1<-ggplot(d_se)+geom_line(aes(x=to/10, y=R_SPECIATION_SPECIES/1000, 
                               color=label_line, 
                               linetype=label_line))+
  scale_color_manual("Evolution scenario", values=evo_type_color)+
  scale_linetype_manual("Evolution scenario", 
                        values=evo_type_line)+
  labs(x=x_label, y="net speciation")+
  guides(color=guide_legend(nrow=2,byrow=TRUE),
         linetype=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank())

p1
p2<-ggplot(d_se)+geom_line(aes(x=to/10, y=R_EXTINCTION_SPECIES/1000, 
                               color=label_line, 
                               linetype=label_line))+
  scale_color_manual("Evolution scenario", values=evo_type_color)+
  scale_linetype_manual("Evolution scenario", values=evo_type_line)+
  labs(x=x_label, y="net extinction")+
  guides(color=guide_legend(nrow=2,byrow=TRUE),
         linetype=guide_legend(nrow=2,byrow=TRUE))+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank())
p2
p3<-ggplot(d_se)+geom_line(aes(x=to/10, y=net_dr, 
                               color=label_line, 
                               linetype=label_line,
                               group=label))+
  scale_color_manual("Evolution scenario", values=evo_type_color)+
  scale_linetype_manual("Evolution scenario", values=evo_type_line)+
  guides(color=guide_legend(nrow=2,byrow=TRUE),
         linetype=guide_legend(nrow=2,byrow=TRUE))+
  labs(x=x_label, y="Net per capita diversification rate")+
  theme_bw()+
  theme(legend.title = element_blank())
p3

pp1<-ggarrange(plotlist=list(p1, p2, p3), nrow=3, ncol=1, common.legend = T, legend = "bottom")
pp1


d<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data_raw.rda")
d<-formatLabels(d)
d_se_all<-readRDS("../Figures/Figure3.SP.EX.SPECIES/figure_data.rda")
d_se_all<-formatLabels(d_se_all)

d_se_all<-d_se_all[is.na(nb) & is.na(da)]
d_se_all_conser<-d_se_all[species_evo_type==1]
d_se_all$label_speciation_color<-"Greater"
d_se_all[N_SPECIATION<d_se_all_conser$N_SPECIATION]$label_speciation_color<-"Less"

d_se_all$label_extinction_color<-"Greater"
d_se_all[N_EXTINCTION<d_se_all_conser$N_EXTINCTION]$label_extinction_color<-"Less"

d_se_all$label_species_color<-"Greater"
d_se_all[N_SPECIES<d_se_all_conser$N_SPECIES]$label_species_color<-"Less"

d_se_all<-d_se_all[species_evo_type!=1]

type.labs <- c("net_dr"= "net per capita diversification rate",
               "R_EXTINCTION_SPECIES"=  "net extinction",
               "R_SPECIATION_SPECIES" ="net speciation")

items1<-d_se_all[, c("label_x", "N_SPECIATION", "label_speciation_color", "CI_N_SPECIATION")]
colnames(items1)<-c("label_x", "N", "label_color", "CI_N")
items1$type<-"Average number of speciations"

items2<-d_se_all[, c("label_x", "N_EXTINCTION", "label_extinction_color", "CI_N_EXTINCTION")]
colnames(items2)<-c("label_x", "N", "label_color", "CI_N")
items2$type<-"Average number of extinctions"

items3<-d_se_all[, c("label_x", "N_SPECIES", "label_species_color", "CI_N_SPECIES")]
colnames(items3)<-c("label_x", "N", "label_color", "CI_N")
items3$type<-"Average number of species"

gg_item<-rbindlist(list(items1, items2, items3))
gg_item$type<-factor(gg_item$type, levels = c("Average number of species", 
                                              "Average number of extinctions",
                                              "Average number of speciations"))

items1<-d_se_all_conser[, c("label_x", "N_SPECIATION", "CI_N_SPECIATION")]
colnames(items1)<-c("label_x", "N", "CI_N")
items1$type<-"Average number of speciations"

items2<-d_se_all_conser[, c("label_x", "N_EXTINCTION", "CI_N_EXTINCTION")]
colnames(items2)<-c("label_x", "N", "CI_N")
items2$type<-"Average number of extinctions"

items3<-d_se_all_conser[, c("label_x", "N_SPECIES", "CI_N_SPECIES")]
colnames(items3)<-c("label_x", "N", "CI_N")
items3$type<-"Average number of species"

gg_item_cons<-rbindlist(list(items1, items2, items3))
gg_item_cons$type<-factor(gg_item_cons$type, levels = c("Average number of species", 
                                                        "Average number of extinctions",
                                                        "Average number of speciations"))
ggplot()+geom_rect(data=gg_item_cons, aes(xmin=N - CI_N,
                                          xmax=N + CI_N,
                                          ymin=min(as.integer(gg_item$label_x)),
                                          ymax=max(as.integer(gg_item$label_x))),
                   fill="black")


p1<-ggplot()+
  geom_rect(data=gg_item_cons, aes(xmin=N - CI_N,
                                   xmax=N + CI_N,
                                   ymin=min(as.integer(gg_item$label_x))-1.2,
                                   ymax=max(as.integer(gg_item$label_x))-0.8),
            fill="grey", alpha=0.2)+
  geom_errorbarh(data=gg_item, 
                 aes(y=label_x, color=label_color, width=0.2, 
                    xmin=N-CI_N, 
                    xmax=N+CI_N))+
  geom_vline(data=gg_item_cons, aes(xintercept = N), linetype=2)+
  
  geom_point(data=gg_item, aes(y=label_x, x=N, color=label_color), size=0.5)+
  scale_y_discrete(limits=rev)+
  scale_color_manual(values=c("#EE6677", "#000000", "#4477AA"), 
                     breaks=c("Greater", "No sig diff", "Less"))+
  theme_bw()+
  facet_wrap(~type, nrow=1, ncol=3, scale="free_x")+
  guides(y = guide_axis_nested(delim = "&"))+
  theme(ggh4x.axis.nestline.y = element_line(linewidth = 1.1),
        ggh4x.axis.nesttext.y = element_text(size = 10, face ="bold"),
        legend.position = "none", 
        #legend.box="horizontal",
        #legend.background = element_rect(fill=bg),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.spacing.x = unit(5, "mm"))
p1

ggsave(p1, filename="../Figures/Figure2.combined/item2.png", width=12, height=3)


df_result<-readRDS("../Figures/20230616/TukeyHSD/TukeyHSD_by_species.rda")
df_result$label<-gsub("-conservatism", "", df_result_list$label)
df_result<-formatLabelX(df_result_list)
df_result<-df_result[is.na(NB) & is.na(DA)]

df_result<-df_result[type!="N_SPECIES"]
unique(df_result$type)
type.labs <- c("net_dr"= "net per capita diversification rate",
               "R_EXTINCTION_SPECIES"=  "net extinction",
               "R_SPECIATION_SPECIES" ="net speciation")

df_result[type!="net_dr"]$diff <- df_result[type!="net_dr"]$diff/1000
df_result[type!="net_dr"]$lwr <- df_result[type!="net_dr"]$lwr/1000
df_result[type!="net_dr"]$upr <- df_result[type!="net_dr"]$upr/1000

df_result$diff_str<-round(df_result$diff, 3)
df_result$alternative<-""
df_result[diff>0]$alternative<-"Greater"
df_result[diff<0]$alternative<-"Less"
df_result[p_label==""]$alternative<-"No sig diff"



table(df_result$alternative)
df_result$label<-gsub("-conservatism", "", df_result$label)
df_result$diff_label<-sprintf("%.3f %s", df_result$diff_str, df_result$p_label)
p<-ggplot(df_result)+geom_errorbarh(aes(y=label_x, xmin=lwr, xmax=upr, color=alternative), height=0.1)+
  geom_vline(aes(xintercept=0), linetype=2, color="#444444")+
  geom_point(aes(y=label_x, x=diff, color=alternative), size=0.5)+
  scale_y_discrete(limits=rev)+
  facet_wrap(~type, scale="free_x", labeller = 
               labeller(type = type.labs), nrow=1, ncol=3)+
  scale_color_manual(values=c("#EE6677", "#000000", "#4477AA"), 
                     breaks=c("Greater", "No sig diff", "Less"))+
  theme_bw()+
  guides(y = ggh4x::guide_axis_nested(delim = "&"))+
  theme(legend.position = c(0.95, 0.25),
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
        panel.spacing.x = unit(5, "mm"))

#scale_x_discrete(guide = guide_axis(n.dodge = 2))
p


ggsave(p, filename="../Figures/Figure2.combined/item3.png",
       width=12, height=3)

p_all<-ggarrange(plotlist=list(pp1, p1, p), nrow=3, ncol=1, heights = c(3, 1, 1),
                 labels=c("a", "b", "c"))

ggsave(p_all, filename="../Figures/Figure2.combined/Figure2.combined.pdf",
       width=12, height=12)

