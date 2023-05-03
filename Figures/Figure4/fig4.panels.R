library(ggplot2)
library(ggthemes)
library(data.table)
library(ggpubr)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
d_se<-readRDS("../Figures/N_Species/figure_data.rda")
d_se_all<-readRDS("../Figures/N_Species/figure_data_all.rda")
d_se_seed<-readRDS("../Figures/N_Species/d_se_seed.rda")
source("commons/functions.r")
#1: conservatism
#2: shift-directional
#3: expansion-directional
#4: expansion-omnidirectional
#5: random-central
#6: random-symmetrical
#7: random-asymmetrical
d_se<-d_se[species_evo_level==0]
d_se$evo_type<-format_evoType(d_se$species_evo_type)

d_se_all<-d_se_all[species_evo_level==0]
d_se_all$evo_type<-format_evoType(d_se_all$species_evo_type)


cor(d_se$N_SPECIES, d_se$N_SPECIATION)

cols<-c("MEDIAN_N_SPECIES", "QUANTILE_25_N_SPECIES", "QUANTILE_75_N_SPECIES", 
        "species_evo_level", "species_evo_type", "directional_speed", "year", "evo_type")
d_se_all_fig1<-d_se_all[, ..cols]
colnames(d_se_all_fig1)[1:3]<-c("N", "Q_25", "Q_75")
d_se_all_fig1$type<-"Median number of species"

cols<-c("MEDIAN_N_SPECIATION", "QUANTILE_25_N_SPECIATION", "QUANTILE_75_N_SPECIATION", 
        "species_evo_level", "species_evo_type", "directional_speed", "year", "evo_type")
d_se_all_fig2<-d_se_all[, ..cols]
colnames(d_se_all_fig2)[1:3]<-c("N", "Q_25", "Q_75")
d_se_all_fig2$type<-"Median number of speciation"

cols<-c("MEDIAN_N_EXTINCTION", "QUANTILE_25_N_EXTINCTION", "QUANTILE_75_N_EXTINCTION",  
        "species_evo_level", "species_evo_type", "directional_speed", "year", "evo_type")
d_se_all_fig3<-d_se_all[, ..cols]
colnames(d_se_all_fig3)[1:3]<-c("N", "Q_25", "Q_75")
d_se_all_fig3$type<-"Median number of extinction"
d_se_all_fig<-rbindlist(list(d_se_all_fig1, d_se_all_fig2, d_se_all_fig3))
d_se_all_fig$type<-factor(d_se_all_fig$type, 
                          levels=rev(c("Median number of species", 
                                       "Median number of speciation", 
                                       "Median number of extinction")))
d_se_all_fig$label<-format_evoType_amplitude(d_se_all_fig$evo_type, d_se_all_fig$directional_speed, order=-1)


p1<-ggplot(d_se_all_fig[year==0])+
  geom_bar(aes(y=label, x=N, fill=type), position="dodge2", stat = "identity")+
  theme_bw()+
  labs(fill="", x="Median number of species, speciation or extinction", y=NULL)+
  #scale_x_reverse(expand = c(0, 0), limits = c(max(d_se_all_fig[year==0]$N), NA),
  #                breaks = c(0, 1e5, 2e5),
  #                labels = c("0", "1 E+5", "2 E+5"))+
  #scale_x_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_fill_manual(values=safe_colorblind_palette[c(1:3)])+
  scale_y_discrete(position = "left")+
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_line(colour = "black"),
        #axis.text.y=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0.05), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null"))
p1

d_se$label2<-format_evoType_amplitude(d_se$evo_type, d_se$directional_speed, order=-1)
#d_se$label<-factor(d_se$label, levels = c("B"))
p2<-ggplot(d_se[year==0&species_evo_level==0])+
  geom_bar(aes(y=label2, x=MEDIAN_N_SPECIES, fill=label), 
           stat = "identity", position = position_dodge(0.9))+
  #geom_errorbarh()+
  labs(x="Number of species", fill="")+
  scale_fill_manual(values=colorBlindGrey8[c(2, 3, 4, 8)])+
  theme_bw()+
  #scale_x_continuous(expand = c(0, 0), limits = c(0, NA), 
  #                   breaks = c(0, 5e4, 1e5),
  #                   labels = c("0", "5 E+4", "1 E+5"))+
  scale_y_discrete(labels = scales::label_wrap(10))+
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        #axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.text.y=element_text(size=10),
        axis.text.y.left = element_text(margin = margin(0, 0, 0, 0), hjust = 0.5),
        plot.margin = unit(c(0, 0.05, 0, 0), "null"),
        panel.spacing = unit(c(0, 0, 0, 0), "null"))

p2
p<-ggarrange(p1, p2, widths=c(1, 1.3))
ggsave(p, filename="../Figures/Figure4/fig.4.png", width=12, height=6)
