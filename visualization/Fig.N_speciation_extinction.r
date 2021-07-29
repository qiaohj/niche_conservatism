library(ggplot2)
library(data.table)
library(RSQLite)
library(DBI)
library(ape)
library(phangorn)
library(phytools)
library(geiger)
library(stringr)
library(tidyverse)
library(plotKML)
library(ggtree)
library(RColorBrewer)

setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
source("commons/functions.r")

df<-readRDS("../Data/Sample_N_Narrow.rda")

df$N_EXTINCTION_Ratio<-ifelse(df$N_EXTINCTION_null==0, 
                              NA, df$N_EXTINCTION/df$N_EXTINCTION_null)
df$N_SPECIATION_Ratio<-ifelse(df$N_SPECIATION_null==0, 
                              NA, df$N_SPECIATION/df$N_SPECIATION_null)
df$N_SPECIES_Ratio<-ifelse(df$N_SPECIES_null==0, 
                              NA, df$N_SPECIES/df$N_SPECIES_null)

df_se<-df[, .(N_EXTINCTION_mean = mean(N_EXTINCTION),
              N_EXTINCTION_sd = sd(N_EXTINCTION),
              N_EXTINCTION_ratio_mean = mean(N_EXTINCTION_Ratio, na.rm = T),
              N_EXTINCTION_ratio_sd = sd(N_EXTINCTION_Ratio, na.rm = T),
              N_SPECIATION_mean = mean(N_SPECIATION),
              N_SPECIATION_sd = sd(N_SPECIATION),
              N_SPECIATION_ratio_mean = mean(N_SPECIATION_Ratio, na.rm = T),
              N_SPECIATION_ratio_sd = sd(N_SPECIATION_Ratio, na.rm = T),
              N_SPECIES_mean = mean(N_SPECIES),
              N_SPECIES_sd = sd(N_SPECIES),
              N_SPECIES_ratio_mean = mean(N_SPECIES_Ratio, na.rm = T),
              N_SPECIES_ratio_sd = sd(N_SPECIES_Ratio, na.rm = T)),
          by = c("year", "species_evo_level", 
                 "species_evo_type", "directional_speed")]

df_se_t<-df_se[(species_evo_type==1)&(directional_speed==0)]
df_se_t$species_evo_level<-1
df_se<-rbind(df_se, df_se_t)
df_se[species_evo_type==1]$N_EXTINCTION_ratio_mean
df_se[is.nan(N_SPECIATION_ratio_mean)]$N_SPECIATION_ratio_mean<-0
df_se[is.na(N_SPECIATION_ratio_sd)]$N_SPECIATION_ratio_sd<-0
df_se$species_evo_level_lab<-ifelse(df_se$species_evo_level==0, 
                                    "Species based", "Group based")

niche_labels<-c(
  "niche conservatism",
  "niche shift (directional)",
  "niche expansion (directional)",
  "niche expansion (omnidirectional)",
  "niche shift (random in box center)",
  "niche shift (random symmetrical change in box limit)",
  "niche shift (random asymmetrical change in box limit)")

niche_colors<-brewer.pal(7, "Dark2")
names(niche_colors)<-as.character(c(1:7))
p<-ggplot(df_se[species_evo_type!=1])+
  geom_line(aes(x=-1*year, y=N_EXTINCTION_ratio_mean, 
                color=as.character(species_evo_type)),size=1)+
  #scale_y_log10()+
  ggtitle("EXTINCTION Ratio")+
  labs(x="Time step", y="EXTINCTION/EXTINCTION NULL")+
  scale_color_manual(labels=niche_labels[2:7], 
                     values=niche_colors[2:7], name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")
p
ggsave(p, filename = "../Figures/sample_N/N_Extinction_ratio_NARROW.png", width=10, height=5)
p<-p+
  geom_ribbon(aes(x=-1*year, 
                  ymin=N_EXTINCTION_ratio_mean - N_EXTINCTION_ratio_sd,
                  ymax=N_EXTINCTION_ratio_mean + N_EXTINCTION_ratio_sd, 
                  fill=as.character(species_evo_type)), alpha=0.2)+
  scale_fill_manual(labels=niche_labels[2:7], 
                    values=niche_colors[2:7], name="evo type")
p
ggsave(p, filename = "../Figures/sample_N/N_Extinction_ratio_with_SD_NARROW.png", width=10, height=5)

p<-ggplot(df_se[species_evo_type!=1])+
  geom_line(aes(x=-1*year, y=N_SPECIATION_ratio_mean, 
                color=as.character(species_evo_type)),size=1)+
  #scale_y_log10()+
  ggtitle("SPECIATION Ratio")+
  labs(x="Time step", y="SPECIATION/SPECIATION NULL")+
  scale_color_manual(labels=niche_labels[2:7], 
    values=niche_colors[2:7], name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIATION_ratio_NARROW.png", width=10, height=5)
p<-p+
  geom_ribbon(aes(x=-1*year, 
                  ymin=N_SPECIATION_ratio_mean - N_SPECIATION_ratio_sd,
                  ymax=N_SPECIATION_ratio_mean + N_SPECIATION_ratio_sd, 
                  fill=as.character(species_evo_type)), alpha=0.2)+
  scale_fill_manual(labels=niche_labels[2:7], 
    values=niche_colors[2:7], name="evo type")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIATION_ratio_with_SD_NARROW.png", width=10, height=5)

p<-ggplot(df_se[species_evo_type!=1])+
  geom_line(aes(x=-1*year, y=N_SPECIES_ratio_mean, 
                color=as.character(species_evo_type)),size=1)+
  #scale_y_log10()+
  ggtitle("SPECIES Ratio")+
  labs(x="Time step", y="SPECIES/SPECIES NULL")+
  scale_color_manual(labels=niche_labels[2:7], 
    values=niche_colors[2:7], name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIES_ratio_NARROW.png", width=10, height=5)
p<-p+
  geom_ribbon(aes(x=-1*year, 
                  ymin=N_SPECIES_ratio_mean - N_SPECIES_ratio_sd,
                  ymax=N_SPECIES_ratio_mean + N_SPECIES_ratio_sd, 
                  fill=as.character(species_evo_type)), alpha=0.2)+
  scale_fill_manual(labels=niche_labels[2:7], 
    values=niche_colors[2:7], name="evo type")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIES_ratio_with_SD_NARROW.png", width=10, height=5)

#Raw number

p<-ggplot(df_se)+
  geom_line(aes(x=-1*year, y=N_EXTINCTION_mean, 
                color=as.character(species_evo_type)),size=1)+
  scale_y_log10()+
  ggtitle("EXTINCTION")+
  labs(x="Time step", y="EXTINCTION")+
  scale_color_manual(labels=niche_labels, 
    values=niche_colors, name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")
p
ggsave(p, filename = "../Figures/sample_N/N_Extinction.png_NARROW", width=12, height=5)
p<-ggplot(df_se)+
  geom_line(aes(x=-1*year, y=N_EXTINCTION_mean, 
                color=as.character(species_evo_type)),size=1)+
  #scale_y_log10()+
  ggtitle("EXTINCTION")+
  labs(x="Time step", y="EXTINCTION")+
  scale_color_manual(labels=niche_labels, 
                     values=niche_colors, name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")+
  geom_ribbon(aes(x=-1*year, 
                  ymin=N_EXTINCTION_mean - N_EXTINCTION_sd,
                  ymax=N_EXTINCTION_mean + N_EXTINCTION_sd, 
                  fill=as.character(species_evo_type)), alpha=0.2)+
  scale_fill_manual(labels=niche_labels, 
    values=niche_colors, name="evo type")
p
ggsave(p, filename = "../Figures/sample_N/N_Extinction_with_SD.png_NARROW", width=12, height=5)


p<-ggplot(df_se)+
  geom_line(aes(x=-1*year, y=N_SPECIATION_mean, 
                color=as.character(species_evo_type)),size=1)+
  scale_y_log10()+
  ggtitle("SPECIATION")+
  labs(x="Time step", y="SPECIATION")+
  scale_color_manual(labels=niche_labels, 
                     values=niche_colors, name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIATION_NARROW.png", width=12, height=5)
p<-ggplot(df_se)+
  geom_line(aes(x=-1*year, y=N_SPECIATION_mean, 
                color=as.character(species_evo_type)),size=1)+
  #scale_y_log10()+
  ggtitle("SPECIATION")+
  labs(x="Time step", y="SPECIATION")+
  scale_color_manual(labels=niche_labels, 
                     values=niche_colors, name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")+
  geom_ribbon(aes(x=-1*year, 
                  ymin=N_SPECIATION_mean - N_SPECIATION_sd,
                  ymax=N_SPECIATION_mean + N_SPECIATION_sd, 
                  fill=as.character(species_evo_type)), alpha=0.2)+
  scale_fill_manual(labels=niche_labels, 
                    values=niche_colors, name="evo type")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIATION_with_SD_NARROW.png", width=12, height=5)

p<-ggplot(df_se)+
  geom_line(aes(x=-1*year, y=N_SPECIES_mean, 
                color=as.character(species_evo_type)),size=1)+
  scale_y_log10()+
  ggtitle("SPECIES")+
  labs(x="Time step", y="SPECIES")+
  scale_color_manual(labels=niche_labels, 
                     values=niche_colors, name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIES_NARROW.png", width=12, height=5)
p<-ggplot(df_se)+
  geom_line(aes(x=-1*year, y=N_SPECIES_mean, 
                color=as.character(species_evo_type)),size=1)+
  #scale_y_log10()+
  ggtitle("SPECIES")+
  labs(x="Time step", y="SPECIES")+
  scale_color_manual(labels=niche_labels, 
                     values=niche_colors, name="evo type")+
  facet_grid(species_evo_level_lab~directional_speed, switch="x", scale="free")+
  theme_bw()+
  theme(legend.position="bottom")+
  geom_ribbon(aes(x=-1*year, 
                  ymin=N_SPECIES_mean - N_SPECIES_sd,
                  ymax=N_SPECIES_mean + N_SPECIES_sd, 
                  fill=as.character(species_evo_type)), alpha=0.2)+
  scale_fill_manual(labels=niche_labels, 
                    values=niche_colors, name="evo type")
p
ggsave(p, filename = "../Figures/sample_N/N_SPECIES_with_SD_NARROW.png", width=12, height=5)
