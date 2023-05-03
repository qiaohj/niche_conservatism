library(data.table)
library(ggplot2)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
df<-readRDS("../Data/N_speciation_extinction/N_speciation_extinction.rda")
head(df)
df_last_year<-df[year==0]

d_se2<-df_last_year[, .(MEAN_N_SPECIES=mean(N_SPECIES),
             SD_N_SPECIES=sd(N_SPECIES),
             MIN_N_SPECIES=min(N_SPECIES),
             MAX_N_SPECIES=max(N_SPECIES),
             IQR_N_SPECIES=IQR(N_SPECIES),
             MEDIAN_N_SPECIES=quantile(N_SPECIES, 0.5),
             QUANTILE_25_NCELL=quantile(N_SPECIES, 0.25),
             QUANTILE_75_NCELL=quantile(N_SPECIES, 0.75)),
         by=list(year, species_evo_type, directional_speed, species_evo_level, nb, da)]

d_se2$Threshold_IQR<-d_se2$QUANTILE_75_NCELL + d_se2$IQR_N_SPECIES * 1.5
d_se2$Threshold_3SD<-d_se2$MEAN_N_SPECIES + d_se2$SD_N_SPECIES * 3
plot(d_se2$Threshold_IQR, d_se2$Threshold_3SD)

df_last_year_with_threshold<-merge(df_last_year, d_se2, by=c("year", "species_evo_type", "directional_speed", 
                                              "species_evo_level", "nb", "da"))
df_last_year_with_threshold$IS_IQR_OUTLIERS<-
  df_last_year_with_threshold$N_SPECIES>df_last_year_with_threshold$Threshold_IQR
df_last_year_with_threshold$IS_3SD_OUTLIERS<-
  df_last_year_with_threshold$N_SPECIES>df_last_year_with_threshold$Threshold_3SD
d_se2$label<-paste(d_se2$species_evo_type, d_se2$directional_speed, 
                   d_se2$species_evo_level, d_se2$nb, d_se2$da)
ggplot(d_se2[Threshold_3SD>500])+geom_point(aes(x=label, y=Threshold_3SD))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))

ggplot(d_se2[Threshold_IQR>100])+geom_point(aes(x=label, y=Threshold_IQR))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


outliers<-d_se2[(nb=="BROAD") & (species_evo_type %in% c(1:4)) & 
                  (directional_speed %in% c(0, 0.1, 0.5))]
outliers<-df_last_year_with_threshold[IS_3SD_OUTLIERS==T &
  (nb=="BROAD") & (species_evo_type %in% c(1:4)) & 
                  (directional_speed %in% c(0, 0.1, 0.5))]

seeds<-readRDS("../Data/seeds.rda")

outliers<-merge(outliers, seeds, by="global_id")
outliers_final<-outliers[lat>30]

library(rnaturalearth)
world <- ne_countries(scale = "small", returnclass = "sf")
plot(world$geometry)
points(outliers_final$lon, outliers_final$lat, col="red")
length(unique(outliers_final$global_id))
length(unique(outliers$global_id))

saveRDS(df_last_year_with_threshold, "../Data/outliers/n_species_last_year_with_threshold.rda")
saveRDS(outliers_final, "../Data/outliers/outliers_3SD.rda")


outliers<-df_last_year_with_threshold[IS_IQR_OUTLIERS==T &
                                        (nb=="BROAD") & (species_evo_type %in% c(1:4)) & 
                                        (directional_speed %in% c(0, 0.1, 0.5))]


outliers<-merge(outliers, seeds, by="global_id")
outliers_final<-outliers[lat>30]

library(rnaturalearth)
world <- ne_countries(scale = "small", returnclass = "sf")
plot(world$geometry)
points(outliers_final$lon, outliers_final$lat, col="red")
length(unique(outliers_final$global_id))
length(unique(outliers$global_id))

saveRDS(outliers_final, "../Data/outliers/outliers_IQR.rda")
