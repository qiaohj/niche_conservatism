library(rgdal)
mask<-readOGR("/home/huijieqiao/git/ees_3d_data/SMART_SPECIES/ISEA3H8/isea3hGen/outputfiles", "isea3h8p")
global_id_lonlat<-data.frame(mask, stringsAsFactors = F)
global_id_lonlat$global_id<-as.numeric(as.character(global_id_lonlat$global_id))
colnames(global_id_lonlat)<-c("global_id", "lon", "lat")
saveRDS(global_id_lonlat, "/home/huijieqiao/git/ees_3d_data/Commons/global_id_lonlat.rda")

base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"

min_temp<-readRDS(sprintf("%s/Data/ENV/min_temp.rda", base))
max_temp<-readRDS(sprintf("%s/Data/ENV/max_temp.rda", base))
max_prec<-readRDS(sprintf("%s/Data/ENV/max_prec.rda", base))
global_id_lonlat<-readRDS("/home/huijieqiao/git/ees_3d_data/Commons/global_id_lonlat.rda")
global_id_lonlat<-global_id_lonlat[,c("global_id", "lon", "lat")]

mask_df<-left_join(global_id_lonlat, min_temp, by=c("global_id"="ID"))
#ggplot(mask_df %>% filter(Y==0))+geom_point(aes(x=lon, y=lat, color=MIN_TEMP))
mask_df<-left_join(mask_df, max_temp, by=c("global_id"="ID", "Y"="Y"))
mask_df<-left_join(mask_df, max_prec, by=c("global_id"="ID", "Y"="Y"))
mask_df<-mask_df[, c("global_id", "lon", "lat", "Y", "MIN_TEMP", "MAX_TEMP", "MAX_PREC")]
saveRDS(mask_df, sprintf("%s/Data/ENV/mask_df.rda", base))
