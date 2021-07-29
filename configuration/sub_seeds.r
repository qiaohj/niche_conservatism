library(rgdal)
setwd("/media/huijieqiao/Butterfly/Niche_Conservatism/RScript")
seeds<-readOGR("../Shape/seeds", "seeds")

sub_seeds<-seeds[which(seeds$global_id %in% c(27790,
                                       25837,
                                       24109,
                                       54786,
                                       19759,
                                       13651,
                                       17569,
                                       18517,
                                       4497,
                                       4847,
                                       9898,
                                       11847,
                                       40440,
                                       23724)),]

writeOGR(sub_seeds, "../Shape/seeds", "sub_seeds", driver = "ESRI Shapefile")

