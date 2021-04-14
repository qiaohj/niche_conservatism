library("dplyr")
library("ggplot2")
library("Rmisc")
base<-"/home/huijieqiao/git/ees_3d_data/niche_conservatism"


year=100
rep=100

i=50
range_df<-NULL

for (i in c(1:rep)){ 
  print(i)
  sub_df<-readRDS(sprintf("%s/Data/items_rep/sp_character_rep_%d.rda", base, i))
  item<-sub_df%>%
    dplyr::group_by(DA, NB, WARP_LABEL, EVO_TYPE, EVO_RATIO, REP, Y)%>%
    dplyr::summarise(MEAN_RANG_TEMP=mean(RANG_TEMP),
                     SD_RANG_TEMP=sd(RANG_TEMP),
                     CI_RANG_TEMP=CI(RANG_TEMP)[2]-CI(RANG_TEMP)[3],
                     MEAN_RANG_PREC=mean(RANG_PREC),
                     SD_RANG_PREC=sd(RANG_PREC),
                     CI_RANG_PREC=CI(RANG_PREC)[2]-CI(RANG_PREC)[3],
                     MEAN_MAX_TEMP=mean(MAX_TEMP),
                     SD_MAX_TEMP=sd(MAX_TEMP),
                     CI_MAX_TEMP=CI(MAX_TEMP)[2]-CI(MAX_TEMP)[3],
                     MEAN_MIN_TEMP=mean(MIN_TEMP),
                     SD_MIN_TEMP=sd(MIN_TEMP),
                     CI_MIN_TEMP=CI(MIN_TEMP)[2]-CI(MIN_TEMP)[3],
                     MEAN_MAX_PREC=mean(MAX_PREC),
                     SD_MAX_PREC=sd(MAX_PREC),
                     CI_MAX_PREC=CI(MAX_PREC)[2]-CI(MAX_PREC)[3],
                     MEAN_MIN_PREC=mean(MIN_PREC),
                     SD_MIN_PREC=sd(MIN_PREC),
                     CI_MIN_PREC=CI(MIN_PREC)[2]-CI(MIN_PREC)[3],
                     MEAN_AREA=mean(AREA),
                     SD_AREA=sd(AREA),
                     CI_AREA=CI(AREA)[2]-CI(AREA)[3],
                     MEAN_RANGE_LAT=mean(RANG_LAT),
                     SD_RANGE_LAT=sd(RANG_LAT),
                     CI_RANGE_LAT=CI(RANG_LAT)[2]-CI(RANG_LAT)[3],
                     MEAN_RANGE_LON=mean(RANG_LON),
                     SD_RANGE_LON=sd(RANG_LON),
                     CI_RANGE_LON=CI(RANG_LON)[2]-CI(RANG_LON)[3]
    )
  
  if (is.null(range_df)){
    range_df<-item
  }else{
    range_df<-bind_rows(range_df, item)
  }
  
}

saveRDS(range_df, sprintf("%s/Data/range_df.rda", base))

