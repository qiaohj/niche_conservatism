
con = file("/home/huijieqiao/git/ees_3d_data/TEST/test_disjoint_set.log", "r")

df<-data.frame()
year_df<-data.frame()
while ( TRUE ) {
  line = readLines(con, n = 1)
  
  if ( length(line) == 0 ) {
    break
  }
  if (grepl("INFO v3.3 Current year:", line)){
    year <- as.numeric(strsplit(strsplit(line, ":")[[1]][4], " @ ")[[1]][1])
    print(year)
    if (nrow(df)==0){
      df<-year_df
    }else{
      df<-rbind(df, year_df)
    }
    year_df<-data.frame()
  }
  if (grepl("INFO GROUP", line)){
    items <- strsplit(gsub(",", "", line), " ")[[1]]
    group1<-as.numeric(items[6])
    group2<-as.numeric(items[9])
  }
  if (grepl("INFO OLD METHODS TOOK", line)){
    items <- strsplit(gsub(",", "", line), " ")[[1]]
    old_time = as.numeric(items[7])
    old_total_time = as.numeric(items[16])
  }
  
  if (grepl("INFO NEW METHODS TOOK", line)){
    items <- strsplit(gsub(",", "", line), " ")[[1]]
    new_time = as.numeric(items[7])
    new_total_time = as.numeric(items[16])
    item_df<-data.frame(year = year, group1=group1, group2=group2, 
                        old_time=old_time, old_total_time=old_total_time,
                        new_time=new_time, new_total_time=new_total_time)
    if (nrow(year_df)==0){
      year_df<-item_df
    }else{
      year_df<-rbind(year_df, item_df)
    }
  }
  
}

close(con)
tail(df)
df$rate<-df$old_time/df$new_time
hist(df$rate)
write.table(df, "/home/huijieqiao/git/ees_3d_data/TEST/test_disjoint_set.csv", row.names = F, sep=",")

df<-read.table("/home/huijieqiao/git/ees_3d_data/TEST/test_disjoint_set.csv", head=T, sep=",", stringsAsFactors = F)

head(df[which((df$old_time>0.1)&(df$old_time<df$new_time)),], 100)

df[which(df$rate==min(df$rate)),]
range(df$rate)

library(dplyr)
result <- df %>% 
  group_by(year) %>%
  filter(old_total_time == max(old_total_time))
df_year<-rbind(data.frame(year=result$year, total_time=result$old_total_time, type="TREE"),
               data.frame(year=result$year, total_time=result$new_total_time, type="DISJOINT SET"))
library(ggplot2)
df_year$year<-df_year$year * -1
ggplot(df_year)+geom_line(aes(x=year, y=total_time, color=factor(type)))
