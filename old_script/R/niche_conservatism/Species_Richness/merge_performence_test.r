
richness$LON<-NULL
richness$LAT<-NULL
richness$MIN_TEMP<-NULL
richness$MAX_TEMP<-NULL
richness$MAX_PREC<-NULL

richness_key<-richness
system.time({setkeyv(richness_key, c("Y", "GLOBAL_ID",
                    "DA", "NB", "EVO_TYPE", "EVO_RATIO",
                    "REP"))})

df$LON<-NULL
df$LAT<-NULL
df$MIN_TEMP<-NULL
df$MAX_TEMP<-NULL
df$MAX_PREC<-NULL
df_key<-data.table(df)
print(paste(Sys.time(), "Setting df keys"))
system.time({setkeyv(df_key, c("Y", "GLOBAL_ID", 
              "DA", "NB", "EVO_TYPE", "EVO_RATIO",
              "REP"))})
system.time({
  
  df_t_key<-merge(richness_key, df_key, 
                  by=c("Y", "GLOBAL_ID", 
                       "DA", "NB", "EVO_TYPE", "EVO_RATIO",
                       "REP"), all=T)
})

indices(richness_key)

richness_index<-richness

system.time({setindexv(richness_index, c("Y", "GLOBAL_ID",
                        "DA", "NB", "EVO_TYPE", "EVO_RATIO",
                        "REP"))})
df_index<-data.table(df)
print(paste(Sys.time(), "Setting df keys"))
system.time({setindexv(df_index, c("Y", "GLOBAL_ID",
                  "DA", "NB", "EVO_TYPE", "EVO_RATIO",
                  "REP"))})

indices(richness_index)
indices(df_index)
system.time({
  
  df_t_index<-merge(richness_index, df_index, 
                  by=c("Y", "GLOBAL_ID",
                       "DA", "NB", "EVO_TYPE", "EVO_RATIO",
                       "REP"), all=T)
})
df_t_index$N_SP.x<-NULL
df_t_index$N_SP.y<-NULL
indices(df_t_index)

system.time({
  
  df_t_index2<-richness_index[df_index, on=c("Y", "GLOBAL_ID", 
                                             "DA", "NB", "EVO_TYPE", "EVO_RATIO",
                                             "REP")], nomatch=0L
})
indices(df_index)
indices(df_index)
