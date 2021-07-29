bind<-function(df1, df2){
  if (is.null(df1)){
    df2
  }else{
    rbindlist(list(df1, df2))
  }
}