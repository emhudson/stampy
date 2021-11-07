#test dataframe
stamp_data<-data.frame(startVal=c(37,13),stampVal=c(3,13),divisor=c(2,10),remaining=c(0,0))
PlotStamps<-function(stamp_data,total_fare){
  lapply(1:nrow(stamp_data),function(i){
    maxCol=5 #maximum number of stamps per row
    d<-stamp_data[i,]
    stamp_grid<-
    ggplot2::ggplot(stamp_data,aes())
  
    
  })
}