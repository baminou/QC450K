negative <- function(shinySummarizedData,channel=0,plot=FALSE,p.treshold=0.01,output_file=NULL){

  answer <- TRUE
  
  set.seed(2)
  if(channel==0){
    neg <- data.frame(shinySummarizedData@greenControls$NEGATIVE)
  }else{
    neg <- data.frame(shinySummarizedData@redControls$NEGATIVE)
  }
  
  #TODO
  
  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    if(channel==0){
      matplot(colMeans(neg),type="p",pch=22,col="black",bg=rainbow(1),main="NEGATIVE - Green",ylab = "Intensity",xlab="Sample",ylim = c(0,max(colMeans(neg))))
    }else{
      matplot(colMeans(neg),type="p",pch=22,col="black",bg=rainbow(1),main="NEGATIVE - Red",ylab = "Intensity",xlab="Sample",ylim = c(0,max(colMeans(neg))))
    }
    points(colSds(as.matrix(neg)),type="p",pch=22,col="black",bg="black")
    
    abline(v=1:(length(summarized.data@sampleNames)),col="grey")
    legend("topright",c("Average","StdDev"),pch=22,pt.bg=c(rainbow(1),"black"))
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(FALSE)
}