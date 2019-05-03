nonPolymorphic <- function(shinySummarizedData,sd.multiplier = 6,plot=FALSE,output_file=NULL){

  np.green <- data.frame(shinySummarizedData@greenControls$`NON-POLYMORPHIC`)
  np.red <- data.frame(shinySummarizedData@redControls$`NON-POLYMORPHIC`)
  
  rownames(np.green) <- c("A","T","C","G")
  rownames(np.red) <- c("A","T","C","G")
  
  sd.green <- sapply(X = np.green,FUN = function(x)sd(x[1:2],na.rm=T))
  sd.red <- sapply(X = np.red,FUN = function(x)sd(x[3:4],na.rm=T))
  
  mean.green <- sapply(X = np.green,FUN = function(x)mean(x[1:2],na.rm=T))
  mean.red <- sapply(X = np.red,FUN = function(x)mean(x[3:4],na.rm=T))
  
  outliers.green <- np.green["C",] > mean.green+sd.green*sd.multiplier & np.green["G",] > mean.green+sd.green*sd.multiplier
  outliers.red <- np.red["A",] > mean.red+sd.red*sd.multiplier & np.red["T",] > mean.red+sd.red*sd.multiplier

  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    
    ylimits <- max(c(t(np.green),t(np.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    col.green <- c("red","purple","green","blue")
    matplot(t(np.green),type="p",pch=22,col="black",bg=col.green,main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    lines(mean.green+sd.green*sd.multiplier,col="black",lwd=2)
    points(mean.green+sd.green*sd.multiplier,col="black",pch="-",lwd=2)
    matplot(t(np.red),type="p",pch=22,col="black",bg=col.green,main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    lines(mean.red+sd.red*sd.multiplier,col="black",lwd=2)
    points(mean.red+sd.red*sd.multiplier,col="black",pch="-",lwd=2)
    legend("topright",rownames(np.green),pch=22,pt.bg = col.green,inset=c(-0.6,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))
    
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers.green==TRUE|outliers.red==TRUE))
}