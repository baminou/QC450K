hybridization <- function(shinySummarizedData,sd.multiplier = 6,plot=FALSE,output_file=NULL){
  

  hyb.green <- data.frame(shinySummarizedData@greenControls$HYBRIDIZATION)
  hyb.red <- data.frame(shinySummarizedData@redControls$HYBRIDIZATION)
  
  rownames(hyb.green) <- c("High","Medium","Low")
  rownames(hyb.red) <- c("High","Medium","Low")
  
  sdev <- sapply(X = hyb.red,FUN = function(x)sd(x,na.rm=T))
  avg <- sapply(X = hyb.red,FUN = function(x)mean(x,na.rm=T))

  outliers <- hyb.green["High",]>avg+sd.multiplier*sdev & hyb.green["Medium",]>avg+sd.multiplier*sdev & hyb.green["Low",]>avg+sd.multiplier*sdev &
    hyb.green["High",]>avg & hyb.green["Medium",] > avg & hyb.green["Low",] > avg

  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }

    ylimits <- max(c(t(hyb.green),t(hyb.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    matplot(t(hyb.green),type="p",pch=22,col="black",bg=c("green","blue","black"),main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    lines(avg+sdev*sd.multiplier,col="black",lwd=2)
    points(avg+sdev*sd.multiplier,col="black",pch="-",lwd=2)
    matplot(t(hyb.red),type="p",pch=22,col="black",bg=c("green","blue","black"),main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    lines(avg+sdev*sd.multiplier,col="black",lwd=2)
    points(avg+sdev*sd.multiplier,col="black",pch="-",lwd=2)
    legend("topright",rownames(hyb.green),pch=22,pt.bg = c("green","blue","black"),inset=c(-0.6,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))

    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers))
}
