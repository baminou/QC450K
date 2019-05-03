targetRemoval <- function(shinySummarizedData,sd.multiplier = 6,plot=FALSE,output_file=NULL){
  
  tr.green <- data.frame(shinySummarizedData@greenControls$`TARGET REMOVAL`)
  tr.red <- data.frame(shinySummarizedData@redControls$`TARGET REMOVAL`)
  
  hyb.green <- data.frame(shinySummarizedData@greenControls$HYBRIDIZATION)
  hyb.red <- data.frame(shinySummarizedData@redControls$HYBRIDIZATION)
  
  rownames(hyb.green) <- c("High","Medium","Low")
  rownames(hyb.red) <- c("High","Medium","Low")
  
  rownames(tr.green) <- c("Target Removal I","Target Removal II")
  rownames(tr.red) <- c("Target Removal I","Target Removal II")
  
  sd.green <- sapply(X = tr.green,FUN = function(x)sd(x,na.rm=T))
  sd.red <- sapply(X = tr.red,FUN = function(x)sd(x,na.rm=T))
  
  mean.green <- sapply(X = tr.green,FUN = function(x)mean(x,na.rm=T))
  mean.red <- sapply(X = tr.red,FUN = function(x)mean(x,na.rm=T))
  
  outliers.green <- hyb.green["High",] > mean.green+sd.green*sd.multiplier & 
    hyb.green["Medium",] > mean.green+sd.green*sd.multiplier &
    hyb.green["Low",] > mean.green+sd.green*sd.multiplier & 
    hyb.green["High",] > hyb.green["Medium",] &
    hyb.green["Medium",] > hyb.green["Low",]
  
  outliers.red <- hyb.green["High",] > mean.red+sd.red*sd.multiplier & 
    hyb.green["Medium",] > mean.red+sd.red*sd.multiplier &
    hyb.green["Low",] > mean.red+sd.red*sd.multiplier & 
    hyb.green["High",] > hyb.green["Medium",] &
    hyb.green["Medium",] > hyb.green["Low",]
  
  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    
    ylimits <- max(c(t(hyb.green),t(hyb.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    matplot(t(rbind(hyb.green,tr.green)),type="p",pch=22,col="black",bg=c("green","blue","black","red","orange"),main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    lines(mean.green+sd.green*sd.multiplier,col="black",lwd=2)
    points(mean.green+sd.green*sd.multiplier,col="black",pch="-",lwd=2)
    matplot(t(rbind(hyb.green,tr.red)),type="p",pch=22,col="black",bg=c("green","blue","black","red","orange"),main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    lines(mean.red+sd.red*sd.multiplier,col="black",lwd=2)
    points(mean.red+sd.red*sd.multiplier,col="black",pch="-",lwd=2)
    legend("topright",rownames(rbind(hyb.green,tr.green)),pch=22,pt.bg = c("green","blue","black","red","orange"),inset=c(-1,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))
    
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers.green==TRUE&outliers.red==TRUE))
}
