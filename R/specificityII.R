specificityII <- function(shinySummarizedData,sd.multiplier = 6,plot=FALSE,output_file=NULL){
  sp.green <- data.frame(shinySummarizedData@greenControls$`SPECIFICITY II`)
  sp.red <- data.frame(shinySummarizedData@redControls$`SPECIFICITY II`)
  
  rownames(sp.green) <- c("Specificity 1","Specificity 2","Specificity 3")
  rownames(sp.red) <- c("Specificity 1","Specificity 2","Specificity 3")
  
  sd.green <- sapply(X = 1:ncol(sp.green),FUN = function(x)sd(sp.green[,x],na.rm=T))
  mean.green <- sapply(X = 1:ncol(sp.green),FUN = function(x)mean(sp.green[,x],na.rm=T))
  
  outliers <- sp.red[1,] > mean.green+sd.green*sd.multiplier & 
    sp.red[2,] > mean.green+sd.green*sd.multiplier &
    sp.red[3,] > mean.green+sd.green*sd.multiplier
  
  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    
    ylimits <- max(c(t(sp.green),t(sp.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    col.green <- rainbow(3)
    matplot(t(sp.green),type="p",pch=22,col="black",bg=col.green,main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    lines(mean.green+sd.green*sd.multiplier,col="black",lwd=2)
    points(mean.green+sd.green*sd.multiplier,col="black",pch="-",lwd=2)
    matplot(t(sp.red),type="p",pch=22,col="black",bg=col.green,main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    lines(mean.green+sd.green*sd.multiplier,col="black",lwd=2)
    points(mean.green+sd.green*sd.multiplier,col="black",pch="-",lwd=2)
    legend("topright",rownames(sp.green),pch=22,pt.bg = col.green,inset=c(-0.9,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))
    
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers))
}
