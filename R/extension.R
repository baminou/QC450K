extension <- function(shinySummarizedData,sd.multiplier = 6,plot=FALSE,output_file=NULL){

  ext.green <- data.frame(shinySummarizedData@greenControls$EXTENSION)
  ext.red <- data.frame(shinySummarizedData@redControls$EXTENSION)
  
  rownames(ext.green) <- c("A","T","C","G")
  rownames(ext.red) <- c("A","T","C","G")
  
  sd.green <- sapply(X = ext.green[c("A","T"),],FUN = function(x)sd(x,na.rm=T))
  sd.red <- sapply(X = ext.red[c("G","C"),],FUN = function(x)sd(x,na.rm=T))
  
  mean.green <- sapply(X = ext.green[c("A","T"),],FUN = function(x)mean(x,na.rm=T))
  mean.red <- sapply(X = ext.red[c("G","C"),],FUN = function(x)mean(x,na.rm=T))
  
  outliers.green <- ext.green["G",]>mean.green+sd.multiplier*sd.green & ext.green["C",]>mean.green+sd.multiplier*sd.green
  outliers.red <- ext.red["A",]>mean.red+sd.multiplier*sd.red & ext.red["T",]>mean.red+sd.multiplier*sd.red
  
  p.green <- sapply(X=ext.green,FUN=function(x)t.test(x[c(1,2)],x[c(3,4)])$p.value)
  p.red <- sapply(X=ext.red,FUN=function(x)t.test(x[c(1,2)],x[c(3,4)])$p.value)
  
  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    
    ylimits <- max(c(t(ext.green),t(ext.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    matplot(t(ext.green),type="p",pch=22,col="black",bg=c("red","purple","green","blue"),main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    lines(mean.green+sd.green*sd.multiplier,col="black",lwd=2)
    points(mean.green+sd.green*sd.multiplier,col="black",pch="-",lwd=2)
    matplot(t(ext.red),type="p",pch=22,col="black",bg=c("red","purple","green","blue"),main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    lines(mean.red+sd.red*sd.multiplier,col="black",lwd=2)
    points(mean.red+sd.red*sd.multiplier,col="black",pch="-",lwd=2)
    legend("topright",rownames(ext.green),pch=22,pt.bg = c("red","purple","green","blue"),inset=c(-0.5,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))

    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers.green==TRUE&outliers.red==TRUE))
}