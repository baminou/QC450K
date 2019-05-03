bisulfiteConversionII <- function(shinySummarizedData,plot=FALSE,p.treshold=0.01,output_file=NULL){
  
  bc.green <- data.frame(shinySummarizedData@greenControls$`BISULFITE CONVERSION II`)
  bc.red <- data.frame(shinySummarizedData@redControls$`BISULFITE CONVERSION II`)
  
  rownames(bc.green) <- c("BS Conversion II-1","BS Conversion II-2","BS Conversion II-3","BS Conversion II-4")
  rownames(bc.red) <- c("BS Conversion II-1","BS Conversion II-2","BS Conversion II-3","BS Conversion II-4")
  
  outliers <- sapply(1:ncol(bc.green),FUN = function(x){t.test(bc.green[,x],bc.red[,x])$p.value < p.treshold})
  outliers <- outliers & colMeans(bc.green) < colMeans(bc.red)
  
  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    
    ylimits <- max(c(t(bc.green),t(bc.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    col.green <- colorRampPalette(c("purple","orange"))(4)
    matplot(t(bc.green),type="p",pch=22,col="black",bg=col.green,main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    matplot(t(bc.red),type="p",pch=22,col="black",bg=col.green,main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    legend("topright",rownames(bc.green),pch=22,pt.bg = col.green,inset=c(-1.1,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))
    
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers))
}