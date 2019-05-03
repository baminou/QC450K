bisulfiteConversionI <- function(shinySummarizedData,plot=FALSE,p.threshold=0.01,output_file=NULL){

  bc1.green <- data.frame(shinySummarizedData@greenControls$`BISULFITE CONVERSION I`)
  bc1.red <- data.frame(shinySummarizedData@redControls$`BISULFITE CONVERSION I`)
  
  rownames(bc1.green) <- c("C1","C2","C3","U1","U2","U3","C4","C5","C6","U4","U5","U6")
  rownames(bc1.red) <- c("C1","C2","C3","U1","U2","U3","C4","C5","C6","U4","U5","U6")
  
  outliers.green <- sapply(1:ncol(bc1.green),FUN = function(x){t.test(bc1.green[c("C1","C2","C3"),x],bc1.green[c("U1","U2","U3"),x])$p.value < p.threshold})
  outliers.green <- outliers.green & colMeans(bc1.green[c("C1","C2","C3"),])>colMeans(bc1.green[c("U1","U2","U3"),])
  outliers.red <- sapply(1:ncol(bc1.red),FUN = function(x){t.test(bc1.red[c("C4","C5","C6"),x],bc1.red[c("U4","U5","U6"),x])$p.value < p.threshold})
  outliers.red <- outliers.red & colMeans(bc1.red[c("C4","C5","C6"),])>colMeans(bc1.red[c("U4","U5","U6"),])
  
  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    
    ylimits <- max(c(t(bc1.green),t(bc1.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    col.green <- c(colorRampPalette(c("green","lightgreen"))(3),colorRampPalette(c("blue","lightblue"))(3),
                   colorRampPalette(c("purple","red"))(3),colorRampPalette(c("orange","yellow"))(3))
    matplot(t(bc1.green),type="p",pch=22,col="black",bg=col.green,main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    matplot(t(bc1.red),type="p",pch=22,col="black",bg=col.green,main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    legend("topright",rownames(bc1.green),pch=22,pt.bg = col.green,inset=c(-0.5,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))
    
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers.green==TRUE|outliers.red==TRUE))
}
