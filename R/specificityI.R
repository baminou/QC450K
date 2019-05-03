specificityI <- function(shinySummarizedData,sd.multiplier = 6,plot=FALSE,output_file=NULL){

  sp.green <- data.frame(shinySummarizedData@greenControls$`SPECIFICITY I`)
  sp.red <- data.frame(shinySummarizedData@redControls$`SPECIFICITY I`)
  
  rownames(sp.green) <- c("GT Mismatch 1 (PM)","GT Mismatch 2 (PM)","GT Mismatch 3 (PM)",
                    "GT Mismatch 1 (MM)","GT Mismatch 2 (MM)","GT Mismatch 3 (MM)",
                    "GT Mismatch 4 (PM)","GT Mismatch 5 (PM)","GT Mismatch 6 (PM)",
                    "GT Mismatch 4 (MM)","GT Mismatch 5 (MM)","GT Mismatch 6 (MM)")
  
  rownames(sp.red) <- c("GT Mismatch 1 (PM)","GT Mismatch 2 (PM)","GT Mismatch 3 (PM)",
                          "GT Mismatch 1 (MM)","GT Mismatch 2 (MM)","GT Mismatch 3 (MM)",
                          "GT Mismatch 4 (PM)","GT Mismatch 5 (PM)","GT Mismatch 6 (PM)",
                          "GT Mismatch 4 (MM)","GT Mismatch 5 (MM)","GT Mismatch 6 (MM)")
  
  sd.green <- sapply(X = 1:ncol(sp.green),FUN = function(x)sd(sp.green[4:12,x],na.rm=T))
  sd.red <- sapply(X = 1:ncol(sp.red),FUN = function(x)sd(sp.red[c(1:6,10:12),x],na.rm=T))
  
  mean.green <- sapply(X = 1:ncol(sp.green),FUN = function(x)mean(sp.green[4:12,x],na.rm=T))
  mean.red <- sapply(X = 1:ncol(sp.red),FUN = function(x)mean(sp.red[c(1:6,10:12),x],na.rm=T))
  
  outliers.green <- sp.green[1,]>mean.green+sd.multiplier*sd.green &
    sp.green[2,]>mean.green+sd.multiplier*sd.green &
    sp.green[3,]>mean.green+sd.multiplier*sd.green
  
  outliers.red <- sp.red[1,]>mean.red+sd.multiplier*sd.red &
    sp.red[2,]>mean.red+sd.multiplier*sd.red &
    sp.red[3,]>mean.red+sd.multiplier*sd.red

  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }
    
    ylimits <- max(c(t(sp.green),t(sp.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    col.green <- c(colorRampPalette(c("darkgreen","lightgreen"))(3),colorRampPalette(c("darkblue","lightblue"))(3),
                   colorRampPalette(c("purple","red"))(3),colorRampPalette(c("orange","yellow"))(3))
    matplot(t(sp.green),type="p",pch=22,col="black",bg=col.green,main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    lines(mean.green+sd.green*sd.multiplier,col="black",lwd=2)
    points(mean.green+sd.green*sd.multiplier,col="black",pch="-",lwd=2)
    matplot(t(sp.red),type="p",pch=22,col="black",bg=col.green,main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    lines(mean.red+sd.red*sd.multiplier,col="black",lwd=2)
    points(mean.red+sd.red*sd.multiplier,col="black",pch="-",lwd=2)
    legend("topright",rownames(sp.green),pch=22,pt.bg = col.green,inset=c(-1.2,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))
    
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers.green==TRUE|outliers.red==TRUE))
}