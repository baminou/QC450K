staining <- function(shinySummarizedData,sd.multiplier=6,plot=FALSE,output_file=NULL){

  st.green <- data.frame(shinySummarizedData@greenControls$STAINING)
  st.red <- data.frame(shinySummarizedData@redControls$STAINING)
  
  rownames(st.green) <- c("DNP (Med)","DNP (Bgnd)","Biotin (Med)","Biotin (Bgnd)")
  rownames(st.red) <- c("DNP (Med)","DNP (Bgnd)","Biotin (Med)","Biotin (Bgnd)")
  
  sd.green <- sapply(X = st.green[c(1,2,4),],FUN = function(x)sd(x,na.rm=T))
  sd.red <- sapply(X = st.red[c(3,2,4),],FUN = function(x)sd(x,na.rm=T))
  
  mean.green <- sapply(X = st.green[c(1,2,4),],FUN = function(x)mean(x,na.rm=T))
  mean.red <- sapply(X = st.red[c(3,2,4),],FUN = function(x)mean(x,na.rm=T))
  
  outliers.green <- st.green[3,]>mean.green+sd.multiplier*sd.green
  outliers.red <- st.red[1,]>mean.red+sd.multiplier*sd.red

  if(plot==T){
    if(!is.null(output_file)){
      png(filename = output_file)
    }

    ylimits <- max(c(t(st.green),t(st.red)))
    par(mfrow=c(1,2), mar=c(3,0,3,0)+0.1, oma=c(0,5,0,10)+0.1)
    matplot(t(st.green),type="p",pch=22,col="black",bg=c("red","purple","green","blue"),main="Green",ylab = "Intensity",xlab="sample",ylim = c(0,ylimits))
    lines(mean.green+sd.green*sd.multiplier,col="black",lwd=2)
    points(mean.green+sd.green*sd.multiplier,col="black",pch="-",lwd=2)
    matplot(t(st.red),type="p",pch=22,col="black",bg=c("red","purple","green","blue"),main="Red",ylab = "",xlab="sample",ylim = c(0,ylimits),yaxt='n')
    lines(mean.red+sd.red*sd.multiplier,col="black",lwd=2)
    points(mean.red+sd.red*sd.multiplier,col="black",pch="-",lwd=2)
    legend("topright",rownames(st.green),pch=22,pt.bg = c("red","purple","green","blue"),inset=c(-0.8,0),xpd=NA,title = "Legend")
    par(mfrow=c(1,1))
    
    if(!is.null(output_file)){
      dev.off()
    }
  }
  return(as.vector(outliers.green==TRUE&outliers.red==TRUE))
}