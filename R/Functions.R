

#' Title
#'
#' @param var
#' @param fac
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_var_cat=function(var,fac,...){
  matable=chisq.test(var,fac)$residuals
  prop=table(var)
  prop=prop/sum(prop)
  if(row.names(matable)[1]=="1") {
    s=as.numeric(row.names(matable))
    matable=matable[order(s),]
    prop=prop[order(s)]
  }
  transfo=function(x){return(log(as.vector(x)+0.1))}

  y=seq(from=0,to=1,length.out=nrow(matable)+1)
  gap=matable[,2]

  mybreaks=unique(c(quantile(gap[which(gap<0)],seq(from=0,to=1,by=0.2)),
                    quantile(gap[which(gap>0)],seq(from=0,to=1,by=0.2))))
  couleurtemp= colorRampPalette(c(mypink,"white", myblue),space="rgb")
  catcouleurs=cut(gap,mybreaks,include.lowest=T,mylabels=1:(length(mybreaks)-1))
  mescouleurs=couleurtemp(length(mybreaks))[catcouleurs]
  plot(transfo(c(0,1)),c(0,1),
       col="white", xaxt="n",yaxt="n",
       xlab="", ylab="", xaxs="i", yaxs="i",...)

  abline(v=transfo(seq(from=0,to=1,by=0.1)),col="grey")
  for (k in 2:length(prop)){
    y2=y[k]
    y1=y[k-1]
    x=prop[k-1]
    polygon(transfo(c(0,x,x,0,0)),c(y1,y1,y2,y2,y1),
            col=mescouleurs[k-1],border="black")
    text(transfo(0.5),mean(c(y1,y2)),rownames(matable)[k-1], lwd=2,cex=1.5)
  }
  text(transfo(0.5),mean(c(y[k],y[k+1])),rownames(matable)[k], lwd=2,cex=1.5)
}


