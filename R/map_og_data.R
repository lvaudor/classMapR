
for (j in 3:nvars){
var=myvars[j]
file=paste(file_results,"/figures/",var,".jpeg",sep="")

  jpeg(file,width=1200,height=900)
  par(mar=c(0,0,0,0),xaxs="i",yaxs="i")
  plot(c(1,myncol),c(0,-mynrow),col="white")
  if(dataType[j]=="cat"){
      palette(mypalette(length(list_lev[[j]])))
  }
  if(dataType[j]=="quanti"){
      palette(mypalette(10))
      mesquantiles=quantile(alldata[,j],seq(from=0,to=1,by=0.1),na.rm=TRUE)
  }
  
  myseq=round(seq(from=1,to=mynrow*myncol,by=myncol))
  for(i in 2:mynrow){
    nlines=myseq[i]-myseq[i-1]+1
    x=alldata[myseq[i-1]:(myseq[i]-1),1]
    y=alldata[myseq[i-1]:(myseq[i]-1),2]
    z=alldata[myseq[i-1]:(myseq[i]-1),j]
    if(dataType[j]=="quanti"){
      z=cut(z,mesquantiles,include.lowest=TRUE, labels=1:10)
    }
    points(x,-y,col=as.numeric(z))
    print(paste(i,"/",mynrow,sep=""))
  }
  points(mydata[,1],-mydata[,2])
  dev.off()

}

palette(mypalette(5))