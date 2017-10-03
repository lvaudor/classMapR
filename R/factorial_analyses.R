# A PCA is realized on the set of quantitative variables
PCA=function(mydataquanti){
  nvars_quanti=ncol(mydataquanti)
  if(nvars_quanti>0){
    mypca=dudi.pca(mydataquanti,
                   scannf=F,
                   nf=ncol(mydataquanti)
                   #,col.w=dataWeight[which(dataType=="quanti")])
    )
  }
  return(mypca)
}


# A Correspondence analysis is realized on the set of categorical variables
MCA=function(mydatacat){
    nvars_cat=ncol(mydatacat)
    if(nvars_cat>0){
      mymca=dudi.acm(mydatacat,
                     scannf=F,
                     nf=+Inf)
    }
    return(mymca)
}


#' This is a slightly modified version of the dudi.acm function, that deals with columns
#' with null weight
#' @param df
#' @param row.w
#' @param scannf
#' @param nf
#'
#' @return an MCA result
#'
#'
#' @examples
dudi.acm=function (df, row.w = rep(1, nrow(df)), scannf = TRUE, nf = 2) {
  if (!all(unlist(lapply(df, is.factor))))
    stop("All variables must be factors")
  df <- as.data.frame(df)
  X <- acm.disjonctif(df)
  lig <- nrow(X)
  col <- ncol(X)
  var <- ncol(df)
  if (length(row.w) != lig)
    stop("Non convenient row weights")
  if (any(row.w < 0))
    stop("row weight < 0")
  row.w <- row.w/sum(row.w)
  col.w <- apply(X, 2, function(x) sum(x * row.w))
  col.w[which(col.w == 0)]=1e-50

  X <- t(t(X)/col.w) - 1
  col.w <- col.w/var
  X <- as.dudi(data.frame(X), col.w, row.w, scannf = scannf,
               nf = nf, call = match.call(), type = "acm")
  rcor <- matrix(0, ncol(df), X$nf)
  rcor <- row(rcor) + 0 + (0 + (0+1i)) * col(rcor)
  floc <- function(x) {
    i <- Re(x)
    j <- Im(x)
    x <- X$l1[, j] * X$lw
    qual <- df[, i]
    poicla <- unlist(tapply(X$lw, qual, sum))
    z <- unlist(tapply(x, qual, sum))/poicla
    return(sum(poicla * z * z))
  }
  rcor <- apply(rcor, c(1, 2), floc)
  rcor <- data.frame(rcor)
  row.names(rcor) <- names(df)
  names(rcor) <- names(X$l1)
  X$cr <- rcor
  return(X)
}

#### Graphical auxiliary functions ###########################################
#' Title
#'
#' @param coin
#'
#' @return
#' @export
#'
#' @examples
plot_coin=function(coin){
  layout(matrix(1:2,nrow=2))
  par(mar=c(1,1,1,1),oma=c(0,0,0,0))
  ##################################################################
  myxlim=c(min(c(0,2*min(coin$co[,1]))),
           max(c(0,2*max(coin$co[,1]))))
  myylim=c(min(c(0,2*min(coin$co[,2]))),
           max(c(0,2*max(coin$co[,2]))))
  plot(coin$co[,1],coin$co[,2],col="white",
       xaxt="n",yaxt="n",
       xlim=myxlim,ylim=myylim)
  abline(h=seq(from=-100,to=100,by=0.5),col="light grey")
  abline(v=seq(from=-100,to=100,by=0.5),col="light grey")
  abline(h=0);abline(v=0)
  text(coin$co[,1],coin$co[,2],rownames(coin$co))
  ##################################################################
  getlevelname=function(x){
    vparts=unlist(strsplit(x,"\\."))
    result=vparts[length(vparts)]
    return(result)
  }
  getvarname=function(x){
    vparts=unlist(strsplit(x,"\\."))
    result=vparts[1]
    return(result)
  }
  vvars=rep(NA,nrow(coin$li))
  vlevs=rep(NA,nrow(coin$li))
  for (i in 1:nrow(coin$li)){
    vvars[i]=getvarname(rownames(coin$li)[i])
    vlevs[i]=getlevelname(rownames(coin$li)[i])
  }
  uvars=unique(vvars)
  listcol=mypalette(length(uvars))
  mycol=rep(NA,length(vvars))
  for (i in 1:length(uvars)){
    ind=which(vvars==uvars[i])
    mycol[ind]=listcol[i]
  }
  myxlim=c(min(c(0,2*min(coin$li[,1]))),
           max(c(0,2*max(coin$li[,1]))))
  myylim=c(min(c(0,2*min(coin$li[,2]))),
           max(c(0,2*max(coin$li[,2]))))
  plot(coin$li[,1],coin$li[,2],
       xlim=myxlim,ylim=myylim,
       col="white", pch=20, xaxt="n", yaxt="n")
  abline(h=seq(from=-100,to=100,by=0.5),col="light grey")
  abline(v=seq(from=-100,to=100,by=0.5),col="light grey")
  abline(h=0);abline(v=0)
  points(coin$li[,1],coin$li[,2],
         pch=20, cex=5.5)
  points(coin$li[,1],coin$li[,2],
         pch=20, cex=5, col=mycol)

  text(coin$li[,1],coin$li[,2],vlevs,col="black")
  legend("bottomleft",uvars,col=unique(mycol),lwd=5)

}

###############################################################################

