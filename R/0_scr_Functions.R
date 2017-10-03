
#' Title
#'
#' @param df
#' @param row.w
#' @param scannf
#' @param nf
#'
#' @return
#' @export
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
  col.w[which(col.w == 0)]=1e-10

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

# The function get_lower_clusters takes a classification tree as input
# and cluster index and its output can be:
# 1) all individuals in this cluster if immediate=FALSE
# 2) the index of the clusters comprised in it (at the level immediately below)
#' Title
#'
#' @param mytree
#' @param i
#' @param immediate
#'
#' @return
#' @export
#'
#' @examples
get_lower_clusters=function(mytree,i,immediate=FALSE){
  result=i
  fclust=function(i){
    numclust=i
    clusters=mytree$merge[numclust,]
    indneg=which(clusters<0)
    indpos=which(clusters>0)
    if(length(indpos)==0){j=0}
    if(length(indpos)!=0){j=clusters[indpos]}
    return(j)
  }
  if(immediate==FALSE){
    elem=c()
    while(sum(result)>0){
      result=fclust(result)
      elem=c(elem,result)
    }
    elem=elem[1:(length(elem)-1)]
  }
  if(immediate==TRUE){
    elem=fclust(i)
  }
  return(elem)

}


#' Title
#'
#' @param mytree
#' @param branches
#'
#' @return
#' @export
#'
#' @examples
get_cluster_locations=function(mytree,branches=FALSE){
  xclust=rep(NA,nrow(mytree$merge))
  x1=xclust
  x2=xclust
  yclust=xclust
  y1=xclust
  y2=xclust
  for (i in 1:nrow(mytree$merge)){
    elemtot=mytree$merge[i,]
    for (j in 1:length(elemtot)){
      if(elemtot[j]<0){
        elem=-elemtot[j]
        o=which(mytree$order==elem)
        assign(paste("o",j,sep=""),o)
        h=0
        assign(paste("h",j,sep=""),h)
      }
      if(elemtot[j]>0){
        o=xclust[elemtot[j]]
        assign(paste("o",j,sep=""),o)
        h=mytree$height[elemtot[j]]
        assign(paste("h",j,sep=""),h)
      }
    }
    xclust[i]=mean(c(o1,o2))
    yclust[i]=mytree$height[i]
    x1[i]=o1
    x2[i]=o2
    y1[i]=h1
    y2[i]=h2
  }
  locations=data.frame(xclust,yclust)
  if(branches==TRUE){locations=data.frame(xclust,yclust,x1,y1,x2,y2)}
  return(locations)
}

#' Title
#'
#' @param mytree
#' @param i
#'
#' @return
#' @export
#'
#' @examples
get_cluster_elem=get_elem=function(mytree,i){
  elem=mytree$merge
  elem1=elem[i,]
  elem1=-elem1[elem1<0]
  elem2=elem[get_lower_clusters(mytree,i),]
  elem2=-elem2[elem2<0]
  elem=c(elem1,elem2)
  return(elem)
}


#' Title
#'
#' @param mytree
#' @param ylim
#' @param labels_ab
#' @param nclust
#'
#' @return
#' @export
#'
#' @examples
plot_tree=function(mytree, ylim=c(0,1.2*max(mytree$height)),labels_ab=FALSE,nclust){
  l=get_cluster_locations(mytree, branches=TRUE)
  mytree$merge
  par(las=2)
  plot(c(1,nrow(mytree$merge)+1),ylim,
       col="white", xaxt="n", xlab="",  ylab="height")
  #axis(side=1, at=1:length(mytree$order), lab=mytree$order)
  g=function(x1,x2,y1,y2, yclust){
    points(c(x1,x1,x2,x2),c(y1,yclust,yclust,y2), type="l")
  }
  for (i in 1:nrow(mytree$merge)){
    g(l$x1[i],l$x2[i],l$y1[i],l$y2[i], l$yclust[i])

  }
  if(labels_ab){
    mylabels=relabel_clusters(mytree)
    cluster_locations=get_cluster_locations(mytree)
    text(cluster_locations[,1],cluster_locations[,2], mylabels)
  }
  h=mean(mytree$height[length(mytree$height)-nclust+c(1:2)])
  abline(h=h,col="red",lty=2)
}
# New labels for the clusters are produced, so as to reflect the successive
# parting of branches ("a" is divided into "aa", and "ab",
# "aa" is divided into "aaa" and "aab", etc.)
#' Title
#'
#' @param mytree
#'
#' @return
#' @export
#'
#' @examples
relabel_clusters=function(mytree){
  mylabels=rep(NA,nrow(mytree$merge))
  n=nrow(mytree$merge)
  mylabels[n]=""
  for (i in n:1){
    clust=get_lower_clusters(mytree,i, immediate=T)
    lclust=length(clust)
    if(lclust>0){
      for(j in 1:lclust){
        mylabels[clust[j]]=paste(mylabels[i],c("a","b")[j], sep="")
      }
    }
  }
  return(mylabels)
}

# The function explained_inertia has as inputs
# the descriptor's data.frame, and classification tree
# and as output the part of inertia as a function of the
# number of clusters.

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

#' Title
#'
#' @param mytree
#' @param i
#'
#' @return
#' @export
#'
#' @examples
get_clusters_containing_individual=function(mytree,i){
  cl=which(mytree$merge==-i,arr.ind=T)[1]
  clusts=cl
  while(!is.na(cl)){
    cl=which(mytree$merge==cl,arr.ind=T)[1]
    clusts=c(clusts,cl)
  }
  return(clusts[1:(length(clusts)-1)])
}

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


