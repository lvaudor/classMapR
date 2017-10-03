

#' Get clusters ID or individuals' indices, of clusters or individuals comprised in a cluster
#'
#' @param mytree a classification tree
#' @param i the index of the cluster
#' @param immediate if TRUE, the function returns clusters at the level immediately below, if FALSE, the function returns all individuals comprised in the cluster
#'
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
#' @param mytree a classification tree
#' @param leaves if TRUE, also returns the location of the 2 leaves
#'
#' @return
#' @export
#'
#' @examples
get_cluster_locations=function(mytree,leaves=FALSE){
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
  if(leaves==TRUE){locations=data.frame(xclust,yclust,x1,y1,x2,y2)}
  return(locations)
}

#' Get indices of individuals that are part of a cluster
#'
#' @param mytree the hierarchical classification tree
#' @param i the ID of the cluster
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
  l=get_cluster_locations(mytree, leaves=TRUE)
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
