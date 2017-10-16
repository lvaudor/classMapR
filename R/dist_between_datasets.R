#### For each pixel in the data,
#### find which individual of the sub-sample (ID between 1 and n) is closest
dist_between_datasets=function(data1,data2,dataWeight,dataSd){
n=nrow(data2)
nvars=ncol(data2)
for (j in 1:n){
  print(paste("Cases closest to individual j=",j,"/",n,sep=""))
  list_dist=vector("list",length=nvars)
  names(list_dist)=paste("dist_",1:nvars,sep="")
  ## Consider each variable one by one:
  for (i in 1:nvars){
    ref=as.vector(data2[j,i])
    #dist_i corresponds to the distance of pixels to j-th individual regarding i-th variable:
    dist_i=ffrowapply((dataWeight[i]/dataSd[i])*abs(data1[i1:i2,i]-ref),
                      X=data1,
                      RETURN=TRUE,
                      FF_RETURN=TRUE,
                      RETCOL=NULL)
    assign(paste("dist_",i,sep=""),dist_i)
    list_dist[[i]]=dist_i
    # the variable-wise distances dist_i are listed into list_dist
  }
  #distdf: each column corresponds to the distance of pixels to j-th individual variable-wise
  #the list is transformed into a data.frame, and then a matrix:
  distdf=do.call(ffdf,list_dist)

  distdf=ffbase:::as.ff_matrix.ffdf(distdf)
  #distfin corresponds to the sum of distances variable-wise:
  distfin=ffrowapply(rowSums(distdf[i1:i2,]),
                     X=distdf,
                     RETURN=TRUE,
                     FF_RETURN=TRUE,
                     RETCOL=NULL)
  if(j==1){
    distind=ff(1L,length=c(length(distfin)),vmode="double")
    distdouble=ffdf(distfin,distind)
    distdouble=ffbase:::as.ff_matrix.ffdf(distdouble)
    dist=ffdf(distdouble,distfin)
    dist=ffbase:::as.ff_matrix.ffdf(dist)
  }
  if(j>1){
    # create a data.frame where rows correspond to pixels and:
    # 1st column corresponds to lowest distance to individuals (individuals yet considered: 1-(j-1))
    # 2nd column corresponds to index of individual with lowest distance
    # 3rd column corresponds to distance to j-th individual
    dist=ffdf(as.ff(dist[,1:2]), distfin)
    dist=ffbase:::as.ff_matrix.ffdf(dist)
  }
  # f2 takes dist (defined above) as input
  # and replaces the value in the first and second column
  # in case the distance to j-th individual is lower than
  # the lowest distance observed so far (on individuals 1-(j-1))
  f2=function(dist){
    res=dist
    ind=which(dist[,3]<=dist[,1])
    res[ind,1]=res[ind,3]
    res[ind,2]=rep(j,length(ind))
    return(res)
  }
  # this function is applied "by batches" on the whole dataset
  dist=ffrowapply(f2(dist[i1:i2,]),
                  X=dist,
                  #VERBOSE=TRUE,
                  RETURN=TRUE,
                  FF_RETURN=TRUE,
                  RETCOL=3)
}
return(dist)
}



