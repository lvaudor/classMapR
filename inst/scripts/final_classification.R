rTree=reactive({
      mydata=rData()
      params=rParams()
      ## Function calc_dist has as inputs:
      # a data.frame, as well as the specification variable's type (quantitative or categorical),
      # and the specification of variable's type, standard deviation, and weight
      calc_dist=function(mydata,dataType,dataSd,dataWeight){
        Mdist=matrix(rep(0,nrow(mydata)^2),nrow=nrow(mydata))
        for (i in 1:ncol(mydata)){
          x=mydata[,i]
          type=dataType[i]
          Mdist_i=as.matrix(dist(x))
          if(type=="cat"){Mdist_i=abs(sign(Mdist_i))}
          # distance is normalized by the variable's standard deviation
          # and multiplied by its weight
          Mdist_i=dataWeight[i]*(Mdist_i)/dataSd[i]
          Mdist=Mdist+Mdist_i
          assign(paste("Mdist_",i,sep=""),Mdist_i,env=globalenv())
        }
        return(Mdist)
      }
      print(mydata)
      print(params)
      # the distance is calculated on the subsample:
      mydist=calc_dist(na.omit(mydata),params$dataType,params$dataSd,params$dataWeight)
      mydist=as.dist(mydist)
      # the classification of subsample based on this distance matrix
      # is implemented, with the Ward method
      mytree=hclust(mydist, method="ward.D")
      return(mytree)
})
# The graphic for this classification tree is produced:

output$tree=renderPlot({
  mytree=rTree()
  plot_tree(mytree, ylim=c(0,max(mytree$height)),labels_ab=TRUE, nclust=input$nclust_exp)
})

rDataClust=reactive({
  mydata=rData()
  clusterlabel=input$clusterlabel
  mytree=rTree()
  mylabels=relabel_clusters(mytree)
  clusterlabel=which(mylabels==clusterlabel)
  ind=get_cluster_elem(mytree,clusterlabel)
  fac=rep("out",nrow(mydata))
  fac[ind]="in"
  mydata=data.frame(mydata,fac)
  return(mydata)
})

rDataQuantiClust=reactive({
  mydata=rDataClust()
  params=rParams()
  dataType=c(params$dataType,"fac")
  mydata=mydata[,which(dataType %in% c("quanti","fac"))]
  return(mydata)
})

rDataCatClust=reactive({
  mydata=rDataClust()
  params=rParams()
  dataType=c(params$dataType,"fac")
  mydata=mydata[,which(dataType %in% c("cat","fac"))]
  return(mydata)
})


output$clustDescCat=renderPlot({
  mydatacat=rDataCatClust()
  if(is.vector(mydatacat)){
      data=gather(mydatacat,key="key",value="value",-fac)
      p=ggplot(data,aes(x=value, fill=fac))+geom_bar(position="fill")+facet_wrap(~key, scales="free")
      plot(p)
  }
})

output$clustDesc=renderPlot({
  mydata=rDataClust()
  if(!is.vector(mydata)){
      data=gather(mydata,key="key",value="value",-fac)
      p=ggplot(data,aes(x=value,fill=fac))+geom_histogram(position="fill")+facet_wrap(~key,scales="free")
      plot(p)
  }
})

output$inertia=renderPlot({
  mydata=rData()
  tree=rTree()
  dataWeight=rParams()$dataWeight
  dataSd=rParams()$dataSd
  n=input$nInd
  nclust=input$nclust_exp
  ## calculate total inertia
  Itot=0
  for (j in 1:ncol(mydata)){
    Ij=sum(abs(mydata[,j]-mean(mydata[,j]))*dataWeight[j]/dataSd[j])
    Itot=Itot+Ij
  }
  ## for varying values of k (the number of cluster) from 2 to 30
  ## what is the value of ratio explained inertia/total inertia
  nclustersmax=min(30,n)
  ratio_I=c(0,rep(NA, nclustersmax-1))
  for(k in 2:nclustersmax){
    clusters_tmp=cutree(tree,k)
    Iwit=0
    ## for each cluster l, calculate inertia within cluster
    for(l in 1:k){
      ind=which(clusters_tmp==l)
      data_tmp=mydata[ind,]
      if(length(ind)==1){
        data_tmp=as.data.frame(t(mydata[ind,]))
      }
      Iwitl=0
      for(j in 1:ncol(data_tmp)){
        Iwitlj=sum(abs(data_tmp[,j]-mean(data_tmp[,j]))*dataWeight[j]/dataSd[j])
        Iwitl=Iwitl+Iwitlj
      }
      ## sum inertia of all clusters l in 1:k
      Iwit=Iwit+Iwitl
    }
    ratio_I[k]=(Itot-Iwit)/Itot*100
  }
  # Produce the associated graphic
  plot(c(1,nclustersmax),c(0,100),
       xlab="number of clusters",
       ylab="Explained Inertia (%)",
       col="white")
  abline(h=seq(from=0,to=100,by=10), col=mygrey)
  points(ratio_I,lwd=4, type="h")
  points(nclust,ratio_I[nclust], col="red",type="h",lwd=4)
  text(nclust,ratio_I[nclust]+10,str_c(round(ratio_I[nclust],2),"%"),col="red")
  #return(ratio_I)
})
