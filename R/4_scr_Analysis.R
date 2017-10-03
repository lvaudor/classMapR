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


# A coinertia analysis is realized on the results of the PCA and correspondence analysis
rCoin=reactive({
  mypca=PCA(rDataQuanti())
  mymca=MCA(rDataCat())
  mycoin<-coinertia(mypca,mymca,
                    scan=FALSE,
                    nf=min(ncol(mypca$li),ncol(mymca$li)))
  return(mycoin)
})


output$imPCA=renderPlot({scatter(PCA(rDataQuanti()))})
output$imMCA=renderPlot({
  scatter(MCA(rDataCat()))
  })
output$imCoin=renderPlot({scatter(rCoin())})