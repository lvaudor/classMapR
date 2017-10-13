readDataFrame=function(resultsFile,type="all"){
    if(type=="all"){   mydata="mydata"}
    if(type=="quanti"){mydata="mydataquanti"}
    if(type=="cat"){   mydata="mydatacat"}
    filePath=str_c(datadir(),mydata,".csv")
    if(file.exists(filePath)){
      myDF=read.csv(filePath, sep=";")
    }else{myDF=NULL}
    if(type=="cat"){
      for(j in 1:ncol(myDF)){myDF[[j]]=as.factor(str_c("L",myDF[[j]]))}
    }
    return(myDF)
  }

  rData=      reactive({readDataFrame(input$resultsFile,type="all")})
  rDataQuanti=reactive({readDataFrame(input$resultsFile,type="quanti")})
  rDataCat=   reactive({readDataFrame(input$resultsFile,type="cat")})

  output$data=renderTable({rData()})

  output$imdesc=renderPlot({
    data=gather(rData())
    p=ggplot(data,aes(x=value))+geom_histogram()+facet_wrap(~key, scales="free")
    plot(p)
  },width=1200,height=1200)


