#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Define server logic required to draw a histogram
shinyServer(function(input,output,session) {
  source("scripts/data_description.R",local=TRUE)
  source("scripts/final_classification.R",local=TRUE)
  shinyDirChoose(input, 'dir', roots = c(home = path.expand('~')), filetypes = c('', 'txt'))

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

  datadir=reactive({
    dirname=str_c(path.expand("~"),
                   "/",
                   input$resultsFile,
                   "/data/")
    return(dirname)}
  )

  makeUI=function(myvars,index){
      listItems=vector(length=length(myvars)*2,mode="list")
      for (i in 1:length(myvars)){
        if(!(myvars[i] %in% c("x","y"))){
          typeWidget=radioButtons(paste0("varType",index[i]),
                                  "descriptor type",
                                  c("cat","quanti"),
                                  selected="quanti",
                                  inline=TRUE)
          defaultWeight=1
        }else{
          typeWidget=p("geographical positioning")
          defaultWeight=0
        }
        listItems[[i]]=fluidRow(column(width=3,
                                       h4(myvars[i])),
                                column(width=4,
                                       typeWidget),
                                column(width=4,
                                       sliderInput(paste0("varWeight",index[i]),
                                                   "weight",
                                                   value=defaultWeight,
                                                   min=0,
                                                   max=20),
                                column(width=1))
        )#fluidRow
      }
  return(listItems)
  }

  output$filesUI1=renderUI({
    input$dir
    myvars=c("x","y",rVars())
    nvars=floor(length(myvars)/2)
    result=makeUI(myvars[1:nvars],1:nvars)
    return(result)
  })

  output$filesUI2=renderUI({
    input$dir
    myvars=c("x","y",rVars())
    nvars=floor(length(myvars)/2)
    result=makeUI(myvars[(nvars+1):length(myvars)],(nvars+1):length(myvars))
    return(result)
  })

  fParams=function(dir){
    paramsfile=str_c(dir,"params.csv")
    if(!file.exists(paramsfile)){
      params=data.frame(myvars=c("x","y"),
                        dataType=c("pos","pos"),
                        dataWeight=c("1","1"),
                        nrow=c(NA,NA),
                        ncol=c(NA,NA),
                        rootpath=rep(path.expand("~"),2),
                        resultpath=c(NA,NA),
                        dataSd=c(NA,NA)
      )
    }
    if(file.exists(paramsfile)){
      params=read.csv(paramsfile,sep=";", stringsAsFactors=FALSE)
    }
    return(params)
  }
  rParams=reactive({
    input$doStep1
    paramspath=str_c(datadir(),"params.csv")
    result=NULL
    if(file.exists(paramspath)){
    result=read.csv(paramspath,
                    sep=";",
                    stringsAsFactors=FALSE
                    )
    }
    return(result)
  })


  rFilesRead=reactive({
    mydir=paste0(c(path.expand('~'),input$dir$path[2:length(input$dir$path)]), collapse="/")
    myfiles=list.files(mydir)
    myfiles=myfiles[str_detect(myfiles,"\\.asc$")]
    myvars=str_match(myfiles,"(.*)(\\.asc$)")[,2]
    myfiles=data.frame(path=str_c(mydir,"/",myfiles),
                       var=myvars,
                       stringsAsFactors=FALSE)
  })

  rFiles=reactive({rFilesRead()[,1]})
  rVars=reactive({rFilesRead()[,2]})


  observeEvent(input$doStep1,{
    resultsFile=dirname(datadir())
    myfiles=rFiles()
    myvars=rVars()
    myvars=c("x","y",myvars)
    nvars=length(myvars)
    dataType=dataType=unlist(reactiveValuesToList(input)[str_c("varType",1:nvars)])
    dataType=c("pos","pos",dataType)
    dataWeight=unlist(reactiveValuesToList(input)[str_c("varWeight",1:nvars)])
    myraster=raster(myfiles[1])
    mynrow=nrow(myraster)
    myncol=ncol(myraster)
    file1=dirname(datadir())
    file2=datadir()
    if(!dir.exists(file1)){dir.create(file1)}
    if(!dir.exists(file2)){dir.create(file2)}
    ### Create params.csv
    rootPath=paste0(input$dir$path[2:length(input$dir$path)], collapse="/")
    params=data.frame(myvars,
                      dataType,
                      dataWeight,
                      nrow=rep(mynrow,length(myvars)),
                      ncol=rep(myncol,length(myvars)),
                      rootPath=rep(rootPath,length(myvars)),
                      path=c("","",myfiles),
                      resultPath=rep(datadir(),length(myvars)),
                      dataSd=rep(NA,length(myvars)),
                      stringsAsFactors=FALSE)
    write.table(params,
                str_c(datadir(),"params.csv"),
                row.names=FALSE,sep=";")
    ################################
    cat_levels_file=str_c(datadir(),"cat_levels.asc")
    if(file.exists(cat_levels_file)){file.remove(cat_levels_file)}
    file.create(str_c(datadir(),"cat_levels.asc"))
    tmpdir=dirname(str_c(datadir(),"tmp"))
    options(fftempdir=tmpdir)
    if(!file.exists(tmpdir)){dir.create(tmpdir)}
    for (j in 3:nvars){
      if(dataType[j]=="quanti"){mymode="double"}
      if(dataType[j]=="cat"){mymode="integer"}
      var=ff(0L,vmode=mymode,length=mynrow*myncol)
      var[]=raster(myfiles[j-2])[]
      if(dataType[j]=="cat"){
        mylevels=sort(unique(var[]))
      }else{mylevels=""}
      cat(myvars[j]," ", dataType[j], mylevels, "\n",
          file=str_c(resultsFile,"/data/cat_levels.asc"),
          sep=" ",append=TRUE)
    }
    ##########################################################################
    # For all variables in the data calculate standard deviation
    datalist=fAlldata(params)
    alldata=datalist$alldata
    completerow=datalist$completerow
    ### Create data tables
    ind_completerow<<-which(completerow[]==1)
    data1<-alldata[ind_completerow[],]
    ind<-round(seq(from=1,
                   to=nrow(data1),
                   length.out=min(input$nInd,nrow(data1))))
    mydata=data1[ind,]
    colnames(mydata)<- myvars
    mydatacat       <- mydata[,which(dataType=="cat")]
    mydataquanti    <- mydata[,which(dataType!="cat")]

    for (i in 1:length(myvars)){
      x=mydata[,i]
      type=dataType[i]
      Mdist=as.matrix(dist(x))
      # if the variable is categorical standard deviation is calculated in a special way
      # the distance matrix is composed of only zeros and ones
      if(type=="cat"){Mdist=abs(sign(Mdist))}
      params$dataSd[i]=sd(Mdist)
    }
    write.table(params,
                str_c(datadir(),"params.csv"),
                row.names=FALSE,sep=";")
    write.table(mydata,
                str_c(datadir(),"mydata.csv"),
                sep=";",
                row.names=F)
    write.table(mydatacat,
                str_c(datadir(),"mydatacat.csv"),
                sep=";",
                row.names=F)
    write.table(mydataquanti,
                str_c(datadir(),"mydataquanti.csv"),
                sep=";",
                row.names=F)
    print("Step1 done")
  })

  rAlldata=reactive({
    params=fParams(datadir())
    result=fAlldata(params)
    return(result)
  })

output$numMap=renderUI({
  params=fParams(datadir())
  radioButtons("numMap","variable",params$myvars[3:nrow(params)])
})

output$map=renderPlot({
    params=fParams(datadir())
    datapath=params$path[which(params$myvars==input$numMap)]
    myraster=raster(datapath)
    plot(myraster)
  })


observeEvent(input$doStep2,{
  params=fParams(datadir())
  data2=rData()
  alldata=fAlldata(params)
  data1=alldata$alldata[alldata$completerow[]==1,]
  mydist=dist_between_datasets(data1,data2, params$dataWeight,params$dataSd)
  write.csv.ffdf(as.ffdf(mydist),
                 str_c(datadir(),"dist.csv"),
                 append=FALSE)
  mytree=rTree()
  # Mclust is a matrix with n lines (individuals in the subsample data2)
  # and as many columns as there are nodes in the classification tree
  # M[i,j]=1 if the j-th node contains the i-th individual
  Mclust=matrix(rep(0,nrow(data2)*nrow(mytree$merge)),nrow=nrow(data2))
  for (i in 1:nrow(data2)){
    clusts=get_clusters_containing_individual(mytree,i)
    Mclust[i,clusts]=1
  }
  write.table(Mclust,
              str_c(datadir(),"Mclust.csv"),
              row.names=FALSE,
              sep=";")
  print("Step2 is done")
})

observeEvent(input$doStep3,{
  params=fParams(datadir())
  nclust=input$nclust
  # In this script we use the results of the extrapolation to n cluster
  # where n is the total number of individuals in the subsample
  # and produce a classification with nclust (nclust<n) clusters
  Mclust=as.matrix(read.table(str_c(datadir(),"Mclust.csv"),
                              sep=";",
                              header=TRUE))
  mytree=rTree()
  clusters=mytree$merge[nrow(mytree$merge),]
  while(length(clusters)<nclust){
    clustermax=max(clusters)
    newclusters=get_lower_clusters(mytree,clustermax,immediate=TRUE)
    if(length(newclusters)>0){
      clusters=c(clusters[1:(length(clusters)-1)],newclusters)
    }
    clusters=sort(clusters)
  }
  mynrow=params$nrow[3]
  myncol=params$ncol[3]
  dataType=params$dataType
  #allclust=ff(-9999,vmode="integer",dim=c(mynrow*myncol,1))
  dataclust=Mclust[,clusters]%*%matrix(nclust:1,nrow=nclust)
  dist=read.csv.ffdf(file=str_c(datadir(),"dist.csv"))
  dist=ffbase:::as.ff_matrix.ffdf(dist)
  f=function(dist){
    return(dataclust[dist[,2]])
  }
  resulttmp=ffrowapply(f(dist[i1:i2,]),
                       X=dist,
                       VERBOSE=TRUE,
                       RETURN=TRUE,
                       FF_RETURN=TRUE,
                       RETCOL=1)

  result=ff(NA,vmode="integer",length=mynrow*myncol)
  completerow=fAlldata(params)$completerow
  ### Create data tables
  ind_completerow<<-which(completerow[]==1)
  ####
  result[ind_completerow]=resulttmp[,1]
  ### Write .asc file containing classification results

  result_classif_file=str_c(datadir(),
                            "result_",
                            nclust,
                            "classes.asc")
  if(file.exists(result_classif_file)){file.remove(result_classif_file)}
  myraster=raster(fParams(datadir())$path[3], band=1)
  # myseq=seq(1,mynrow*myncol+1, by=myncol)
  # for(i in 2:length(myseq)){
  #   myraster[myseq[i-1]:(myseq[i]-1)]=result[myseq[i-1]:(myseq[i]-1)]
  #   print(str_c("Writing row ",i-1,"/",mynrow))
  # }
  myraster[]=result[]
  writeRaster(myraster,filename=result_classif_file,format="ascii",overwrite=TRUE)
  print("Step3 is done")
})

output$resultmap=renderPlot({
    input$doStep3
    datapath=str_c(datadir(),
                   "result_",
                   input$nclust,
                   "classes.asc")
    if(file.exists(datapath)){
        myraster=raster(datapath)
        plot(myraster)
    }
})



output$params=renderTable({rParams()})
output$rawdataDirectory=renderText({
  rawdataDirectory=paste0(c(path.expand('~'),input$dir$path[2:length(input$dir$path)]), collapse="/")
})
output$resultDirectory=renderText({datadir()})
output$sampleSize=renderText({str_c("The sample size is ", input$nInd)})
output$showParams=renderTable({rParams()[c("myvars","dataType","dataWeight")]})
output$listMaps=renderUI({
  input$nclust
  files=list.files(datadir())
  files=str_subset(files,"result_\\d+classes\\.asc")
  result=c()
  for (i in 1:length(files)){
    result=str_c(result,
                 "<li>",
                 files[i],
                 "</li>")
  }
  HTML(result)
})

})

