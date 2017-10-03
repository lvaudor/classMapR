#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(stringr)
library(raster)

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



  output$filesUI=renderUI({
    input$dir
    myvars=c("x","y",rVars())
    result=NULL
    if(length(myvars)>=3){
        listItems=vector(length=length(myvars)*2,mode="list")
        for (i in 1:length(myvars)){
          if(!(myvars[i] %in% c("x","y"))){
            typeWidget=radioButtons(paste0("varType",i),
                                    "descriptor type",
                                    c("cat","quanti"),
                                    selected="quanti",
                                    inline=TRUE)
            defaultWeight=1
          }else{
           typeWidget=p("geographical positioning")
           defaultWeight=0
          }
          listItems[[i]]=fluidRow(column(width=4,h4(myvars[i])),
                                  column(width=4,typeWidget),
                                  column(width=4,
                                         sliderInput(paste0("varWeight",i),
                                                      "weight",
                                                      value=defaultWeight,
                                                      min=0,
                                                      max=20))
                                   )#fluidRow
        }
        result=do.call("wellPanel",listItems)
    }
  })


  rParams=reactiveFileReader(1000,session,"../data/params.csv","read.csv",sep=";", stringsAsFactors=FALSE)

  observeEvent(input$dir,{
    mydir=paste0(c(path.expand('~'),input$dir$path[2:length(input$dir$path)]), collapse="/")
    myfiles=list.files(mydir)
    myfiles=myfiles[str_detect(myfiles,"\\.asc$")]
    myvars=str_match(myfiles,"(.*)(\\.asc$)")[,2]
    myfiles=data.frame(path=str_c(mydir,"/",myfiles),
                       var=myvars)
    write.table(myfiles,"files.csv",sep=";", row.names=FALSE,col.names=FALSE)
  })

  rFilesRead=reactiveFileReader(1000,session,"files.csv","read.csv", sep=";", stringsAsFactors=FALSE, header=FALSE)
  rFiles=reactive({rFilesRead()[,1]})
  rVars=reactive({rFilesRead()[,2]})


  observeEvent(input$readFiles,{
    resultsFile=str_c(path.expand('~'),"/",input$resultsFile)
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
    file1=resultsFile
    file2=str_c(resultsFile,"/data")
    if(!file.exists(file1)){dir.create(file1)}
    if(!file.exists(file2)){dir.create(file2)}
    ### Create params.csv
    rootPath=paste0(input$dir$path[2:length(input$dir$path)], collapse="/")
    params=data.frame(myvars,
                      dataType,
                      dataWeight,
                      nrow=rep(mynrow,length(myvars)),
                      ncol=rep(myncol,length(myvars)),
                      rootPath=rep(rootPath,length(myvars)),
                      path=c("","",myfiles),
                      resultPath=rep(input$resultsFile,length(myvars)),
                      dataSd=rep(NA,length(myvars)))
    write.table(params,
                "../data/params.csv",
                row.names=FALSE,sep=";")
    ################################
    cat_levels_file=str_c(resultsFile,"/data/cat_levels.asc")
    if(file.exists(cat_levels_file)){file.remove(cat_levels_file)}
    file.create(str_c(resultsFile,"/data/cat_levels.asc"))
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
    alldata=rAlldata()$alldata
    completerow=rAlldata()$completerow
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
                str_c(resultsFile,"/data/params.csv"),
                row.names=FALSE,sep=";")
    write.table(mydata,
                str_c(resultsFile,"/data/mydata.csv"),
                sep=";",
                row.names=F)
    write.table(mydatacat,
                str_c(resultsFile,"/data/mydatacat.csv"),
                sep=";",
                row.names=F)
    write.table(mydataquanti,
                str_c(resultsFile,"/data/mydataquanti.csv"),
                sep=";",
                row.names=F)
  })

  rAlldata=reactive({
    params=rParams()
    myvars=params$myvars
    nr=unique(params$nrow)
    nc=unique(params$ncol)
    dataType=params$dataType
    n=nr*nc
    alldata=ff(0L,vmode="double",dim=c(n,length(myvars)))
    mygrid=expand.ffgrid(list(x=ff(1:nc),y=ff(1:nr)))
    var1=ff(0L,vmode="double",length=n)
    var1[]=mygrid[,1]
    alldata[,1]=var1[]
    var2=ff(0L,vmode="double",length=n)
    var2[]=mygrid[,2]
    alldata[,2]=var2[]
    completerow<<-ff(1L,length=n)
    for (j in 3:length(myvars)){
      if(dataType[j]=="quanti"){mymode="double"}
      if(dataType[j]=="cat"){mymode="integer"}
      var=ff(0L,vmode=mymode,length=nr*nc)
      var[]=raster(params$path[j])[]
      alldata[,j]=var[]
      completerow[which(is.na(var[]))]<-0
    }
    result=list(alldata=alldata,completerow=completerow)
    return(result)
  })

output$numMap=renderUI({
  params=rParams()
  radioButtons("numMap","variable",params$myvars[3:nrow(params)])
})

output$map=renderPlot({
    params=rParams()
    datapath=params$path[which(params$myvars==input$numMap)]
    myraster=raster(datapath)
    plot(myraster)
  })


observeEvent(input$calculate_distances,{
  data2=rData()
  alldata=rAlldata()
  data1=alldata$alldata[alldata$completerow[]==1,]
  print(data1)
  params=rParams()
  mydist=dist_between_datasets(data1,data2, params$dataWeight,params$dataSd)
  write.csv.ffdf(as.ffdf(mydist),file=paste0(input$resultsFile,"//data//dist.csv"), append=FALSE)
  mytree=rTree()
  # Mclust is a matrix with n lines (individuals in the subsample data2)
  # and as many columns as there are merges in the classification tree
  Mclust=matrix(rep(0,nrow(data2)*nrow(mytree$merge)),nrow=nrow(data2))
  for (i in 1:nrow(data2)){
    clusts=get_clusters_containing_individual(mytree,i)
    Mclust[i,clusts]=1
  }
  write.table(Mclust,paste0(input$resultsFile,"//data//Mclust.csv"), row.names=FALSE, sep=";")
})

observeEvent(input$extrapolate_to_nclust,{
  nclust=input$nclust
  # In this script we use the results of the extrapolation to n cluster
  # where n is the total number of individuals in the subsample
  # and produce a classification with nclust (nclust<n) clusters
  Mclust=as.matrix(read.table(paste0(input$resultsFile,"//data/Mclust.csv"),sep=";", header=TRUE))
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
  mynrow=rParams()$nrow[3]
  myncol=rParams()$ncol[3]
  dataType=rParams()$dataType
  #allclust=ff(-9999,vmode="integer",dim=c(mynrow*myncol,1))
  dataclust=Mclust[,clusters]%*%matrix(nclust:1,nrow=nclust)
  dist=read.csv.ffdf(file=paste0(input$resultsFile,"//data//dist.csv"))
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
  completerow=rAlldata()$completerow
  ### Create data tables
  ind_completerow<<-which(completerow[]==1)
  ####
  result[ind_completerow]=resulttmp[,1]
  ### Write .asc file containing classification results

  result_classif_file=paste(input$resultsFile,"/data/result_",
                            nclust,"classes",
                            ".asc", sep="")
  if(file.exists(result_classif_file)){file.remove(result_classif_file)}
  myraster=raster(rParams()$path[3], band=1)
  # myseq=seq(1,mynrow*myncol+1, by=myncol)
  # for(i in 2:length(myseq)){
  #   myraster[myseq[i-1]:(myseq[i]-1)]=result[myseq[i-1]:(myseq[i]-1)]
  #   print(str_c("Writing row ",i-1,"/",mynrow))
  # }
  myraster[]=result[]
  writeRaster(myraster,filename=result_classif_file,format="ascii",overwrite=TRUE)
})

output$resultmap=renderPlot({
    datapath=paste(input$resultsFile,"/data/result_",input$nclust,"classes",".asc", sep="")
    if(file.exists(datapath)){
        myraster=raster(datapath)
        plot(myraster)
    }
})

output$params=renderTable({rParams()})
output$dataDirectory=renderText({truc=rParams()$rootPath[3]})
output$resultDirectory=renderText({rParams()$resultPath[3]})
output$n=renderText({input$nInd})
output$showParams=renderTable({rParams()[c("myvars","dataType","dataWeight")]})
})

