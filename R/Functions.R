#' Return ff data based on params table
#'
#' @param params table with layers' characeristics
#'
#' @return a ff data table
#' @export
#'
#' @examples
fAlldata=function(params){
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
  result=list(alldata=alldata,
              completerow=completerow)
  return(result)
}
