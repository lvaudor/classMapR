#' Launch the classMapR shiny app
#'
#' For more information see package's vignette.
#'
#' @export
#' @examples
#' classMapR()

graphiT=function(){
  shiny::runApp(findmypath("app",""))
}

#' Find path to classMapR package
#'
#' This function is used internally to find package's path.
#' @export

findmypath=function(dir,file){
  path=system.file(dir,file,package="classMapR")
  return(path)
}
