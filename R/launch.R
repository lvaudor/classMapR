#' Launch the classMapR shiny app
#'
#' For more information see package's vignette.
#'
#' @export
#' @examples
#' classMapR()

classMapR=function(){
  path=system.file(package="classMapR")
  shiny::runApp(path)
}
