#' message_ueb Function
#'
#' The function desc_changes is used to save messages of changes applied in database
#' @param x   a string
#' @param col color
#' @param ls si se trata del elemento de una lista
#' @export message_ueb
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' message_ueb("se ha realizado un cambio en el nombre de la variable", col = "purple")
#'
#'

#' @keywords message report changes

message_ueb <- function(x, col = "blue", ls = FALSE){
  x <- paste0("\n" ,ifelse(ls," * ", ""), "<span style='color:",col,"'>",x,"</span> \n ")
  return(cat(x))
}
