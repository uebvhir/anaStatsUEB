#' @title Guardar mensajes de cambios aplicados en la base de datos
#'
#' @description
#' La función \code{desc_changes} se utiliza para guardar mensajes de cambios aplicados
#' en la base de datos.
#'
#' @param x Cadena de texto.
#'
#' @return Mensaje(s) de texto (invisible); además, añade cada mensaje al objeto
#'   \code{list_changes} en el entorno global, para poder listarlos o imprimirlos más
#'   tarde (p. ej. en un chunk de RMarkdown con \code{results = 'asis'}).
#'
#' @examples
#' desc_changes("")
#'
#' var_sino <- c("Fum", "alcohol")
#' desc_changes(paste0(
#'   "Los valores, 0 y 1 se han considerado 'No' y 'Si' respectivamente ",
#'   "para las variables: ", paste(var_sino, collapse = ", ")
#' ))
#' desc_changes("Los valores superiores a 10000 para la variable n_il6_f han sido considerados NA ")
#' list_changes
#'
#' #With results = 'asis' rmarkdown chunk
#' for(i in seq_along(list_changes)){
#' cat(list_changes[[i]])
#' }
#'
#' @keywords message report changes
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @include desc_changes.R
#' @export
desc_changes <- function(x, col = "blue"){
  x <- paste("\n * <span style='color:",col,"'>",x,"</span> \n")
  if(!exists("list_changes")){
    list_changes <<- list()
    i = 0
  }else{
    i <- length(list_changes)
  }
  list_changes[[i+1]] <<- x
  colors <- sapply(list_changes, extract_color)
  list_changes <- list_changes[order(match(colors, unique(colors)), na.last = TRUE)]
}


extract_color <- function(text) {
  matches <- regmatches(text, regexpr("color: #[0-9a-fA-F]+|color: [a-zA-Z]+", text))
  if (length(matches) > 0) {
    return(matches)
  } else {
    return(NA)
  }
}
