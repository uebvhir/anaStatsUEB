#' @title Conversión a character conservando las etiquetas (labels)
#'
#' @description
#' Convierte un objeto a tipo \code{character}, conservando sus etiquetas (\code{label}).
#'
#' @param x Objeto a convertir.
#'
#' @return Intenta convertir su argumento a tipo \code{character}; igual que
#'   \code{as.vector}, elimina los atributos, incluidos los nombres. Para listas y
#'   pairlists (incluidos objetos de lenguaje como las llamadas), deparsea los elementos
#'   individualmente, salvo que extrae el primer elemento de los vectores de caracteres
#'   de longitud uno.
#'
#' @import Hmisc
#'
#' @examples
#' #create variable
#' variable <- factor(c(0,0,0,1,1,1,0,"a",1,0, "B"))
#' Hmisc::label(variable) <- "label de la variable"
#' # automatic character
#' variable <- as.character.ueb(variable)
#'
#' @keywords factor variable class levels labels
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @include as.character.ueb.R
#' @export
as.character.ueb <- function(x){
  lab_var <- Hmisc::label(x)
  var_chr <- as.character(x)
  Hmisc::label(var_chr) <- lab_var
  return(var_chr)
}

