#' as.character.ueb Function
#'
#' Create objects of type "character". Conserve labels
#' @param x   object to be coerced
#' @export as.character.ueb
#' @import Hmisc
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' #create variable
#' variable <- factor(c(0,0,0,1,1,1,0,"a",1,0, "B"))
#' Hmisc::label(variable) <- "label de la variable"
#' # automatic character
#' variable <- as.character.ueb(variable)
#' @return  attempts to coerce its argument to character type; like as.vector it strips attributes including names. For lists and pairlists (including language objects such as calls) it deparses the elements individually, except that it extracts the first element of length-one character vectors.
#'
#' @author
#' Miriam Mota-Foix <mmota.foix@gmail.com>
#' @keywords factor variable class levels labels

as.character.ueb <- function(x){
  lab_var <- Hmisc::label(x)
  var_chr <- as.character(x)
  Hmisc::label(var_chr) <- lab_var
  return(var_chr)
}

