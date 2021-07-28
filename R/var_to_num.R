#' var_to_num Function
#'
#' The function var_to_num is used to convert a character variable to numeric variable. Change ',' by '.'. Delete spaces. Return a numeric variable and a warning with the delete values.
#' @param x   a character variable with numbers
#' @param name.var a string with variable name
#' @export var_to_num
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#'
#' @return a numeric variable and a warning with delete values.
#'  edad <- c("6", "6,7", "10 ", "12.5", "3   ", "4,", "no se" )
#'  Hmisc::label(edad) <- "Edad de los pacientes"
#'  edad_num <- var_to_num(edad)
#'  edad_num
#'  Hmisc::label(edad_num)

#' @keywords numeric character class change warnings


var_to_num <- function(x, name.var = NULL){
  x_orig <- x
  x <- as.character(x)
  x <- gsub("%","",gsub(",",".", x))
  x <- trimws(x)
  x <- as.numeric(x)
  na_value <- na.omit(unique(x_orig[is.na(x)]))
  try(name_var <- unlist(strsplit(x,split = "$",fixed = T))[2], T)
  if(length(na_value) >0){
    name.var <- ifelse(!is.null(name.var), name.var,"")
    mss <- paste("Los valores", paste0(na_value, collapse = ", "), "de la variable",name.var," han sido considerados datos faltantes")
    desc_changes(mss)
    warning(mss)
  }
  return(x)
}
