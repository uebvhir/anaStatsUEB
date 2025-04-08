#' var_to_num Function
#'
#' The function var_to_num is used to convert a character variable to numeric variable. Change ',' by '.'. Delete spaces. Return a numeric variable and a warning with the delete values.
#' @param x   a character variable with numbers
#' @param pat a string or strings with pattern to delete
#' @param rep a string or strings witg pattern to replace
#' @export var_to_num
#' @import magrittr purrr
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#'
#'
#' @return a numeric variable and a warning with delete values.
#'  edad <- c("6", "6,7", "10 ", "12.5", "3%   ", "4,", "no se", ">2" ,"4pct")
#'  Hmisc::label(edad) <- "Edad de los pacientes"
#'  df <- data.frame(edad = edad, e1 = edad)
#'  df %>% mutate(edad_mod = structure(var_to_num(edad), label = Hmisc::label(edad)))
#'  df %>% mutate(edad_mod2 = structure(var_to_num(edad, pat = c(">", "pct"),rep = c("","")), label = Hmisc::label(edad)))
#' @keywords numeric character class change warnings




var_to_num <- function( x ,pat = NULL, rep = NULL){
  txt_col <- deparse(substitute(x))
  orig <- x  #variable original
  x_new <-  as.numeric(trimws(reduce2(fixed(c(",", "%", pat)), fixed(c(".", "",rep)), .init = x, str_replace)))
  na_value <-   na.omit(unique(orig[is.na(x_new)]))
  Hmisc::label(x_new) <- Hmisc::label(orig)
  if(length(na_value) >0){
    mss <- paste0("Los valores '", paste0(na_value, collapse = "', '"), "' de la variable '",txt_col,
                  "' han sido considerados datos faltantes")
    desc_changes(mss, col = "#33cc33" )
    warning(mss)
  }
  return(x_new)
}




# var_to_num2 <- function( dat,col,pat = NULL, rep = NULL){
#   txt_col <- deparse(substitute(col))
#   orig <- dat %>% dplyr::select({{col}}) #variable original
#   lbl <- ifelse(Hmisc::label(orig) == "", txt_col,Hmisc::label(orig)) #etiqueta
#    dd <- dat |>
#      mutate({{ col }} :=  structure(
#        as.numeric(
#        trimws(
#          reduce2(fixed(c(",", "%", pat)), fixed(c(".", "",rep)), .init = {{col}}, str_replace))),
#        label = lbl       )) %>%
#      dplyr::select({{col}})
#    na_value <-  orig[dd %>% select({{col}}) %>% is.na() ]
#    if(length(na_value) >0){
#      mss <- paste0("Los valores '", paste0(na_value, collapse = "', '"), "' de la variable '",txt_col,"' han sido considerados datos faltantes")
#      desc_changes(mss)
#      warning(mss)
#    }
#    return(dd)
# }










# var_to_num <- function(x, name.var = NULL){
#   x_orig <- x
#   x <- as.character(x)
#   x <- gsub("%","",gsub(",",".", x))
#   x <- trimws(x)
#   x <- as.numeric(x)
#   na_value <- na.omit(unique(x_orig[is.na(x)]))
#   try(name_var <- unlist(strsplit(x,split = "$",fixed = T))[2], T)
#   if(length(na_value) >0){
#     name.var <- ifelse(!is.null(name.var), name.var,"")
#     mss <- paste("Los valores '", paste0(na_value, collapse = ", "), "' de la variable",name.var," han sido considerados datos faltantes")
#     desc_changes(mss)
#     warning(mss)
#   }
#   return(x)
# }
