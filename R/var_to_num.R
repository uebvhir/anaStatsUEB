#' @title Conversión de una variable de texto a numérica
#'
#' @description
#' Convierte una variable de texto con números en una variable numérica: sustituye
#' la coma por el punto decimal, elimina el símbolo \code{\%} y los espacios al
#' principio y al final, y permite eliminar o sustituir otros patrones. Los valores
#' que no se pueden convertir pasan a \code{NA} y se avisa de cuáles son.
#'
#' @param x Variable de texto con números (también admite numéricos y factores).
#' @param pat Vector de texto con patrones literales (no expresiones regulares) a
#'   sustituir, además de la coma y el \code{\%}. Por defecto, \code{NULL}.
#' @param rep Vector de texto con los reemplazos de \code{pat}, en el mismo orden.
#'   Debe tener la misma longitud que \code{pat}. Por defecto, \code{NULL}.
#'
#' @return Un vector numérico con la etiqueta de \code{x} (\code{Hmisc::label}),
#' de clase \code{c("labelled", "numeric")}. Los valores no convertibles son
#' \code{NA}.
#'
#' @details
#' \itemize{
#'   \item El orden de las sustituciones es: \code{","} por \code{"."}, \code{"\%"}
#'     por \code{""} y, después, \code{pat} por \code{rep}. Luego se eliminan los
#'     espacios iniciales y finales y se convierte con \code{as.numeric()}.
#'   \item Cada patrón se sustituye una sola vez por elemento (se usa
#'     \code{stringr::str_replace()}): por ejemplo, \code{"1,5,3"} o \code{"5\%\%"}
#'     pasan a \code{NA}. Los espacios internos y los separadores de millares
#'     (\code{"1 000"}, \code{"1.234,5"}) tampoco se tratan y dan \code{NA}.
#'   \item Si algún valor (no perdido) no se puede convertir, se emiten dos avisos:
#'     el de \code{as.numeric()} (\code{NAs introduced by coercion}) y otro con la
#'     lista de los valores originales descartados y el nombre de la variable.
#'     Los valores vacíos y los espacios en blanco también se consideran perdidos.
#'   \item Efecto secundario: en ese caso, el mensaje se registra también con
#'     \code{desc_changes()}, que lo guarda en la variable global
#'     \code{list_changes} del entorno global.
#'   \item Si \code{pat} y \code{rep} no tienen la misma longitud, o solo se indica
#'     \code{pat}, se produce un error.
#' }
#'
#' @seealso \code{\link{desc_changes}}, \code{\link[stringr]{str_replace}},
#'   \code{\link[purrr]{reduce2}}, \code{\link[base]{as.numeric}}
#'
#' @examples
#' edad <- c("6", "6,7", "10 ", "12.5", "3%   ", "4,", "no se", ">2", "4pct")
#' Hmisc::label(edad) <- "Edad de los pacientes"
#' edad_num <- var_to_num(edad)   # avisa de los valores convertidos a NA
#' edad_num
#' Hmisc::label(edad_num)
#'
#' # Eliminar también otros patrones (pat y rep deben tener la misma longitud)
#' var_to_num(edad, pat = c(">", "pct"), rep = c("", ""))
#'
#' @keywords numeric character class change warnings
#'
#' @import magrittr purrr
#' @importFrom stringr fixed str_replace
#' @importFrom stats na.omit
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
var_to_num <- function(
  x ,
  pat = NULL, 
  rep = NULL){
  
### ---0. Inicio ---
  txt_col <- deparse(substitute(x))
  orig <- x  #variable original

### --- 1. Modificación ---
### Substitución ',' y '.'
### Eliminación de '%'
  x_new <-  as.numeric(trimws(reduce2(fixed(c(',', '%', pat)), fixed(c('.', '',rep)), .init = x, str_replace)))

  na_value <-   na.omit(unique(orig[is.na(x_new)]))

### --- 2. Conserva etiquetas ---
  Hmisc::label(x_new) <- Hmisc::label(orig)

### --- 3. Registro y aviso de valores 'NA' ---
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
