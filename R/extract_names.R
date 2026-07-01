#' A extract_names Function
#'
#' Genera resumen para los coeficientes de laso distintos de 0 ( o todos)
#' @param varlev character vector. Nombre de la variable (var) y los niveles de los factores. Habitualmente, la salida de los coeficientes de un modelo.
#' @param var character vector. Original name of variable
#' @param del.intercept TRUE or FALSE. Remove "(Intercept)"
#' @keywords lasso coefficients models
#' @export extract_names
#' @import stringr
#' @examples
#' ch2clean <- c("otras_drogasSí",   "svp_ostomiaSí"  ,     "destino_al_altaCSS" , "localesSí"  ,
#' "fractura_luxacionSí", "edad"  ,   "minutos_cirugia"  ,   "fijacion_externaSi" )
#' var_orig <- c("variable1", "variable2" ,"otras_drogas" ,   "sexo", "hipertension",  "svp_ostomia"  ,     "destino_al_alta" ,  "locales"  ,
#'  "fractura_luxacion", "edad"    ,          "minutos_cirugia" ,  "fijacion_externa" )
#' extract_names (ch2clean, var_orig)



extract_names <- function (varlev, var, del.intercept = T){

  matches <- stringr::str_c(var, collapse = "|")
  levs <- stringr::str_replace_all(string = varlev, pattern = matches, "")

  levs_esc <- stringr::str_replace_all(levs, "([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1")

  names_var_sel <- stringr::str_replace_all(string = varlev, pattern = paste0(stringr::str_c(levs_esc, collapse = "$|"), "$"), "")

  if (del.intercept)
    names_var_sel <- names_var_sel[!names_var_sel %in% "(Intercept)"]

  names_var_sel <- names_var_sel[names_var_sel != ""]

  return(names_var_sel)
}
