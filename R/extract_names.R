#' @title Nombres de variable para los coeficientes de un modelo (p. ej. lasso)
#'
#' @description
#' Genera resumen para los coeficientes de lasso distintos de 0 (o todos).
#'
#' @param varlev Vector de caracteres. Nombre de la variable (\code{var}) y los niveles
#'   de los factores. Habitualmente, la salida de los coeficientes de un modelo.
#' @param var Vector de caracteres. Nombre original de la variable.
#' @param del.intercept \code{TRUE} o \code{FALSE}. Elimina \code{"(Intercept)"}.
#'
#' @return Vector de caracteres con los nombres de variable (sin el nivel del factor)
#'   correspondientes a cada elemento de \code{varlev}; si \code{del.intercept = TRUE},
#'   se excluye \code{"(Intercept)"}.
#'
#' @import stringr
#'
#' @examples
#' ch2clean <- c("otras_drogasSí", "svp_ostomiaSí", "destino_al_altaCSS",
#'                "localesSí", "fractura_luxacionSí", "edad",
#'                "minutos_cirugia", "fijacion_externaSi")
#' var_orig <- c("variable1", "variable2", "otras_drogas", "sexo",
#'                "hipertension", "svp_ostomia", "destino_al_alta",
#'                "locales", "fractura_luxacion", "edad",
#'                "minutos_cirugia", "fijacion_externa")
#' extract_names(ch2clean, var_orig)
#'
#' @keywords lasso coefficients models
#'
#' @author
#' Miriam Mota-Foix \email{mmota.foix@gmail.com}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus..
#'
#' @export
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
