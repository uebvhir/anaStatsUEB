#' @title Porcentaje de datos faltantes por variable
#'
#' @description
#' Calcula el porcentaje de valores faltantes (\code{NA}) de cada variable de un
#' data frame e identifica las variables cuyo porcentaje supera un límite.
#'
#' @param df Data frame que contiene las variables a evaluar.
#' @param maxNA Porcentaje máximo de \code{NA} permitido, entre 0 y 100. Las
#'   variables con un porcentaje estrictamente superior se consideran
#'   excluidas. Por defecto, 80.
#'
#' @return
#' Lista con dos elementos:
#' \describe{
#'   \item{perc}{Vector con nombres (las variables de \code{df}) con el porcentaje
#'     de \code{NA} de cada una, redondeado a 2 decimales y ordenado de menor a
#'     mayor.}
#'   \item{var}{Vector de caracteres con los nombres de las variables cuyo
#'     porcentaje de \code{NA} es mayor que \code{maxNA}. Es \code{character(0)}
#'     si ninguna lo supera.}
#' }
#'
#' @details
#' La comparación con \code{maxNA} es estricta: una variable con exactamente
#' \code{maxNA} por ciento de \code{NA} no se incluye en \code{var}. El cálculo se
#' hace con \code{apply(df, 2, ...)}, por lo que \code{df} se convierte
#' internamente en una matriz.
#'
#' @seealso \code{\link{desc_lasso}}
#'
#' @examples
#' df <- data.frame(
#'   a = c(1, NA, 3, 4),
#'   b = c(NA, NA, NA, 1),
#'   c = c("x", NA, "y", "z"),
#'   d = c(NA, NA, NA, NA)
#' )
#'
#' res <- NAperc(df, maxNA = 50)
#' res$perc
#' res$var
#'
#' @keywords na missing percentage
#'
#' @author
#' Miriam Mota \email{miriam.mota@@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
NAperc <- function(df, maxNA = 80) {
    na <- list()
    na$perc <- sort(apply(df, 2,
                          function(x) round( (sum(is.na(x)) / length(x)) * 100, 2 ) ) )
    na$var <- names(na$perc)[na$perc > maxNA]
    return(na)
}
