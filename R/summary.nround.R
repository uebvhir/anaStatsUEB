#' @title Resolución dinámica del número de decimales (nround)
#'
#' @description
#' Devuelve el número de decimales que hay que usar al redondear una variable
#' cuantitativa. Si \code{nround} es numérico, se devuelve sin cambios
#' (comportamiento histórico). Si \code{nround = "auto"}, se aplica la regla de
#' Hopkins (Hopkins et al., 2011; citada por Cole, 2015): se usan los decimales
#' necesarios para que la desviación estándar (SD) de la variable completa (todos
#' los grupos juntos) tenga \code{sig_sd} cifras significativas. La media y el resto
#' de estadísticos usan los mismos decimales que la SD.
#'
#' Función compartida por las funciones descriptivas del paquete (por ejemplo,
#' \code{\link{summary.quanti}}).
#'
#' @usage
#' summary.nround(x, nround = 1, sig_sd = 2, nround_min = 1, nround_max = 3)
#'
#' @param x Vector numérico (variable completa, sin separar por grupos).
#' @param nround Número de decimales (numérico) o \code{"auto"} (por defecto, 1).
#' @param sig_sd Número de cifras significativas de la SD en el modo
#'   \code{"auto"} (por defecto, 2).
#' @param nround_min,nround_max Límites inferior y superior del número de
#'   decimales en el modo \code{"auto"} (por defecto, 1 y 3).
#'
#' @return Si \code{nround} es numérico, el mismo valor. Si es \code{"auto"}, un
#' entero con el número de decimales:
#' \code{sig_sd - 1 - floor(log10(SD))}, acotado a
#' [\code{nround_min}, \code{nround_max}].
#'
#' @details
#' \itemize{
#'   \item La SD se calcula con \code{stats::sd(x, na.rm = TRUE)}. Si no es finita
#'     o es 0 (por ejemplo, un vector constante, vacío o todo \code{NA}), se
#'     devuelve \code{nround_min}.
#'   \item Con \code{nround_min = 1}, el modo \code{"auto"} solo puede aumentar la
#'     precisión respecto al valor por defecto (1 decimal).
#'   \item Si \code{nround} no es numérico ni \code{"auto"}, se produce un error.
#'   \item Aunque el nombre recuerda a un método S3 de \code{summary()}, es una
#'     función normal: se llama directamente, no mediante \code{summary(x)}.
#' }
#'
#' @references
#' Cole TJ. Too many digits: the presentation of numerical data.
#' Arch Dis Child. 2015;100(7):608-609. \doi{10.1136/archdischild-2014-307149}
#'
#' Hopkins WG, Batterham AM, Pyne DB, Impellizzeri FM. Misplaced decimal places.
#' Scand J Med Sci Sports. 2011;21(6):867-868. \doi{10.1111/j.1600-0838.2011.01393.x}
#'
#' @seealso \code{\link{summary.quanti}}, \code{\link{desc_group}},
#'   \code{\link{summary.quali}}
#'
#' @examples
#' summary.nround(mtcars$wt, "auto")   # SD = 0.98 -> 2 decimales
#' summary.nround(mtcars$mpg, "auto")  # SD = 6.03 -> 1 decimal
#' summary.nround(mtcars$wt, 1)        # numérico: sin cambios
#' summary.nround(mtcars$disp, "auto", nround_min = 0)  # SD = 123.9 -> 0 decimales
#'
#' @keywords internal decimales redondeo
#'
#' @importFrom stats sd
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
summary.nround <- function(x, nround = 1, sig_sd = 2, nround_min = 1, nround_max = 3) {
  if (is.numeric(nround)) return(nround)
  if (!identical(nround, "auto"))
    stop("'nround' debe ser numerico o \"auto\"")
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(as.integer(nround_min))
  d <- sig_sd - 1 - floor(log10(s))
  as.integer(max(nround_min, min(nround_max, d)))
}
