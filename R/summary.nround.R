#' Resuelve el numero de decimales (nround) de forma dinamica
#'
#' Si \code{nround} es numerico se devuelve sin cambios (comportamiento historico).
#' Si \code{nround = "auto"} se aplica la regla de Hopkins (Hopkins et al., 2011;
#' citada por Cole, 2015): decimales suficientes para que la SD de la variable
#' completa (todos los grupos juntos) tenga \code{sig_sd} cifras significativas.
#' La media y el resto de estadisticos usan los mismos decimales que la SD.
#' El resultado se acota a [\code{nround_min}, \code{nround_max}]; con
#' \code{nround_min = 1} el modo "auto" solo puede AUMENTAR la precision respecto
#' al valor por defecto actual.
#'
#' Funcion compartida por las funciones descriptivas del paquete
#' (p. ej. \code{summary.quanti()}).
#'
#' @param x vector numerico (variable completa, sin separar por grupos).
#' @param nround numerico o "auto".
#' @param sig_sd cifras significativas de la SD. Por defecto 2.
#' @param nround_min,nround_max limites de decimales en modo "auto".
#' @return entero con el numero de decimales (o \code{nround} sin cambios si es numerico).
#' @references
#' Cole TJ. Too many digits: the presentation of numerical data.
#' Arch Dis Child. 2015;100(7):608-609. \doi{10.1136/archdischild-2014-307149}
#'
#' Hopkins WG, Batterham AM, Pyne DB, Impellizzeri FM. Misplaced decimal places.
#' Scand J Med Sci Sports. 2011;21(6):867-868. \doi{10.1111/j.1600-0838.2011.01393.x}
#' @author Miquel Vazquez-Santiago <miquel.vazquez@vhir.org>. BIDU-UEB. VHIR
#' @keywords internal
#' @rdname summary.nround
#' @export summary.nround
#' @examples
#' summary.nround(mtcars$wt, "auto")   # SD = 0.98 -> 2 decimales
#' summary.nround(mtcars$mpg, "auto")  # SD = 6.03 -> 1 decimal
#' summary.nround(mtcars$wt, 1)        # numerico: sin cambios
summary.nround <- function(x, nround = 1, sig_sd = 2, nround_min = 1, nround_max = 3) {
  if (is.numeric(nround)) return(nround)
  if (!identical(nround, "auto"))
    stop("'nround' debe ser numerico o \"auto\"")
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(as.integer(nround_min))
  d <- sig_sd - 1 - floor(log10(s))
  as.integer(max(nround_min, min(nround_max, d)))
}
