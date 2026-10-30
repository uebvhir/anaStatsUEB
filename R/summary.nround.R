#' Resuelve el numero de decimales (nround) de forma dinamica
#'
#' Si \code{nround} es numerico se devuelve sin cambios (comportamiento historico).
#' Si \code{nround = "auto"} se aplica la regla de Hopkins (citada por Cole, 2015):
#' decimales suficientes para que la SD de la variable completa (todos los grupos
#' juntos) tenga \code{sig_sd} cifras significativas. El resultado se acota a
#' [\code{nround_min}, \code{nround_max}]; con \code{nround_min = 1} el modo "auto"
#' solo puede AUMENTAR la precision respecto al valor por defecto actual.
#'
#' Funcion interna compartida por las funciones descriptivas del paquete
#' (p. ej. \code{summary.quanti()}).
#'
#' @param x vector numerico (variable completa, sin separar por grupos).
#' @param nround numerico o "auto".
#' @param sig_sd cifras significativas de la SD. Por defecto 2.
#' @param nround_min,nround_max limites de decimales en modo "auto".
#' @return entero con el numero de decimales (o \code{nround} sin cambios si es numerico).
#' @author Miquel Vazquez-Santiago <miquel.vazquez@vhir.org>. BIDU-UEB. VHIR
#' @keywords internal
#' @noRd
summary.nround <- function(x, nround = 1, sig_sd = 2, nround_min = 1, nround_max = 3) {
  if (is.numeric(nround)) return(nround)
  if (!identical(nround, "auto"))
    stop("'nround' debe ser numerico o \"auto\"")
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(as.integer(nround_min))
  d <- sig_sd - 1 - floor(log10(s))
  as.integer(max(nround_min, min(nround_max, d)))
}
