#' @title Colores por defecto de ggplot2
#'
#' @description
#' Genera la paleta de colores que usa \pkg{ggplot2} por defecto para variables
#' discretas: \code{n} tonos equidistantes en la rueda de color HCL.
#'
#' @param n Número entero positivo con el total de colores a generar.
#'
#' @return Vector de caracteres de longitud \code{n} con los colores en formato
#'   hexadecimal (por ejemplo, \code{"#F8766D"}).
#'
#' @details
#' Los colores se calculan con \code{grDevices::hcl()}, con tonos entre 15 y 375
#' grados, luminancia 65 y croma 100. Con \code{n = 3} el resultado coincide con
#' el de \code{scales::hue_pal()(3)}. La función espera un \code{n} positivo: con
#' \code{n = 0} devuelve un color en lugar de un vector vacío.
#'
#' @seealso \code{\link[ggplot2]{scale_colour_hue}}, \code{\link[grDevices]{hcl}},
#'   \code{\link{plot_forest_unimods}}, \code{\link{plot_forest_multimods}},
#'   \code{\link{plot_multi_roc}}
#'
#' @examples
#' gg_color(3)
#' gg_color(5)
#'
#' @keywords plots descriptive ggplot color gg
#'
#' @importFrom grDevices hcl
#'
#' @author
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
gg_color <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}
