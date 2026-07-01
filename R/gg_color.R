#' A gg_color Function
#'
#' Genera el código de colores que usa la libreria ggplot
#' @param n número total de colores a generar
#' @export gg_color
#' @examples
#' gg_color(3)
#' @keywords plots descriptive  ggplot color gg



gg_color <- function(n) {
    hues <- seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
}
