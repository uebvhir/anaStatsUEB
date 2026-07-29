#' UEB/VHIR ggplot Theme
#'
#' @description
#' Tema gráfico corporativo basado en ggplot2 para homogeneizar la apariencia
#' visual de los gráficos generados por el paquete.
#'
#' Está diseñado para gráficos descriptivos, forest plots, curvas ROC,
#' curvas de supervivencia y otros gráficos estadísticos.
#'
#' @param base_theme Tema base de ggplot2.
#' @param legend_position Posición de la leyenda.
#' @param font_family Familia tipográfica.
#' @param size_title Tamaño del título.
#' @param size_subtitle Tamaño del subtítulo.
#' @param size_axis Tamaño de las etiquetas de los ejes.
#' @param size_axis_title Tamaño de los títulos de los ejes.
#' @param size_legend Tamaño del texto de la leyenda.
#' @param rotate_x Etiquetas del eje X inclinadas 45 grados.
#' @param aspect_ratio Relación ancho/alto del panel.
#'
#' @return
#' Un objeto de clase \code{theme} que puede añadirse a cualquier gráfico
#' mediante el operador \code{+}.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   gg_theme_ueb()
#' }
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Modificaciones y mantenimiento:
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @rdname gg_theme
#' @export
gg_theme <- function(
    base_theme = c('minimal', 'classic', 'bw', 'light', 'linedraw'),
    legend_position = c('bottom', 'right', 'none'),
    font_family = 'sans',
    size_title = 13,
    size_subtitle = 11,
    size_axis = 10,
    size_axis_title = 11,
    size_legend = 10,
    rotate_x = FALSE,
    aspect_ratio = NULL,
    border = TRUE
){
 
  base_theme <- match.arg(base_theme)
  legend_position <- match.arg(legend_position)
 
  selected_theme <- switch(
    base_theme,
    minimal  = ggplot2::theme_minimal(),
    classic  = ggplot2::theme_classic(),
    bw       = ggplot2::theme_bw(),
    light    = ggplot2::theme_light(),
    linedraw = ggplot2::theme_linedraw()
  )
 
  th <- selected_theme +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_family, colour = 'black'),
      plot.title = ggplot2::element_text(face = 'bold', size = size_title, hjust = .5),
      plot.subtitle = ggplot2::element_text(size = size_subtitle, hjust = .5),
      plot.caption = ggplot2::element_text(colour = 'grey40'),
      axis.title = ggplot2::element_text(face = 'bold', size = size_axis_title),
      axis.text.x = if (rotate_x) {
        ggplot2::element_text(angle = 45, hjust = 1, vjust = 1, size = size_axis)
      } else {
        ggplot2::element_text(size = size_axis)
      },
      axis.text.y = ggplot2::element_text(size = size_axis),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = 'bold', size = size_legend),
      legend.text = ggplot2::element_text(size = size_legend),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      aspect.ratio = aspect_ratio
    )
 
  if (!border && base_theme %in% c('bw', 'light', 'linedraw')) {
    th <- th + ggplot2::theme(panel.border = ggplot2::element_blank())
  }
 
  th
}