#' @title Tema gráfico corporativo UEB/VHIR para ggplot2
#'
#' @description
#' Tema gráfico corporativo basado en \pkg{ggplot2} para homogeneizar la
#' apariencia visual de los gráficos generados por el paquete.
#'
#' Está diseñado para gráficos descriptivos, forest plots, curvas ROC, curvas de
#' supervivencia y otros gráficos estadísticos. Se aplica igual que cualquier
#' tema de \pkg{ggplot2}, con el operador \code{+} al final de la cadena de
#' capas.
#'
#' @param base_theme Tema base de \pkg{ggplot2}: uno de \code{"minimal"} (por
#'   defecto), \code{"classic"}, \code{"bw"}, \code{"light"} o \code{"linedraw"}.
#' @param legend_position Posición de la leyenda: \code{"bottom"} (por defecto),
#'   \code{"right"} o \code{"none"}.
#' @param font_family Familia tipográfica. Por defecto, \code{"sans"}.
#' @param size_title Tamaño del título. Por defecto, 13.
#' @param size_subtitle Tamaño del subtítulo. Por defecto, 11.
#' @param size_axis Tamaño de las etiquetas de los ejes. Por defecto, 10.
#' @param size_axis_title Tamaño de los títulos de los ejes. Por defecto, 11.
#' @param size_legend Tamaño del texto de la leyenda. Por defecto, 10.
#' @param rotate_x Lógico. Si es \code{TRUE}, las etiquetas del eje X se inclinan
#'   45 grados. Por defecto, \code{FALSE}.
#' @param aspect_ratio Relación alto/ancho del panel (argumento
#'   \code{aspect.ratio} de \code{ggplot2::theme()}). Por defecto, \code{NULL}
#'   (sin relación fija).
#' @param border Lógico. Si es \code{FALSE}, elimina \code{panel.border} cuando
#'   \code{base_theme} es \code{"bw"}, \code{"light"} o \code{"linedraw"}. No
#'   tiene efecto con \code{"minimal"} ni \code{"classic"}, que no dibujan borde
#'   de panel. Por defecto, \code{TRUE}.
#' @param grid_major_y Lógico. Si es \code{FALSE} (por defecto), se elimina la
#'   rejilla principal del eje Y (\code{panel.grid.major.y}). Si es \code{TRUE},
#'   se dibuja con el color de \code{grid_major_y_color}.
#' @param grid_major_y_color Color de la rejilla principal del eje Y. Solo se
#'   usa si \code{grid_major_y = TRUE}. Por defecto, \code{NULL} (color por
#'   defecto del tema).
#' @param remove_minor_grid Lógico. Si es \code{TRUE}, se elimina la rejilla
#'   secundaria (\code{panel.grid.minor}). Si es \code{FALSE} (por defecto), se
#'   mantiene la del tema base.
#'
#' @return
#' Un objeto de clase \code{theme} que puede añadirse a cualquier gráfico
#' mediante el operador \code{+}.
#'
#' @details
#' \strong{Limitaciones}
#' \itemize{
#'   \item No implementa \dQuote{zebra striping} (bandas alternas de color de
#'     fondo por fila o categoría): al ser un tema, no tiene acceso a los datos
#'     del gráfico, así que ese efecto debe añadirse en la propia función de
#'     ploteo (por ejemplo, con \code{geom_rect()} antes de aplicar el tema).
#'   \item \code{aspect_ratio} fuerza una relación de aspecto fija del panel;
#'     combinado con \code{coord_cartesian(clip = "off")} (habitual en los forest
#'     plots del paquete) puede recortar anotaciones de texto situadas fuera del
#'     panel. Si se usan ambas cosas a la vez, conviene revisar el resultado
#'     visualmente.
#'   \item \code{base_theme} y \code{legend_position} se validan con
#'     \code{match.arg()}: distinguen mayúsculas de minúsculas, admiten
#'     coincidencia parcial (por ejemplo, \code{"cl"} se interpreta como
#'     \code{"classic"}) y un valor no reconocido produce un error.
#' }
#'
#' @seealso \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{theme_minimal}},
#'   \code{\link{plot_forest_unimods}}, \code{\link{plot_multi_roc}}
#'
#' @examples
#' library(ggplot2)
#'
#' mtcars_na <- mtcars
#' mtcars_na$hp[c(2, 10, 15)] <- NA
#'
#' ggplot(mtcars_na, aes(mpg, hp, colour = factor(cyl))) +
#'   geom_point(na.rm = TRUE) +
#'   gg_theme(base_theme = "classic", legend_position = "bottom")
#'
#' ggplot(mtcars_na, aes(mpg, hp)) +
#'   geom_point(na.rm = TRUE) +
#'   gg_theme(base_theme = "bw", border = FALSE, rotate_x = TRUE)
#'
#' @keywords plots ggplot theme graphics
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
gg_theme <- function(
    base_theme = c("minimal", "classic", "bw", "light", "linedraw"),
    legend_position = c("bottom", "right", "none"),
    font_family = "sans",
    size_title = 13,
    size_subtitle = 11,
    size_axis = 10,
    size_axis_title = 11,
    size_legend = 10,
    rotate_x = FALSE,
    aspect_ratio = NULL,
    border = TRUE,
    grid_major_y = FALSE,
    grid_major_y_color = NULL,
    remove_minor_grid = FALSE
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
      panel.grid.major.y = if(grid_major_y){
        ggplot2::element_line(color = grid_major_y_color)
        } else {
        ggplot2::element_blank()},
      panel.grid.minor = if(remove_minor_grid){
          ggplot2::element_blank()
        } else {
          ggplot2::element_line()
        },
      plot.margin = ggplot2::margin(10, 10, 10, 10),
      aspect.ratio = aspect_ratio
    )

  if (!border && base_theme %in% c('bw', 'light', 'linedraw')) {
    th <- th + ggplot2::theme(panel.border = ggplot2::element_blank())
  }

  th
}
