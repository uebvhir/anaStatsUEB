#' @title Tabla con estilo kable para HTML o LaTeX
#'
#' @description
#' Genera una tabla con estilo \code{kable} (vía \pkg{kableExtra}) a partir de
#' un data frame o matriz, pensada para su uso en informes de la UEB en
#' formato HTML o LaTeX. Permite añadir un título o leyenda, controlar el
#' tamaño de letra, el ancho de la tabla y el color de fondo de las filas, y
#' gestionar el escapado de caracteres especiales.
#'
#' @param tab Tabla (data frame o matriz) a formatear con \code{kable}.
#' @param caption Vector de caracteres con el título o leyenda de la tabla.
#'   Se puede establecer en \code{NULL} para suprimir la leyenda. Valor por
#'   defecto: \code{NULL}.
#' @param font_size Valor numérico para el tamaño de letra de la tabla.
#' @param full_width Valor lógico (\code{TRUE}/\code{FALSE}) que controla si
#'   la tabla HTML debe ocupar el 100\% del ancho. Como HTML y PDF tienen
#'   formatos preferentes distintos para \code{full_width}, si no se
#'   especifica, una tabla HTML tendrá ancho completo por defecto, pero esta
#'   opción se fijará en \code{FALSE} para una tabla LaTeX.
#' @param escape Valor lógico; indica si se deben escapar los caracteres
#'   especiales al generar tablas HTML o LaTeX. Si \code{escape = FALSE}, hay
#'   que asegurarse de que los caracteres especiales no provoquen errores de
#'   sintaxis en LaTeX o HTML.
#' @param col.background Cadena de caracteres con el color de fondo de las
#'   filas. Aquí hay que prestar atención a las diferencias en los códigos de
#'   color entre HTML y LaTeX.
#'
#' @return Un objeto de tabla con estilo \code{kable}/\code{kableExtra}, listo
#'   para su renderizado en HTML o LaTeX.
#'
#' @import kableExtra
#'
#' @examples
#' aa <- data.frame(num = rnorm(10), fact = factor(rep(c("A", "B", "C"), 10)))
#' kable_ueb(aa)
#' kable_ueb(aa, digits = 2, row.names = TRUE, position = "left")
#' kable_ueb(aa, digits = 2, row.names = TRUE, format = "latex")
#'
#' @keywords kable html ueb
#'
#' @author
#' Miriam Mota \email{miriam.mota@@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
kable_ueb <- function(tab,
                      caption = NULL,
                      font_size = 13,
                      full_width = FALSE,
                      escape = FALSE,
                      col.background = "#993489",
                      position = "center",
                      ...){
  options(knitr.kable.NA = '')
  kable(tab, caption = caption, escape = escape, format = "html", ...) %>%
    kable_styling(latex_options = c("striped","hold_position",
                                    "repeat_header"),
                  position = position,
                  bootstrap_options = c("striped"),
                  font_size = font_size,
                  full_width = full_width) %>%
    row_spec(0,background = col.background, color = "white")
}
