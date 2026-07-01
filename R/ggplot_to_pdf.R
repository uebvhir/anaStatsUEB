#' Guarda una lista de gráficos ggplot en un archivo PDF
#'
#' Esta función toma una lista de objetos ggplot y los guarda en un archivo PDF, organizándolos en una cuadrícula con un número especificado de filas y columnas.
#'
#' @param lista_ggplots Lista de gráficos en formato ggplot2 que se desean exportar.
#' @param row Número de filas en la disposición de gráficos por página. Por defecto, 2.
#' @param col Número de columnas en la disposición de gráficos por página. Por defecto, 2.
#' @param name.file Nombre del archivo PDF de salida. Por defecto, "graficos.pdf".
#'
#' @details La función calcula el número total de gráficos y determina cuántas páginas son necesarias para acomodarlos en el archivo PDF. Luego, usa `grid.arrange()` de `gridExtra` para organizar los gráficos en la cuadrícula definida por los parámetros `row` y `col`.
#'
#' @return No devuelve un objeto en R, sino que guarda los gráficos en un archivo PDF en el directorio de trabajo.
#'
#' @import ggplot2 gridExtra
#' @export
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(gridExtra)
#'
#' # Crear una lista de gráficos de ejemplo
#' p1 <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' p2 <- ggplot(mtcars, aes(hp, drat)) + geom_point()
#' p3 <- ggplot(mtcars, aes(qsec, disp)) + geom_point()
#' p4 <- ggplot(mtcars, aes(cyl, gear)) + geom_point()
#'
#' lista <- list(p1, p2, p3, p4)
#'
#' # Guardar los gráficos en un PDF
#' ggplot_to_pdf(lista, row = 2, col = 2, name.file = "mis_graficos.pdf")
#' }
#'
#'

ggplot_to_pdf <- function(lista_ggplots, row = 2, col = 2, name.file = "graficos.pdf") {
  total_graficos <- length(lista_ggplots)
  graficos_por_pagina <- row * col
  paginas_necesarias <- ceiling(total_graficos / graficos_por_pagina)

  pdf(name.file, width = 8.5, height = 11) # Ajusta el tamaño si es necesario

  for (i in 1:paginas_necesarias) {
    inicio <- ((i - 1) * graficos_por_pagina) + 1
    fin <- min(i * graficos_por_pagina, total_graficos)

    grid.arrange(grobs = lista_ggplots[inicio:fin], nrow = row, ncol = col)
  }

  dev.off() # Cierra el dispositivo PDF
}
