#' Exportar un Data Frame limpio a Excel con formato
#'
#' Esta función limpia etiquetas HTML comunes (`<sub>`, `<sup>`, `<br>`, etc.) del contenido
#' y los nombres de columnas de un `data.frame`, y luego exporta el resultado a un archivo
#' Excel con estilo: celdas con texto ajustado (`wrap text`) y altura de fila aumentada.
#'
#' @param df Un `data.frame` con posibles etiquetas HTML en sus celdas o nombres de columnas.
#' @param file Nombre del archivo Excel de salida (por defecto: `"resultats_nets.xlsx"`).
#' @param sheet Nombre de la hoja del archivo Excel (por defecto: `"Resultats"`).
#'
#' @return No devuelve un objeto. Escribe un archivo `.xlsx` en el disco.
#'
#' @examples
#' \dontrun{
#' # Exportar un data.frame con etiquetas HTML
#' export_clean_excel(res$df_all, file = "informe.xlsx", sheet = "Pacients")
#' }
#'
#' @import openxlsx
#' @export
#'
export_clean_excel <- function(df, file = "resultats_nets.xlsx", sheet = "Resultats") {
  # Limpieza de contenido HTML en celdas


  # Aplicar limpieza al contenido
  df[] <- lapply(df, function(col) {
    if (is.character(col)) clean_html(col) else col
  })

  # Limpiar nombres de columnas
  colnames(df) <- clean_colnames(colnames(df))

  # Crear archivo Excel
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)

  # Estilo con salto de línea visible
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")

  # Escribir y aplicar estilo
  openxlsx::writeData(wb, sheet, df)
  openxlsx::addStyle(wb, sheet, style = wrap_style,
                     rows = 1:(nrow(df) + 1), cols = 1:ncol(df),
                     gridExpand = TRUE)
  openxlsx::setRowHeights(wb, sheet, rows = 2:(nrow(df) + 1), heights = 40)

  # Guardar archivo
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}


clean_html <- function(x) {
  x <- gsub("<sub>.*?</sub>", "", x)
  x <- gsub("<sup>.*?</sup>", "", x)
  x <- gsub("<br\\s*/?>", "\n", x)
  x <- gsub("<.*?>", "", x)
  trimws(x)
}

# Limpieza de nombres de columna
clean_colnames <- function(names_vec) {
  names_vec <- gsub("<sub>.*?</sub>", "", names_vec)
  names_vec <- gsub("<sup>.*?</sup>", "", names_vec)
  names_vec <- gsub("<br\\s*/?>", " ", names_vec)
  names_vec <- gsub("<.*?>", "", names_vec)
  trimws(names_vec)
}
