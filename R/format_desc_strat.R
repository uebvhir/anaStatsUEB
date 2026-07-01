#' A format_desc_strat Function
#'
#' Formatea una tabla descriptiva estratificada en formato HTML utilizando
#' \code{kableExtra}. Añade automáticamente cabeceras agrupadas por estrato,
#' aplica estilos visuales, incorpora notas al pie y resalta las variables
#' que presentan al menos un p-valor inferior al punto de corte especificado.
#'
#' @param results_filtrados data frame con los resultados previamente generados
#' y combinados por estratos.
#' @param caption título de la tabla. Por defecto \code{"Resumen de resultados"}.
#' @param pval_cut punto de corte para resaltar variables con asociación
#' estadísticamente significativa. Por defecto 0.05.
#' @param font_size tamaño de letra de la tabla html. Por defecto 12.
#' @param width_lev ancho de la columna de niveles. Por defecto 25.
#' @param col.background color de fondo utilizado en las cabeceras de la tabla.
#' Por defecto \code{"#993489"}.
#' @param col.varsel color de fondo utilizado para resaltar las variables
#' seleccionadas según el criterio de p-valor. Por defecto \code{"#ebe0e9"}.
#' @param footnote texto o vector de textos que se añadirán como nota al pie
#' de la tabla. Por defecto NULL.
#'
#' @return Un objeto \code{kableExtra} en formato HTML con:
#' \itemize{
#'   \item Cabeceras agrupadas automáticamente por estrato.
#'   \item Formato visual personalizado.
#'   \item Notas al pie.
#'   \item Resaltado de variables con p-valores inferiores a \code{pval_cut}.
#' }
#'
#' @details
#' La función identifica automáticamente los estratos a partir del nombre de las
#' columnas, asumiendo que siguen la estructura:
#'
#' \code{Estrato_variable}
#'
#' donde la parte previa al primer guion bajo corresponde al nombre del estrato.
#'
#' Si alguna variable presenta al menos un p-valor menor o igual que
#' \code{pval_cut}, todas las filas correspondientes a dicha variable se resaltan
#' utilizando el color definido en \code{col.varsel}.
#'
#' @export format_desc_strat
#' @import dplyr knitr kableExtra
#' @author Marcos Esteve \email{marcos.esteve@@vhir.org}
#' @examples
#' df_res <- data.frame(
#'   variable = c("Edad", "", "Sexo", ""),
#'   levels = c("", "", "Hombre", "Mujer"),
#'   GrupoA_summary = c("65 (10)", "", "20 (40%)", "30 (60%)"),
#'   GrupoA_p.value = c("0.02", "", "0.15", ""),
#'   GrupoB_summary = c("68 (12)", "", "25 (50%)", "25 (50%)"),
#'   GrupoB_p.value = c("0.03", "", "0.08", "")
#' )
#'
#' # Formatear tabla estratificada
#' format_desc_strat(
#'   results_filtrados = df_res,
#'   caption = "Resultados estratificados"
#' )
#'
#' # Modificar colores y tamaño de fuente
#' format_desc_strat(
#'   results_filtrados = df_res,
#'   font_size = 14,
#'   col.background = "#4F81BD",
#'   col.varsel = "#D9EAD3"
#' )
#'
#' # Cambiar punto de corte de significación
#' format_desc_strat(
#'   results_filtrados = df_res,
#'   pval_cut = 0.01
#' )
#'
#' @keywords tables html formatting stratified kableExtra

format_desc_strat <- function(results_filtrados,
                              caption = "Resumen de resultados",
                              pval_cut = 0.05,
                              font_size = 12,
                              width_lev = 25,
                              col.background = "#993489",
                              col.varsel = "#ebe0e9",
                              footnote = NULL) {

  # Alineación de columnas
  align <- rep("c", ncol(results_filtrados))
  align[names(results_filtrados) == "levels"] <- "l"

  # P-valores para destacar filas
  # Filas a destacar según p-valor de la variable
  inicio_var <- which(results_filtrados$variable != "")
  colorRow <- c()
  pval_cols <- grep("p.value$", names(results_filtrados), value = TRUE)

  if(length(pval_cols) > 0){
    for(i in inicio_var){
      pvals <- sapply(pval_cols, function(col){
        suppressWarnings(
          as.numeric(
            sub("su.*", "",
                gsub("<", "", results_filtrados[[col]][i]))
          )
        )
      })
      if(any(pvals <= pval_cut, na.rm = TRUE)){
        fin <- inicio_var[which(inicio_var > i)[1]] - 1
        if (is.na(fin))
          fin <- nrow(results_filtrados)
        colorRow <- c(colorRow, i:fin)
      }
    }
  }

  # Construcción automática de cabecera superior
  cols_data <- names(results_filtrados)[
    !names(results_filtrados) %in% c("variable", "levels")
  ]

  # Extraer nombre del estrato (todo antes del primer "_")
  strat_names <- sub("_.*$", "", cols_data)

  # Forçar a mantenir l'ordre original d'aparició de les columnes
  strat_names <- factor(strat_names, levels = unique(strat_names))

  # Contar cuántas columnas tiene cada estrato
  header_vector <- c(" " = 2)

  tab_header <- table(strat_names)

  header_vector <- c(
    header_vector,
    as.numeric(tab_header)
  )

  names(header_vector)[-1] <- names(tab_header)

  # Limpiar nombres de columnas
  names(results_filtrados) <- c(
    names(results_filtrados)[1:2],
    sub("^[^_]+_", "", names(results_filtrados)[-c(1:2)])
  )

  # Crear tabla con formato HTML
  tabla <- results_filtrados %>%
    kable(escape = FALSE, row.names = FALSE, align = align, caption = caption, format = "html", table.attr = 'class="table-with-group-header"')

  tabla <- tabla %>% add_header_above(header_vector, background = col.background, color = "white", bold = TRUE)
  tabla <- tabla %>%
    kable_styling(
      latex_options = c("striped", "hold_position", "repeat_header"),
      font_size = font_size,
      full_width = FALSE,
      fixed_thead = TRUE
    ) %>%
    row_spec(0, background = col.background, color = "white") %>%
    column_spec(which(names(results_filtrados) == "levels"), width_max = width_lev) %>%
    column_spec(which(names(results_filtrados) == "variable"), bold = TRUE) %>%
    column_spec(which(names(results_filtrados) == "ALL"), bold = TRUE) %>%
    add_footnote(footnote, escape = FALSE, notation = "symbol")

  # Destacar filas con p < pval_cut
  if (length(colorRow) > 0) {
    tabla <- tabla %>%
      row_spec(colorRow, bold = FALSE, color = "black", background = col.varsel)
  }

  return(tabla)
}
