#' @title Construir etiquetas Variable/Nivel para forest plots
#'
#' @description
#' Función auxiliar compartida por las funciones de forest plot. A partir de un
#' data frame de resultados de modelos (una fila por combinación
#' variable/nivel), genera tres columnas nuevas:
#' \itemize{
#'   \item \code{Variable_plot}: etiqueta descriptiva de la variable (o el nombre
#'     original si no hay \code{lbls_vars}).
#'   \item \code{Level_plot}: etiqueta descriptiva del nivel (o el código
#'     original si no hay \code{lbls_levels}), con el prefijo del nombre de la
#'     variable eliminado cuando corresponde (por ejemplo, \code{"sexMujer"}
#'     pasa a \code{"Mujer"}).
#'   \item \code{y_label}: etiqueta final combinada (\dQuote{Variable: Nivel})
#'     que se usa como eje Y del forest plot.
#' }
#'
#' Está pensada para su uso interno desde las funciones de forest plot, no para
#' llamarla directamente en un flujo de análisis normal.
#'
#' @param df Data frame de resultados (una fila por variable/nivel).
#' @param variable Nombre de la columna de \code{df} que identifica la variable
#'   (por ejemplo, \code{"Variable"}). Debe ser de tipo \code{character}.
#' @param level Nombre de la columna que identifica el nivel (por ejemplo,
#'   \code{"Level"}). Si es \code{NULL} o no existe en \code{df}, se considera
#'   que todas las variables son continuas (\code{Level_plot} es \code{NA} e
#'   \code{y_label} es igual a \code{Variable_plot}).
#' @param lbls_vars Vector con nombres (código de variable -> etiqueta). Si es
#'   \code{NULL}, se usa el nombre original de la variable.
#' @param lbls_levels Lista con nombres (código de variable -> vector con
#'   nombres nivel -> etiqueta). Si es \code{NULL}, se usa el código original del
#'   nivel. Es compatible con \code{desc_unimods_multi()$lbls_levels}.
#'
#' @return El mismo \code{df} de entrada con las columnas añadidas
#'   \code{Variable_plot}, \code{Level_plot} e \code{y_label}.
#'
#' @details
#' Si el nivel de una fila es \code{NA} o una cadena vacía, \code{y_label} es
#' únicamente la etiqueta de la variable.
#'
#' \strong{Limitaciones}
#' \itemize{
#'   \item \code{df[[variable]]} debería ser de tipo \code{character}. Si es un
#'     factor y alguna variable no figura en \code{lbls_vars}, \code{Variable_plot}
#'     recibe el código entero del factor en lugar del nombre.
#'   \item El mapeo de \code{lbls_levels} requiere que los nombres del vector
#'     coincidan exactamente con \code{Level_plot}, es decir, con el prefijo de la
#'     variable ya eliminado. Si no coinciden, se mantiene el código original sin
#'     avisar.
#'   \item El prefijo se elimina interpretando el nombre de la variable como
#'     expresión regular, por lo que los nombres con caracteres especiales pueden
#'     comportarse de forma inesperada.
#'   \item El mapeo de \code{lbls_levels} no está vectorizado (usa un bucle
#'     \code{for}), por lo que puede ser lento con tablas de resultados muy
#'     grandes.
#' }
#'
#' @seealso \code{\link{plot_forest_unimods}}, \code{\link{plot_forest_multimods}},
#'   \code{\link{plot_forest_multimods_binom}},
#'   \code{\link{plot_forest_multimods_multi}}, \code{\link{desc_unimods_multi}}
#'
#' @examples
#' df <- data.frame(
#'   Variable = c("cyl", "cyl", "hp"),
#'   Level    = c("cyl6", "cyl8", NA),
#'   estimate = c(1.4, 2.1, 1.02)
#' )
#'
#' lbls_vars <- c(cyl = "Cilindres", hp = "Potencia (hp)")
#' lbls_levels <- list(cyl = c(`6` = "6 cilindres", `8` = "8 cilindres"))
#'
#' forest_build_labels(df, "Variable", "Level", lbls_vars, lbls_levels)
#'
#' @keywords internal forest-plot labels
#'
#' @author
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
forest_build_labels <- function(df, variable, level, lbls_vars, lbls_levels) {

  df$Variable_plot <- as.character(df[[variable]])

  if (!is.null(lbls_vars)) {
    idx <- match(df[[variable]], names(lbls_vars))
    df$Variable_plot <- ifelse(is.na(idx), df[[variable]], unname(lbls_vars[idx]))
  }

  if (!is.null(level) && level %in% names(df)) {

    df$Level_plot <- as.character(df[[level]])

    # treu el prefix del nom de variable si el nivell l'incorpora (p.ex. "sexMujer")
    df$Level_plot <- mapply(
      function(var, lev) {
        if (is.na(lev)) return(NA_character_)
        sub(paste0("^", var), "", lev)
      },
      df[[variable]], df$Level_plot,
      USE.NAMES = FALSE
    )

    if (!is.null(lbls_levels)) {
      for (i in seq_len(nrow(df))) {
        var_i <- df[[variable]][i]
        lev_i <- df$Level_plot[i]
        if (!is.na(lev_i) && var_i %in% names(lbls_levels)) {
          lev_map <- lbls_levels[[var_i]]
          if (!is.null(lev_map) && !is.null(names(lev_map)) && lev_i %in% names(lev_map)) {
            df$Level_plot[i] <- lev_map[[lev_i]]
          }
        }
      }
    }

    df$y_label <- mapply(
      function(var_lab, lev) {
        if (is.na(lev) || lev == "") return(var_lab)
        paste0(var_lab, ": ", lev)
      },
      df$Variable_plot,
      df$Level_plot,
      SIMPLIFY = TRUE
    )

  } else {
    df$Level_plot <- NA_character_
    df$y_label <- df$Variable_plot
  }

  df
}
