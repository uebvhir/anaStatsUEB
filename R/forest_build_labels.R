#' Construir etiquetes Variable/Nivell per a forest plots
#'
#' @description
#' Funció interna compartida per \code{plot_forest_uni()} i
#' \code{plot_forest_multimod()}. A partir d'un data frame de resultats de
#' models (una fila per combinació variable/nivell), genera tres columnes
#' noves:
#' \itemize{
#'   \item \code{Variable_plot}: etiqueta descriptiva de la variable (o el
#'   nom cru si no hi ha \code{lbls_vars}).
#'   \item \code{Level_plot}: etiqueta descriptiva del nivell (o el codi cru
#'   si no hi ha \code{lbls_levels}), amb el prefix del nom de variable
#'   eliminat quan escaigui (p. ex. \code{"sexMujer"} -> \code{"Mujer"}).
#'   \item \code{y_label}: etiqueta final combinada ("Variable: Nivell") que
#'   s'usa com a eix Y del forest plot.
#' }
#'
#' Feu servir aquesta funció NOMÉS de manera interna des de
#' \code{plot_forest_uni()} / \code{plot_forest_multimod()}; no està pensada
#' per cridar-la directament en un flux d'anàlisi normal.
#'
#' @param df Data frame de resultats (una fila per variable/nivell).
#' @param variable Nom de la columna que identifica la variable (per defecte
#'   \code{"Variable"}).
#' @param level Nom de la columna que identifica el nivell (per defecte
#'   \code{"Level"}). Si és \code{NULL} o no existeix a \code{df}, es
#'   considera que totes les variables són contínues (\code{y_label} =
#'   \code{Variable_plot}).
#' @param lbls_vars Vector amb noms (codi de variable -> etiqueta). Si és
#'   \code{NULL}, s'usa el nom cru de la variable.
#' @param lbls_levels Llista amb noms (codi de variable -> vector amb noms
#'   nivell -> etiqueta). Si és \code{NULL}, s'usa el codi cru del nivell.
#'   Compatible directament amb \code{desc_unimods_multi()$lbls_levels}.
#'
#' @return El mateix \code{df} d'entrada amb les columnes afegides
#'   \code{Variable_plot}, \code{Level_plot} i \code{y_label}.
#'
#' @details
#' \strong{Limitacions}
#' \itemize{
#'   \item Assumeix que \code{df[[variable]]} i (si escau) \code{df[[level]]}
#'   són vectors de tipus \code{character} o coercibles a \code{character};
#'   factors amb nivells \code{NA} generaran \code{y_label = NA}, que
#'   \code{ggplot2} eliminarà silenciosament del gràfic (amb un avís).
#'   \item El mapeig de \code{lbls_levels} requereix que els noms del vector
#'   coincideixin EXACTAMENT amb \code{Level_plot} (és a dir, amb el prefix
#'   de la variable ja eliminat). Si els noms no coincideixen, es manté el
#'   codi cru sense avisar.
#'   \item No vectoritza el mapeig de \code{lbls_levels} (fa servir un bucle
#'   \code{for}); amb taules de resultats molt grans (> 10.000 files) pot ser
#'   lent. Per als usos habituals (desenes/centenars de variables) no té
#'   impacte perceptible.
#' }
#'
#' @author
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
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
