#' Forest Plot for Multinomial Logistic Regression Models
#'
#' @description
#' Creates a forest plot from multinomial logistic regression results.
#' The function visualizes odds ratios (ORs) with 95% confidence intervals
#' for multiple outcomes and predictor variables, allowing comparison across
#' categories in a faceted layout.
#'
#' @details
#' The function expects a tidy results data frame containing at least:
#' outcome categories, predictor variables, odds ratios, and confidence intervals.
#' Each outcome level is vertically jittered to avoid overlap.
#'
#' Odds ratios are displayed on a logarithmic scale.
#'
#' @param models_list List of fitted models (currently not used but kept for compatibility).
#' @param results_df Data frame containing model results. Must include:
#' \code{Outcome}, \code{Variable}, \code{Level}, \code{Odds Ratio},
#' \code{Lower95}, \code{Upper95}, and \code{P.value}.
#'
#' @param var_out_levels Character vector or NULL. Outcome levels to include in the plot.
#' If NULL, all levels in \code{results_df} are used.
#'
#' @param vars_plot Character vector or NULL. Subset of predictor variables to include.
#' If NULL, all variables are included.
#'
#' @param pval_cut Numeric. P-value threshold used to define statistical significance
#' (default: 0.05).
#'
#' @return A \code{ggplot2} object representing a forest plot of multinomial regression results.
#'
#' @importFrom ggplot2 ggplot aes geom_vline geom_errorbarh geom_point facet_grid
#' @importFrom ggplot2 scale_x_log10 scale_y_continuous scale_color_brewer scale_shape_manual
#' @importFrom ggplot2 labs theme_minimal theme element_text unit
#' @importFrom dplyr filter mutate arrange
#'
#' @examples
#' \dontrun{
#' # Example structure (results_df must be precomputed)
#' plot_multinomial_forest(
#'   models_list = NULL,
#'   results_df = results_example,
#'   var_out_levels = NULL,
#'   vars_plot = c("mpg", "hp"),
#'   pval_cut = 0.05
#' )
#' }
#'
#' @export
plot_multinomial_forest <- function(models_list,
                                    results_df,
                                    var_out_levels = NULL,
                                    vars_plot = NULL,
                                    pval_cut = 0.05) {
  
  plot_data <- results_df
  
  # Si no se especifican niveles, obtenerlos de results_df
  if (is.null(var_out_levels)) {
    var_out_levels <- unique(results_df$Outcome)
  }
  
  # Filtrado opcional de variables
  if (!is.null(vars_plot)) {
    plot_data <- plot_data %>%
      dplyr::filter(Variable %in% vars_plot)
  }
  
  # Preparación de las variables para los gráficos
  plot_data <- plot_data %>%
    dplyr::filter(Outcome %in% var_out_levels) %>%
    dplyr::mutate(
      y_label = paste0(Variable, "\n", Level),
      
      OR = `Relative Risk Ratio`,
      OR_lower = Lower95,
      OR_upper = Upper95,
      
      significant = P.value < pval_cut,
      Variable_group = Variable
    )
  
  # Orden de filas
  plot_data <- plot_data %>%
    dplyr::arrange(Variable_group, Level)
  
  # Posición base de cada fila
  y_levels <- unique(plot_data$y_label)
  
  plot_data <- plot_data %>%
    dplyr::mutate(
      y_base = match(y_label, y_levels)
    )
  
  # Creamos desplazamientos verticales para separar visualmente los outcomes
  outcomes <- sort(unique(plot_data$Outcome))
  
  offsets <- seq(
    from = -0.2,
    to = 0.2,
    length.out = length(outcomes)
  )
  # Asignamos un desplazamiento a cada outcome
  names(offsets) <- outcomes
  
  plot_data <- plot_data %>%
    dplyr::mutate(
      y = y_base + offsets[Outcome] # Posición final en Y (base + offset por outcome)
    )
  
  # Crear plot
  p <- ggplot(plot_data, aes(x = OR, y = y)) +
    
    # Línea OR = 1
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = "gray50"
    ) +
    
    # IC95%
    geom_errorbarh(
      aes(
        xmin = OR_lower,
        xmax = OR_upper,
        color = Outcome
      ),
      height = 0.15,
      linewidth = 0.8
    ) +
    
    # Punto estimado
    geom_point(
      aes(
        color = Outcome,
        shape = significant
      ),
      size = 3
    ) +
    
    # Facets por variable
    facet_grid(
      Variable_group ~ .,
      scales = "free_y",
      space = "free"
    ) +
    
    # Escala log
    scale_x_log10() +
    
    # Recuperar etiquetas originales
    scale_y_continuous(
      breaks = seq_along(y_levels),
      labels = y_levels
    ) +
    
    scale_color_brewer(
      palette = "Set1",
      name = "Outcome"
    ) +
    
    scale_shape_manual(
      values = c(16, 17),
      labels = c("No significativo", "Significativo"),
      name = "Significancia"
    ) +
    
    labs(
      title = "Forest Plot en Modelos Multinomiales",
      subtitle = "Relative Risk Ratio con IC95%",
      x = "Relative Risk Ratio",
      y = "Variable / Nivel"
    ) +
    
    theme_minimal() +
    
    theme(
      strip.text.y = element_text(
        angle = 0,
        hjust = 0,
        face = "bold"
      ),
      panel.spacing = unit(0.5, "lines"),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom"
    )
  
  return(p)
}
