#' @title Forest plot para modelos de regresión logística multinomial
#'
#' @description
#' Crea un forest plot a partir de los resultados de modelos de regresión
#' logística multinomial. Representa los riesgos relativos (Relative Risk Ratio,
#' RRR) con su IC95\% para cada categoría del resultado (\code{Outcome}) y cada
#' variable predictora, con un panel por variable.
#'
#' @param models_list Lista de modelos ajustados. Actualmente no se utiliza, pero
#'   se conserva por compatibilidad; como nunca se evalúa, puede omitirse.
#' @param results_df Data frame con los resultados del modelo (una fila por
#'   categoría del resultado y variable/nivel). Debe incluir las columnas
#'   \code{Outcome}, \code{Variable}, \code{Level}, \code{Relative Risk Ratio},
#'   \code{Lower95}, \code{Upper95} y \code{P.value}.
#' @param var_out_levels Vector de caracteres o \code{NULL}. Niveles de
#'   \code{Outcome} que se incluyen en el gráfico. Si es \code{NULL} (por defecto),
#'   se usan todos los de \code{results_df}.
#' @param vars_plot Vector de caracteres o \code{NULL}. Subconjunto de variables
#'   predictoras a incluir. Si es \code{NULL} (por defecto), se incluyen todas.
#' @param pval_cut Umbral de p-valor que define la significación estadística
#'   (por defecto, 0.05).
#'
#' @return Un objeto \code{ggplot2} con el forest plot de los resultados.
#'
#' @details
#' La función espera un data frame de resultados con, al menos, las categorías del
#' resultado, las variables predictoras, los riesgos relativos y sus intervalos de
#' confianza. Con los valores por defecto admite el elemento \code{results} de
#' \code{\link{desc_multimod}} o de \code{\link{desc_unimods_multi}} para modelos
#' multinomiales.
#'
#' El gráfico tiene un panel por variable (\code{facet_grid()} con escalas libres
#' en el eje Y). Dentro de cada fila, cada categoría de \code{Outcome} se
#' distingue por color (paleta \code{Set1}) y se desplaza verticalmente entre
#' -0.2 y 0.2 para evitar que se solapen. La forma del punto indica si el resultado
#' es significativo (\code{P.value < pval_cut}). Los riesgos relativos se muestran
#' en escala logarítmica. El título, el subtítulo y los ejes tienen textos fijos.
#'
#' @seealso \code{\link{plot_forest_multimods}},
#'   \code{\link{plot_forest_multimods_multi}}, \code{\link{desc_multimod}},
#'   \code{\link{desc_unimods_multi}}
#'
#' @examples
#' # Tabla de resultados de ejemplo (valores ilustrativos)
#' res <- data.frame(
#'   Outcome = rep(c("versicolor", "virginica"), each = 3),
#'   Variable = rep(c("Sepal.Length", "Petal.Width", "grupo"), 2),
#'   Level = rep(c("", "", "B"), 2),
#'   `Relative Risk Ratio` = c(1.5, 3.2, 0.8, 2.1, 6.0, 1.4),
#'   Lower95 = c(1.1, 1.5, 0.3, 1.2, 2.5, 0.5),
#'   Upper95 = c(2.0, 6.8, 2.1, 3.7, 14, 3.9),
#'   P.value = c(0.01, 0.003, 0.65, 0.008, 0.0001, 0.5),
#'   check.names = FALSE
#' )
#'
#' plot_multinomial_forest(results_df = res)
#'
#' # Solo algunas variables y una categoria del resultado
#' plot_multinomial_forest(
#'   results_df = res,
#'   vars_plot = c("Petal.Width", "grupo"),
#'   var_out_levels = "virginica"
#' )
#'
#' @keywords forest-plot multinomial relative-risk-ratio ggplot
#'
#' @importFrom ggplot2 ggplot aes geom_vline geom_errorbarh geom_point facet_grid
#' @importFrom ggplot2 scale_x_log10 scale_y_continuous scale_color_brewer scale_shape_manual
#' @importFrom ggplot2 labs theme_minimal theme element_text unit
#' @importFrom dplyr filter mutate arrange
#'
#' @author
#' Marcos Esteve \email{marcos.esteve@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
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
