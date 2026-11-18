#' @title Forest plot para modelos multinomiales multivariables
#'
#' @description
#' Genera un forest plot (Relative Risk Ratio con IC95\%) para un modelo de
#' regresión multinomial multivariable, con un panel por cada nivel de la variable
#' de resultado (\code{Outcome}), mediante \code{ggplot2::facet_grid()}. La
#' estimación puntual y, opcionalmente, el intervalo de confianza y el p-valor se
#' muestran como una etiqueta de texto anclada directamente sobre cada punto,
#' ligeramente desplazada hacia arriba y a la derecha (al estilo de
#' \code{sjPlot::plot_model()}), en lugar de una columna de texto alineada aparte.
#' Este anclaje por punto evita el problema de recorte o solapamiento de texto
#' entre paneles que aparece cuando el texto se sitúa en una columna común a una
#' posición fija del eje X (véase \sQuote{Details}).
#'
#' El subtítulo incorpora la categoría de referencia de la variable resultado
#' (\code{show.ref.outcome}), imprescindible para interpretar las RRR: cada panel
#' compara su categoría frente a esa referencia, que por definición no tiene panel
#' propio.
#'
#' @param x Lista resultante de \code{\link{desc_multimod}} para un modelo
#'   multinomial (\code{x$model_type == "multinomial"}). Debe contener, como
#'   mínimo, un data.frame \code{x$results} con las columnas \code{Outcome},
#'   \code{Variable}, \code{Level}, \code{Relative Risk Ratio}, \code{Lower95},
#'   \code{Upper95} y \code{P.value}. Si existen \code{x$lbls_vars},
#'   \code{x$lbls_levels}, \code{x$model}, \code{x$N}, \code{x$R2_McFadden} y
#'   \code{x$R2_McFadden_ajustado}, se utilizan para las etiquetas, la categoría de
#'   referencia y el pie del gráfico.
#' @param title Título principal del gráfico. Por defecto,
#'   \code{"Modelo multinomial multivariable"}.
#' @param subtitle Subtítulo del gráfico. Por defecto,
#'   \code{"Relative Risk Ratio (IC95\%)"}.
#' @param xlab Etiqueta del eje X. Por defecto, \code{"RRR"}.
#' @param palette Paleta de colores. \code{"okabe"}/\code{"okabe_ito"} usa la
#'   paleta Okabe-Ito; \code{NULL} usa \code{\link{gg_color}}; o bien un vector de
#'   colores. En todos los casos los colores se reciclan hasta el número de niveles
#'   de \code{Outcome}. Por defecto, \code{"okabe"}.
#' @param point_size Tamaño de los puntos de estimación. Por defecto, 2.8.
#' @param text_size Tamaño de letra de la etiqueta anclada a cada punto. Por
#'   defecto, 3.
#' @param show.effect.column Lógico. Si es \code{TRUE} (por defecto), la etiqueta
#'   incluye la estimación puntual y el IC95\%.
#' @param show.p Lógico. Si es \code{TRUE} (por defecto), la etiqueta incluye el
#'   p-valor.
#' @param show.p.label Lógico. Si es \code{TRUE} (por defecto), los p-valores por
#'   encima de \code{pval_cut} se muestran como \code{"NS"} en lugar del valor
#'   numérico.
#' @param hide.ns Lógico. Si es \code{TRUE} (por defecto), se oculta por completo
#'   la etiqueta de texto (aunque no el punto ni el intervalo) para las filas no
#'   significativas.
#' @param shape.label Lógico. Si es \code{TRUE} (por defecto), se usa una forma
#'   distinta (triángulo) para los puntos significativos
#'   (\code{P.value < pval_cut}).
#' @param pval_cut Umbral de significación estadística. Debe ser un único número
#'   entre 0 y 1. Por defecto, 0.05.
#' @param text_sep Separador entre el bloque de estimación/IC y el p-valor dentro
#'   de la etiqueta. Por defecto, \code{"; "}.
#' @param label_dx Factor multiplicativo aplicado sobre el valor de
#'   \code{Relative Risk Ratio} de cada fila para desplazar la etiqueta
#'   ligeramente a la derecha del punto (la escala del eje X es log10, por lo que
#'   el desplazamiento debe ser multiplicativo, no aditivo). Debe ser mayor que 1.
#'   Por defecto, 1.05.
#' @param label_dy Desplazamiento aditivo, en unidades de \code{y} (posiciones
#'   discretas de variable/nivel), aplicado para situar la etiqueta por encima del
#'   punto. Por defecto, 0.25.
#' @param x_limit_mult Factor multiplicativo sobre el máximo redondeado de la
#'   escala (\code{x_plot_max}) que determina el límite derecho del eje X, dejando
#'   margen para que las etiquetas de los puntos más extremos no queden cortadas.
#'   Debe ser mayor que 1. Por defecto, 1.8.
#' @param show.null.band Lógico. Si es \code{TRUE} (por defecto), se dibuja una
#'   banda sombreada alrededor del valor nulo (\code{null_band}).
#' @param null_band Vector numérico de longitud 2 con los límites inferior y
#'   superior de la banda del valor nulo. Por defecto, \code{c(0.95, 1.05)}.
#' @param show.model.info Lógico. Si es \code{TRUE}, se añade un pie de página
#'   (\code{caption}) con la información del modelo (N, R2 de McFadden y R2 de
#'   McFadden ajustado). Por defecto, \code{FALSE}.
#' @param show.ref.outcome Lógico. Si es \code{TRUE} (por defecto), el subtítulo
#'   indica la categoría de referencia de la variable resultado, frente a la cual
#'   se interpretan todas las RRR de los paneles.
#' @param ref_outcome Categoría de referencia de la variable resultado. Si es
#'   \code{NULL} (por defecto), se obtiene automáticamente del modelo almacenado en
#'   \code{x$model} (\code{model$lev[1]} para objetos \code{nnet::multinom}; en su
#'   defecto, el primer nivel de la variable respuesta del \code{model.frame()}).
#'   Indíquela explícitamente solo si la detección automática no es posible.
#' @param ref_outcome_label Texto que precede a la categoría de referencia en el
#'   subtítulo. Por defecto, \code{"Categoría de referencia"}.
#'
#' @return Un objeto \code{ggplot} con un panel por cada nivel de \code{Outcome}
#'   (mediante \code{facet_grid()}), eje X en escala log10 y etiquetas de texto
#'   ancladas a cada punto.
#'
#' @details
#' A diferencia de una columna de texto alineada a una posición fija del eje X
#' (idéntica para todas las filas de un panel), aquí cada etiqueta se ancla a las
#' coordenadas \code{(Relative Risk Ratio, y)} de su propia fila, con un pequeño
#' desplazamiento (\code{label_dx}, \code{label_dy}) hacia arriba y a la derecha.
#' Al depender del valor de dato de cada fila y no de una posición compartida entre
#' todas las filas de un panel, la etiqueta queda dentro del rango de datos
#' representado en su propio panel con independencia del ancho físico que
#' \code{facet_grid()} le asigne, evitando así el recorte o solapamiento con el
#' panel contiguo que aparecía con el enfoque de columna común.
#'
#' Cuando \code{show.ref.outcome = TRUE} y se conoce la categoría de referencia, se
#' añade al subtítulo con la forma \code{"<subtitle> | <ref_outcome_label>:
#' <categoría>"}. Se necesita el paquete \pkg{ggtext}.
#'
#' @seealso \code{\link{desc_multimod}}, \code{\link{plot_forest_multimods_binom}},
#'   \code{\link{plot_forest_multimods}}, \code{\link{plot_forest_unimods}}
#'
#' @examples
#' \dontrun{
#' # Modelo multinomial resumido con desc_multimod()
#' mod <- nnet::multinom(Species ~ Sepal.Length + Petal.Width, data = iris, trace = FALSE)
#' res <- desc_multimod(mod)
#'
#' plot_forest_multimods_multi(res)
#'
#' # Con pie del modelo y mostrando tambien las filas no significativas
#' plot_forest_multimods_multi(res, show.model.info = TRUE, hide.ns = FALSE)
#' }
#'
#' @keywords forest-plot multinomial relative-risk-ratio multivariable ggplot
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
plot_forest_multimods <- function(
    results_df,
    lbls_vars = NULL,
    lbls_levels = NULL,
    estimate = 'Relative Risk Ratio',
    lower = 'Lower95',
    upper = 'Upper95',
    p = 'P.value',
    variable = 'Variable',
    level = 'Level',
    header.est.text = 'RRR',
    header.ic.text = 'IC95%',
    header.pval.text = 'P.value',
    text_sep = '; ',
    outcome = 'Outcome',
    pval_cut = 0.05,
    vars_plot = NULL,
    outcomes_plot = NULL,
    title = 'Análisis Multinomial',
    subtitle = 'Relative Risk Ratio (IC95%)',
    xlab = NULL,
    show.effect.column = TRUE,
    show.p = FALSE,
    show.p.label = FALSE,
    hide.ns = FALSE,
    show.null.band = TRUE,
    palette = NULL,
    point_size = 2.8,
    text_size = 3.75,
    text_spacing = 0.4
){

  if (!requireNamespace("ggtext", quietly = TRUE)) {
    stop(
      "El paquete 'ggtext' es necesario para la columna de texto combinada ",
      "(instala con install.packages('ggtext')).",
      call. = FALSE
    )
  }

  req_cols <- c(estimate = estimate, lower = lower, upper = upper,
                p = p, variable = variable, outcome = outcome)
  missing_cols <- req_cols[!req_cols %in% names(results_df)]
  if (length(missing_cols) > 0) {
    stop(
      "Las siguientes columnas no existen en results_df: ",
      paste0(names(missing_cols), " = '", missing_cols, "'", collapse = ", "),
      ". Estos argumentos deben ser el NOMBRE EXACTO de una columna real; ",
      "para el texto de la cabecera usa 'header.est.text'/'header.ic.text'/",
      "'header.pval.text' en su lugar. Columnas disponibles: ",
      paste(names(results_df), collapse = ", "),
      call. = FALSE
    )
  }

  plot_data <- results_df

  if (!is.null(vars_plot)) {
    plot_data <- plot_data |> dplyr::filter(.data[[variable]] %in% vars_plot)
  }
  if (!is.null(outcomes_plot)) {
    plot_data <- plot_data |> dplyr::filter(.data[[outcome]] %in% outcomes_plot)
  }

  plot_data <- forest_build_labels(plot_data, variable, level, lbls_vars, lbls_levels)

  plot_data <- plot_data |>
    dplyr::mutate(
      label_ci = sprintf('%.2f (%.2f, %.2f)', .data[[estimate]], .data[[lower]], .data[[upper]]),
      label_p = dplyr::case_when(
        is.na(.data[[p]]) ~ '',
        show.p.label & .data[[p]] >= pval_cut ~ 'NS',
        TRUE ~ format.pval(.data[[p]], digits = 3, eps = 0.001)),
      significant = !is.na(.data[[p]]) & .data[[p]] < pval_cut,
      sig_shape = factor(
        ifelse(significant, 'p < 0.05', 'p ≥ 0.05'),
        levels = c('p ≥ 0.05', 'p < 0.05')))

  # --- Columna de texto única (gestión de NA cuando hide.ns = TRUE) --------
  plot_data <- plot_data |>
    dplyr::mutate(
      label_html = dplyr::case_when(
        hide.ns & !significant ~ NA_character_,
        show.effect.column & show.p ~ paste0(label_ci, text_sep, '<b>', label_p, '</b>'),
        show.effect.column & !show.p ~ label_ci,
        !show.effect.column & show.p ~ paste0('<b>', label_p, '</b>'),
        TRUE ~ NA_character_))

  header_text <- if (show.effect.column && show.p) {
    paste0(header.est.text, " (", header.ic.text, ")", text_sep, header.pval.text)
  } else if (show.effect.column) {
    paste0(header.est.text, " (", header.ic.text, ")")
  } else if (show.p) {
    header.pval.text
  } else {
    NULL
  }

  outcomes <- unique(plot_data[[outcome]])

  # --- Paleta de color (Okabe-Ito opcional) ---------------------------------
  okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                  "#0072B2", "#D55E00", "#CC79A7", "#999999")

  if (is.null(palette)) {
    colors <- gg_color(length(outcomes))
  } else if (is.character(palette) && length(palette) == 1 &&
             tolower(palette) %in% c("okabe", "okabe_ito")) {
    if (length(outcomes) > length(okabe_ito)) {
      warning("Mas categorias de outcome que colores Okabe-Ito; se reciclara.")
    }
    colors <- rep_len(okabe_ito, length(outcomes))
  } else {
    if (length(outcomes) > length(palette)) {
      warning("Mas categorias de outcome que colores en 'palette'; se reciclara.")
    }
    colors <- rep_len(palette, length(outcomes))
  }

  # --- Tamano de texto dinamico si es NULL ----------------------------------
  y_levels_n <- length(unique(plot_data$y_label))
  if (is.null(text_size)) {
    text_size <- max(2.0, min(3.5, 3.8 - (y_levels_n * 0.03)))
  }

  # --- Interlineado: hueco corto entre niveles, largo entre variables ------
  y_levels <- rev(unique(plot_data$y_label))
  plot_data$y_label <- factor(plot_data$y_label, levels = y_levels)

  order_info <- plot_data |>
    dplyr::distinct(.data$y_label, .data$Variable_plot) |>
    dplyr::mutate(order_idx = match(as.character(.data$y_label), y_levels)) |>
    dplyr::arrange(.data$order_idx)

  y_positions <- stats::setNames(seq(1, by = 2, length.out = length(y_levels)), y_levels)
  offsets <- stats::setNames(seq(-.5, .5, length.out = length(outcomes)), outcomes)

  plot_data <- plot_data |>
    dplyr::mutate(
      y_base = y_positions[as.character(y_label)],
      y = y_base + offsets[.data[[outcome]]]
    )

  if (is.null(xlab)) xlab <- header.est.text

  # --- Posicion de la columna de texto: decadas log10, robusto a outliers --
  xmax <- max(plot_data[[upper]], na.rm = TRUE)
  log_xmax <- log10(xmax)
  x_text <- 10 ^ (log_xmax + text_spacing)
  x_sep  <- 10 ^ (log_xmax + text_spacing - 0.08)

  p1 <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = .data[[estimate]], y = y, colour = .data[[outcome]], shape = sig_shape)
  )

  if (show.null.band) {
    p1 <- p1 + ggplot2::annotate(
      "rect", xmin = .95, xmax = 1.05, ymin = -Inf, ymax = Inf,
      fill = "grey85", alpha = .15
    )
  }

  p1 <- p1 +
    ggplot2::geom_vline(xintercept = 1, linetype = 2, colour = "grey40") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data[[lower]], xmax = .data[[upper]]),
      width = .15, orientation = "y"
    ) +
    ggplot2::geom_point(size = point_size) +
    ggplot2::scale_colour_manual(values = colors) +
    ggplot2::scale_shape_manual(
      values = c('p ≥ 0.05' = 16, 'p < 0.05' = 17)) +
    ggplot2::scale_x_log10(
      labels = function(x) format(x, scientific = FALSE)) +
    ggplot2::scale_y_continuous(breaks = y_positions, labels = names(y_positions))

  if (!all(is.na(plot_data$label_html))) {
    p1 <- p1 +
      ggplot2::geom_vline(xintercept = x_sep, colour = "grey85", linewidth = .3) +
      ggtext::geom_richtext(
        data = plot_data,
        ggplot2::aes(x = x_text, y = y, label = label_html),
        hjust = 0, size = text_size, fill = NA, label.color = NA,
        colour = "black", inherit.aes = FALSE, na.rm = TRUE
      ) +
      ggplot2::annotate("text", x = x_text, y = max(plot_data$y) + 2,
                         label = header_text, fontface = "bold", hjust = 0,
                         size = text_size * 1.1)
  }

  has_text_column <- !all(is.na(plot_data$label_html))
  max_lab_width <- if (has_text_column) max(nchar(plot_data$label_html), na.rm = TRUE) else 0
  right_margin <- if (has_text_column) 40 + max_lab_width * (text_size * 2.8) + 20 else 40

  p1 +
    ggplot2::expand_limits(x = 10 ^ (log_xmax + text_spacing + (max_lab_width * 0.015) + 0.2)) +
    ggplot2::labs(
      title = title, subtitle = subtitle, x = xlab, y = NULL,
      colour = 'Outcome', shape = 'Outcome') +
    ggplot2::guides(
      colour = guide_legend(
        title = 'Outcome',
        override.aes = list(shape = 16, size = 3)),
      shape = guide_legend(
        title = 'Significación',
        override.aes = list(colour = 'black', size = 3))) +
    ggplot2::coord_cartesian(clip = 'off') +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(10, right_margin, 10, 10),
      panel.grid.minor = ggplot2::element_blank())
}
