#' Forest plot para modelos de regresión logística multinomial
#'
#' @description
#' Genera un forest plot a partir de resultados de modelos multinomiales
#' (Relative Risk Ratio con IC95%), con una columna de texto combinada
#' RRR(IC95%) [; p-valor] al estilo \code{summata}, alineada a la derecha
#' del panel gráfico y separada por una línea fina.
#'
#' @details
#'
#' \strong{Columnas de datos frente a etiquetas de visualización}: los
#' argumentos \code{estimate}, \code{lower}, \code{upper}, \code{p},
#' \code{variable}, \code{level} y \code{outcome} son \strong{nombres de
#' columna}: deben coincidir EXACTAMENTE con \code{names(results_df)}. En
#' cambio, \code{header.est.text}, \code{header.ic.text} y
#' \code{header.pval.text} son \strong{texto de visualización} para la
#' cabecera de la columna combinada; no tienen que coincidir con ningún
#' nombre de columna. Por ejemplo, \code{estimate = "Relative Risk Ratio"}
#' (columna real) + \code{header.est.text = "RRR"} (lo que se lee en la
#' cabecera) es la combinación habitual.
#'
#' \strong{Visualización de resultados}
#'
#' La combinación de \code{show.effect.column}, \code{show.p},
#' \code{show.p.label} y \code{hide.ns} permite distintos modos de
#' visualización de la columna de resultados:
#'
#' \itemize{
#'   \item \code{show.effect.column = TRUE}, \code{show.p = TRUE},
#'   \code{show.p.label = FALSE}, \code{hide.ns = FALSE}
#'   \describe{
#'     \item{Resultado}{Medida de efecto, IC y p-valor exacto:
#'       \code{1.03 (1.00, 1.06); 0.072}}
#'   }
#'   \item \code{show.effect.column = TRUE}, \code{show.p = TRUE},
#'   \code{show.p.label = TRUE}, \code{hide.ns = FALSE}
#'   \describe{
#'     \item{Resultado}{Los no significativos se muestran como \code{NS}:
#'       \code{1.03 (1.00, 1.06); NS}}
#'   }
#'   \item \code{show.effect.column = TRUE}, \code{show.p = FALSE}
#'   \describe{
#'     \item{Resultado}{Solo la medida de efecto con su IC95\%:
#'       \code{1.03 (1.00, 1.06)}}
#'   }
#'   \item \code{show.effect.column = FALSE}, \code{show.p = TRUE}
#'   \describe{
#'     \item{Resultado}{Solo los p-valores: \code{0.072} o \code{NS}.}
#'   }
#'   \item \code{show.effect.column = TRUE}, \code{show.p = TRUE},
#'   \code{show.p.label = TRUE}, \code{hide.ns = TRUE}
#'   \describe{
#'     \item{Resultado}{Las asociaciones no significativas se eliminan
#'       completamente de la columna de texto (\code{NA}); solo se
#'       muestran las significativas: \code{2.45 (1.38, 4.34); 0.002}}
#'   }
#' }
#'
#' Cuando \code{hide.ns = TRUE}, los puntos y los intervalos de confianza
#' continúan representándose en el forest plot; solo se oculta el texto de
#' los resultados no significativos. Al ser un \code{NA} intencionado,
#' \code{geom_richtext()} se llama con \code{na.rm = TRUE} para no generar
#' el aviso \code{"Removed N rows containing missing values"}.
#'
#' \strong{Limitaciones}
#' \itemize{
#'   \item Los RRR y los límites de confianza deben ser estrictamente
#'   positivos (escala log10).
#'   \item Requiere el paquete \pkg{ggtext} (para \code{geom_richtext()});
#'   si no está instalado, la función lanza un error explicativo.
#'   \item La paleta \code{"okabe_ito"} tiene 8 colores y el vector de
#'   formas por defecto tiene 8 valores; con más de 8 categorías de outcome
#'   ambos se reciclan (con aviso), dificultando distinguirlas.
#'   \item El color del texto de la columna de resultados es fijo
#'   (\code{'black'}); el color/forma por outcome se aplica solo a puntos y
#'   barras de error.
#'   \item \code{'text_size'} acepta valores inferiores a 3.75 sin solapamientos
#'   en etiquetas. 
#' }
#'
#' @param results_df Data frame con los resultados multinomiales (una fila
#'   por combinación variable/nivel/outcome).
#' @param lbls_vars Vector con nombres (columna \code{variable} -> etiqueta).
#'   Se pasa a \code{forest_build_labels()}.
#' @param lbls_levels Lista con nombres (columna \code{variable} -> vector
#'   nivel -> etiqueta). Se pasa a \code{forest_build_labels()}.
#' @param estimate Nombre EXACTO de columna con la medida de efecto
#'   representada por los puntos del forest plot. Habitualmente el Relative
#'   Risk Ratio (RRR) generado por \code{desc_unimods_multi()}, aunque puede
#'   usarse cualquier medida positiva representable en escala logarítmica
#'   (\code{"RRR"}, \code{"OR"}, \code{"HR"}...).
#' @param lower Nombre EXACTO de columna con el límite inferior del IC
#'   asociado a \code{estimate}.
#' @param upper Nombre EXACTO de columna con el límite superior del IC
#'   asociado a \code{estimate}.
#' @param p Nombre EXACTO de columna con los p-valores.
#' @param header.est.text Etiqueta de VISUALIZACIÓN para la medida de
#'   efecto en la cabecera (por defecto \code{"RRR"}).
#' @param header.ic.text Etiqueta de VISUALIZACIÓN para el intervalo de
#'   confianza en la cabecera (por defecto \code{"IC95%"}).
#' @param header.pval.text Etiqueta de VISUALIZACIÓN para el p-valor en la
#'   cabecera (por defecto \code{"P.value"}).
#' @param text_sep Separador entre la parte de efecto/IC y la de p-valor,
#'   usado tanto en cada fila de datos como en la cabecera (por defecto
#'   \code{"; "}).
#' @param variable Nombre EXACTO de columna que identifica la variable
#'   explicativa evaluada en cada modelo.
#' @param level Nombre EXACTO de columna con el nivel evaluado para
#'   variables categóricas. Para variables continuas puede dejarse en
#'   \code{NA}; se usa internamente para construir las etiquetas del eje Y.
#' @param outcome Nombre EXACTO de columna que identifica la categoría del
#'   outcome multinomial frente a la categoría de referencia.
#' @param pval_cut Punto de corte de significación estadística (por defecto
#'   0.05).
#' @param vars_plot Variables a representar (subconjunto opcional).
#' @param outcomes_plot Categorías de outcome a representar (subconjunto
#'   opcional).
#' @param title Título principal.
#' @param subtitle Subtítulo.
#' @param xlab Título eje X. Si \code{NULL} (por defecto), se usa
#'   \code{header.est.text}.
#' @param show.effect.column Mostrar la medida de efecto y su IC95\% en la
#'   columna de texto.
#' @param show.p Mostrar el p-valor en la columna de texto.
#' @param show.p.label Lógico. Si \code{TRUE}, los p-valores no
#'   significativos se muestran como \code{"NS"} (salvo si
#'   \code{hide.ns = TRUE}). Si \code{FALSE}, se muestran los valores
#'   exactos.
#' @param hide.ns Lógico. Si \code{TRUE}, no se muestra ningún texto para
#'   los resultados no significativos (\code{p >= pval_cut}); los puntos e
#'   IC se siguen dibujando.
#' @param show.null.band Mostrar banda sombreada alrededor de RRR=1.
#' @param palette Paleta de color para el outcome. Puede ser \code{NULL}
#'   (usa \code{gg_color(n)}), \code{"okabe"}/\code{"okabe_ito"} (paleta
#'   accesible Okabe-Ito) o un vector de colores propio (se recicla si hace
#'   falta). El texto de los resultados se representa siempre en negro,
#'   independientemente de la paleta.
#' @param point_size Tamaño de los puntos.
#' @param text_size Tamaño del texto de la columna de resultados. Si
#'   \code{NULL} (por defecto), se calcula dinámicamente según el número de
#'   filas (\code{max(2.0, min(3.5, 3.8 - n_rows * 0.03))}).
#' @param text_spacing Separación de la columna de texto respecto al IC
#'   máximo, en DÉCADAS log10 (por defecto 0.4).
#'
#' @return Objeto \code{ggplot}.
#'
#' @seealso \code{\link{forest_build_labels}}, \code{\link{plot_forest_uni}}
#' @importFrom ggtext geom_richtext
#'
#' @examples
#' \dontrun{
#' plot_forest_multimods(
#'   res$results, res$lbls_vars, res$lbls_levels,
#'   show.p = TRUE, show.p.label = TRUE, hide.ns = TRUE,
#'   palette = 'okabe'
#' )
#'}
#'
#' @author
#' Alba García Zarzoso \email{alba.garcia.zarzoso@vhir.org}
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
