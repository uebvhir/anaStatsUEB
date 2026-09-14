#' @title Forest plot para modelos binomiales multivariables
#'
#' @description
#' Genera un forest plot (Odds Ratio con IC95%) para un modelo de
#' regresión logística binomial multivariable. La estimación puntual y,
#' opcionalmente, el intervalo de confianza y el p-valor se muestran
#' como una etiqueta de texto anclada directamente sobre cada punto,
#' ligeramente desplazada hacia arriba y a la derecha, siguiendo el
#' mismo estilo que \code{\link{plot_forest_multimods_multi}}. A
#' diferencia de esta última, al tratarse de un modelo con una única
#' categoría de resultado no se generan paneles por \code{Outcome}.
#'
#' @param x Lista resultante de \code{summarise_multivariable_model()}
#'   para un modelo binomial (\code{x$model_type == "binomial"}). Debe
#'   contener, como mínimo, un data.frame \code{x$results} con las
#'   columnas \code{Variable}, \code{Level}, \code{Odds Ratio},
#'   \code{CI (lower)}, \code{CI (upper)} y \code{Pr(>|z|)}.
#' @param title Título principal del gráfico.
#' @param subtitle Subtítulo del gráfico.
#' @param xlab Etiqueta del eje X.
#' @param palette Color de los puntos e intervalos. Acepta
#'   \code{"okabe"}/\code{"okabe_ito"} (se usa un único color fijo de
#'   la paleta Okabe-Ito), \code{NULL} (color por defecto de
#'   \pkg{ggplot2}), o un vector de color del que se toma el primer
#'   elemento. A diferencia de
#'   \code{\link{plot_forest_multimods_multi}}, aquí no existe una
#'   variable de agrupación (\code{Outcome}), por lo que solo se
#'   utiliza un único color para todo el gráfico.
#' @param point_size Tamaño de los puntos de estimación.
#' @param text_size Tamaño de letra de la etiqueta anclada a cada punto.
#' @param show.effect.column Lógico. Si es \code{TRUE}, la etiqueta
#'   incluye la estimación puntual y el IC95%.
#' @param show.p Lógico. Si es \code{TRUE}, la etiqueta incluye el
#'   p-valor.
#' @param show.p.label Lógico. Si es \code{TRUE}, los p-valores por
#'   encima de \code{pval_cut} se muestran como \code{"NS"} en lugar
#'   del valor numérico.
#' @param hide.ns Lógico. Si es \code{TRUE}, se oculta por completo la
#'   etiqueta de texto (aunque no el punto ni el intervalo) para las
#'   filas no significativas.
#' @param shape.label Lógico. Si es \code{TRUE}, se usa una forma
#'   distinta (triángulo) para los puntos significativos
#'   (\code{P.value < pval_cut}).
#' @param pval_cut Umbral de significación estadística. Debe ser un
#'   único número entre 0 y 1.
#' @param text_sep Separador entre el bloque de estimación/IC y el
#'   p-valor dentro de la etiqueta.
#' @param label_dx Factor multiplicativo aplicado sobre el valor de
#'   \code{Odds Ratio} de cada fila para desplazar la etiqueta
#'   ligeramente a la derecha del punto (la escala del eje X es log10,
#'   por lo que el desplazamiento debe ser multiplicativo, no
#'   aditivo). Debe ser mayor que 1.
#' @param label_dy Desplazamiento aditivo, en unidades de \code{y}
#'   (posiciones discretas de variable/nivel), aplicado para situar la
#'   etiqueta por encima del punto.
#' @param x_limit_mult Factor multiplicativo sobre el máximo de la
#'   escala que determina el límite derecho del eje X, dejando margen
#'   para que las etiquetas de los puntos más extremos no queden
#'   cortadas.
#' @param show.null.band Lógico. Si es \code{TRUE}, se dibuja una banda
#'   sombreada alrededor del valor nulo (\code{null_band}).
#' @param null_band Vector numérico de longitud 2 con los límites
#'   inferior y superior de la banda del valor nulo.
#' @param show.model.info Lógico. Si es \code{TRUE}, se añade un pie de
#'   página (\code{caption}) con la información del modelo (N, R2 de
#'   McFadden y R2 de McFadden ajustado).
#'
#' @return Un objeto \code{ggplot} con eje X en escala log10 y
#'   etiquetas de texto ancladas a cada punto.
#'
#' @details
#' Esta función comparte la lógica de etiquetado de
#' \code{\link{plot_forest_multivariable_multi}}: cada etiqueta se
#' ancla a las coordenadas \code{(Odds Ratio, y)} de su propia fila,
#' con un pequeño desplazamiento (\code{label_dx}, \code{label_dy})
#' hacia arriba y a la derecha, en lugar de usar una columna de texto
#' común a una posición fija del eje X. Al no existir una variable
#' \code{Outcome} en un modelo binomial, no se utiliza
#' \code{ggplot2::facet_grid()} y todo el gráfico se representa en un
#' único panel.
#'
#' @seealso \code{\link{plot_forest_multimods_multi}},
#'   \code{\link{plot_forest_multimods}},
#'   \code{\link{plot_forest_uniimods}}
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
plot_forest_multimods_binom <- function(
    x,
    title = "Modelo binomial multivariable",
    subtitle = "Odds Ratio (IC95%)",
    xlab = "OR",
    palette = "okabe",
    point_size = 2.8,
    text_size = 3,
    show.effect.column = TRUE,
    show.p = TRUE,
    show.p.label = TRUE,
    hide.ns = FALSE,
    shape.label = TRUE,
    pval_cut = 0.05,
    text_sep = "; ",
    label_dx = 1.05,
    label_dy = 0.25,
    x_limit_mult = 1.8,
    show.null.band = TRUE,
    null_band = c(0.95, 1.05),
    show.model.info = TRUE
) {

  # -----------------------------------------------------------------------
  # 1. Validaciones
  # -----------------------------------------------------------------------

  if (!is.list(x) || !identical(x$model_type, "binomial")) {
    stop(
      "'x' debe ser el resultado de summarise_multivariable_model() ",
      "para un modelo binomial.",
      call. = FALSE
    )
  }

  if (!requireNamespace("ggtext", quietly = TRUE)) {
    stop(
      "El paquete 'ggtext' es necesario. Instálalo con ",
      "install.packages('ggtext').",
      call. = FALSE
    )
  }

  required_columns <- c(
    "Variable",
    "Level",
    "Odds Ratio",
    "CI (lower)",
    "CI (upper)",
    "Pr(>|z|)"
  )

  missing_columns <- setdiff(
    required_columns,
    names(x$results)
  )

  if (length(missing_columns) > 0L) {
    stop(
      "Faltan las siguientes columnas en 'x$results': ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  logical_arguments <- list(
    show.effect.column = show.effect.column,
    show.p = show.p,
    show.p.label = show.p.label,
    hide.ns = hide.ns,
    shape.label = shape.label,
    show.null.band = show.null.band,
    show.model.info = show.model.info
  )

  invalid_logical <- names(logical_arguments)[
    !vapply(
      logical_arguments,
      function(z) is.logical(z) && length(z) == 1L && !is.na(z),
      logical(1)
    )
  ]

  if (length(invalid_logical) > 0L) {
    stop(
      "Los siguientes argumentos deben ser lógicos de longitud 1: ",
      paste(invalid_logical, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (
    !is.numeric(pval_cut) ||
    length(pval_cut) != 1L ||
    is.na(pval_cut) ||
    pval_cut <= 0 ||
    pval_cut >= 1
  ) {
    stop(
      "'pval_cut' debe ser un único número entre 0 y 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(label_dx) ||
    length(label_dx) != 1L ||
    is.na(label_dx) ||
    label_dx <= 1
  ) {
    stop(
      "'label_dx' debe ser un único número mayor que 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(label_dy) ||
    length(label_dy) != 1L ||
    is.na(label_dy)
  ) {
    stop(
      "'label_dy' debe ser un único número.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(x_limit_mult) ||
    length(x_limit_mult) != 1L ||
    is.na(x_limit_mult) ||
    x_limit_mult <= 1
  ) {
    stop(
      "'x_limit_mult' debe ser un único número mayor que 1.",
      call. = FALSE
    )
  }

  if (
    !is.numeric(null_band) ||
    length(null_band) != 2L ||
    anyNA(null_band) ||
    any(!is.finite(null_band)) ||
    any(null_band <= 0) ||
    null_band[1] >= null_band[2]
  ) {
    stop(
      "'null_band' debe contener dos valores positivos, finitos y ordenados.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # 2. Preparación y conversión de columnas numéricas
  # -----------------------------------------------------------------------

  numeric_columns <- c(
    "Odds Ratio",
    "CI (lower)",
    "CI (upper)",
    "Pr(>|z|)"
  )

  dat <- x$results

  for (column in numeric_columns) {

    original_values <- dat[[column]]

    converted_values <- suppressWarnings(
      as.numeric(original_values)
    )

    introduced_na <-
      !is.na(original_values) &
      is.na(converted_values)

    if (any(introduced_na)) {
      warning(
        sum(introduced_na),
        " valores no numéricos de la columna '",
        column,
        "' se han convertido a NA.",
        call. = FALSE
      )
    }

    dat[[column]] <- converted_values
  }

  # -----------------------------------------------------------------------
  # 3. Construcción de etiquetas
  # -----------------------------------------------------------------------

  dat <- forest_build_labels(
    dat,
    variable = "Variable",
    level = "Level",
    lbls_vars = x$lbls_vars,
    lbls_levels = x$lbls_levels
  )

  dat <- dat |>
    dplyr::mutate(
      significant =
        !is.na(.data$`Pr(>|z|)`) &
        .data$`Pr(>|z|)` < pval_cut,

      sig_shape = factor(
        ifelse(
          .data$significant,
          "p < 0.05",
          "p >= 0.05"
        ),
        levels = c(
          "p >= 0.05",
          "p < 0.05"
        )
      ),

      label_effect = sprintf(
        "%.2f (%.2f, %.2f)",
        .data$`Odds Ratio`,
        .data$`CI (lower)`,
        .data$`CI (upper)`
      ),

      label_p = dplyr::case_when(
        is.na(.data$`Pr(>|z|)`) ~ "",

        show.p.label &
          .data$`Pr(>|z|)` >= pval_cut ~ "NS",

        TRUE ~ format.pval(
          .data$`Pr(>|z|)`,
          digits = 3,
          eps = 0.001
        )
      ),

      label_html = dplyr::case_when(
        hide.ns &
          !.data$significant ~ NA_character_,

        show.effect.column &
          show.p ~ paste0(
            .data$label_effect,
            text_sep,
            "<b>",
            .data$label_p,
            "</b>"
          ),

        show.effect.column &
          !show.p ~ .data$label_effect,

        !show.effect.column &
          show.p ~ paste0(
            "<b>",
            .data$label_p,
            "</b>"
          ),

        TRUE ~ NA_character_
      )
    )

  if (nrow(dat) == 0L) {
    stop(
      "No hay resultados para representar.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # 4. Filtrado de valores incompatibles con la escala logarítmica
  # -----------------------------------------------------------------------

  valid_x <- with(
    dat,
    is.finite(`Odds Ratio`) &
      `Odds Ratio` > 0 &
      is.finite(`CI (lower)`) &
      `CI (lower)` > 0 &
      is.finite(`CI (upper)`) &
      `CI (upper)` > 0
  )

  if (!all(valid_x)) {
    warning(
      sum(!valid_x),
      " filas con OR o IC no positivos o no finitos ",
      "se han excluido del gráfico.",
      call. = FALSE
    )

    dat <- dat[
      valid_x,
      ,
      drop = FALSE
    ]
  }

  if (nrow(dat) == 0L) {
    stop(
      "No quedan valores positivos y finitos para representar ",
      "en escala log10.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # 5. Orden y posiciones del eje Y
  # -----------------------------------------------------------------------

  y_levels <- rev(
    unique(dat$y_label)
  )

  dat$y_label <- factor(
    dat$y_label,
    levels = y_levels
  )

  y_positions <- stats::setNames(
    seq_along(y_levels),
    y_levels
  )

  dat <- dat |>
    dplyr::mutate(
      y = unname(
        y_positions[
          as.character(.data$y_label)
        ]
      )
    )

  # -----------------------------------------------------------------------
  # 6. Color (único, sin agrupación por Outcome)
  # -----------------------------------------------------------------------

  okabe_ito <- c(
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7",
    "#999999"
  )

  point_colour <- if (is.null(palette)) {

    scales::hue_pal()(1)

  } else if (
    is.character(palette) &&
    length(palette) == 1L &&
    tolower(palette) %in% c(
      "okabe",
      "okabe_ito"
    )
  ) {

    okabe_ito[5]

  } else {

    if (
      !is.character(palette) ||
      length(palette) == 0L
    ) {
      stop(
        "'palette' debe ser NULL, 'okabe', 'okabe_ito' ",
        "o un vector de colores.",
        call. = FALSE
      )
    }

    palette[1]
  }

  # -----------------------------------------------------------------------
  # 7. Límites y marcas del eje X
  # -----------------------------------------------------------------------

  x_values <- c(
    dat$`Odds Ratio`,
    dat$`CI (lower)`,
    dat$`CI (upper)`
  )

  x_values <- x_values[
    is.finite(x_values) &
      x_values > 0
  ]

  x_data_min <- min(x_values)
  x_data_max <- max(x_values)

  x_plot_min <- 10^floor(
    log10(x_data_min)
  )

  x_plot_max <- 10^ceiling(
    log10(x_data_max)
  )

  x_limit <- x_plot_max * x_limit_mult

  candidate_breaks <- 10^seq(
    floor(log10(x_plot_min)),
    ceiling(log10(x_plot_max)),
    by = 1
  )

  x_breaks <- candidate_breaks[
    candidate_breaks >= x_plot_min &
      candidate_breaks <= x_plot_max
  ]

  x_breaks <- sort(
    unique(
      c(
        x_breaks,
        if (
          x_plot_min <= 1 &&
          x_plot_max >= 1
        ) {
          1
        } else {
          numeric(0)
        }
      )
    )
  )

  # -----------------------------------------------------------------------
  # 8. Formas y leyenda de significación
  # -----------------------------------------------------------------------

  shape_values <- if (isTRUE(shape.label)) {

    c(
      "p >= 0.05" = 16,
      "p < 0.05" = 17
    )

  } else {

    c(
      "p >= 0.05" = 16,
      "p < 0.05" = 16
    )
  }

  shape_guide <- if (isTRUE(shape.label)) {

    ggplot2::guide_legend(
      title = "Significación",
      override.aes = list(
        colour = "black",
        size = 3
      )
    )

  } else {

    "none"
  }

  # -----------------------------------------------------------------------
  # 9. Información del modelo
  # -----------------------------------------------------------------------

  model_caption <- NULL

  if (isTRUE(show.model.info)) {

    caption_parts <- character()

    if (
      !is.null(x$N) &&
      length(x$N) == 1L &&
      !is.na(x$N)
    ) {
      caption_parts <- c(
        caption_parts,
        paste0(
          "N = ",
          x$N
        )
      )
    }

    if (
      !is.null(x$R2_McFadden) &&
      length(x$R2_McFadden) == 1L &&
      !is.na(x$R2_McFadden)
    ) {
      caption_parts <- c(
        caption_parts,
        paste0(
          "R2 McFadden = ",
          sprintf(
            "%.3f",
            x$R2_McFadden
          )
        )
      )
    }

    if (
      !is.null(x$R2_McFadden_ajustado) &&
      length(x$R2_McFadden_ajustado) == 1L &&
      !is.na(x$R2_McFadden_ajustado)
    ) {
      caption_parts <- c(
        caption_parts,
        paste0(
          "R2 McFadden ajustado = ",
          sprintf(
            "%.3f",
            x$R2_McFadden_ajustado
          )
        )
      )
    }

    if (length(caption_parts) > 0L) {
      model_caption <- paste(
        caption_parts,
        collapse = "; "
      )
    }
  }

  # -----------------------------------------------------------------------
  # 10. Construcción del gráfico
  # -----------------------------------------------------------------------

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$`Odds Ratio`,
      y = .data$y,
      shape = .data$sig_shape
    )
  )

  if (isTRUE(show.null.band)) {

    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = null_band[1],
        xmax = null_band[2],
        ymin = -Inf,
        ymax = Inf,
        fill = "grey85",
        alpha = 0.20
      )
  }

  p <- p +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype = 2,
      colour = "grey40"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data$`CI (lower)`,
        xmax = .data$`CI (upper)`
      ),
      width = 0.15,
      linewidth = 0.5,
      colour = point_colour,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      size = point_size,
      colour = point_colour,
      na.rm = TRUE
    )

  # -----------------------------------------------------------------------
  # 11. Etiqueta anclada a cada punto (arriba y a la derecha)
  # -----------------------------------------------------------------------

  text_data <- dat |>
    dplyr::filter(
      !is.na(.data$label_html),
      nzchar(.data$label_html)
    )

  if (nrow(text_data) > 0L) {

    p <- p +
      ggtext::geom_richtext(
        data = text_data,
        ggplot2::aes(
          x = .data$`Odds Ratio` * label_dx,
          y = .data$y + label_dy,
          label = .data$label_html
        ),
        hjust = 0,
        vjust = 0,
        size = text_size,
        colour = "black",
        fill = NA,
        label.color = NA,
        label.padding = grid::unit(
          c(0, 0, 0, 0),
          "pt"
        ),
        inherit.aes = FALSE,
        na.rm = TRUE,
        show.legend = FALSE
      )
  }

  # -----------------------------------------------------------------------
  # 12. Escalas y tema
  # -----------------------------------------------------------------------

  p +
    ggplot2::scale_shape_manual(
      values = shape_values,
      drop = FALSE
    ) +
    ggplot2::scale_x_log10(
      limits = c(
        x_plot_min,
        x_limit
      ),
      breaks = x_breaks,
      labels = function(z) {
        format(
          z,
          scientific = FALSE,
          trim = TRUE
        )
      },
      expand = ggplot2::expansion(
        mult = c(0.02, 0)
      )
    ) +
    ggplot2::scale_y_continuous(
      breaks = unname(y_positions),
      labels = names(y_positions),
      expand = ggplot2::expansion(
        add = c(0.5, 1.25)
      )
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = xlab,
      y = NULL,
      shape = "Significación",
      caption = model_caption
    ) +
    ggplot2::guides(
      shape = shape_guide
    ) +
    ggplot2::coord_cartesian(
      clip = "off"
    ) +
    ggplot2::theme_bw(
      base_size = 11
    ) +
    ggplot2::theme(
      panel.grid.minor =
        ggplot2::element_blank(),

      panel.grid.major.y =
        ggplot2::element_line(
          colour = "grey90",
          linewidth = 0.3
        ),

      axis.text.y =
        ggplot2::element_text(
          hjust = 1
        ),

      legend.position = "bottom",

      plot.caption =
        ggplot2::element_text(
          hjust = 0,
          colour = "grey30"
        ),

      plot.margin =
        ggplot2::margin(
          10,
          15,
          10,
          10
        )
    )
}