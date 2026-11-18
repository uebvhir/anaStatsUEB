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
plot_forest_multimods_multi <- function(
    x,
    title = "Modelo multinomial multivariable",
    subtitle = "Relative Risk Ratio (IC95%)",
    xlab = "RRR",
    palette = "okabe",
    point_size = 2.8,
    text_size = 3,
    show.effect.column = TRUE,
    show.p = TRUE,
    show.p.label = TRUE,
    hide.ns = TRUE,
    shape.label = TRUE,
    pval_cut = 0.05,
    text_sep = "; ",
    label_dx = 1.05,
    label_dy = 0.25,
    x_limit_mult = 1.8,
    show.null.band = TRUE,
    null_band = c(0.95, 1.05),
    show.model.info = FALSE,
    show.ref.outcome = TRUE,
    ref_outcome = NULL,
    ref_outcome_label = "Categoría de referencia") {

  # -----------------------------------------------------------------------
  # 1. Validaciones
  # -----------------------------------------------------------------------

  if (!is.list(x) || !identical(x$model_type, "multinomial")) {
    stop(
      "'x' debe ser el resultado de desc_multimod() ",
      "para un modelo multinomial.",
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
    "Outcome",
    "Variable",
    "Level",
    "Relative Risk Ratio",
    "Lower95",
    "Upper95",
    "P.value"
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
    show.model.info = show.model.info,
    show.ref.outcome = show.ref.outcome
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
    "Relative Risk Ratio",
    "Lower95",
    "Upper95",
    "P.value"
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
      Outcome = as.character(.data$Outcome),

      significant =
        !is.na(.data$P.value) &
        .data$P.value < pval_cut,

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
        .data$`Relative Risk Ratio`,
        .data$Lower95,
        .data$Upper95
      ),

      label_p = dplyr::case_when(
        is.na(.data$P.value) ~ "",

        show.p.label &
          .data$P.value >= pval_cut ~ "NS",

        TRUE ~ format.pval(
          .data$P.value,
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
    is.finite(`Relative Risk Ratio`) &
      `Relative Risk Ratio` > 0 &
      is.finite(Lower95) &
      Lower95 > 0 &
      is.finite(Upper95) &
      Upper95 > 0
  )

  if (!all(valid_x)) {
    warning(
      sum(!valid_x),
      " filas con RRR o IC no positivos o no finitos ",
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
  # 6. Paleta de colores
  # -----------------------------------------------------------------------

  outcomes <- unique(dat$Outcome)

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

  if (is.null(palette)) {

    colors <- gg_color(
      length(outcomes)
    )

  } else if (
    is.character(palette) &&
    length(palette) == 1L &&
    tolower(palette) %in% c(
      "okabe",
      "okabe_ito"
    )
  ) {

    colors <- rep_len(
      okabe_ito,
      length(outcomes)
    )

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

    colors <- rep_len(
      palette,
      length(outcomes)
    )
  }

  colors <- stats::setNames(
    colors,
    outcomes
  )

  # -----------------------------------------------------------------------
  # 7. Límites y marcas del eje X
  # -----------------------------------------------------------------------

  x_values <- c(
    dat$`Relative Risk Ratio`,
    dat$Lower95,
    dat$Upper95
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

  # El límite derecho reserva margen para que la etiqueta del punto
  # más extremo (desplazada label_dx a la derecha) no quede cortada.
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
  # 9. Categoría de referencia del outcome (subtítulo)
  # -----------------------------------------------------------------------
  # Todas las RRR de los paneles se interpretan frente a esta categoría,
  # que por definición no aparece como panel. Se obtiene del modelo
  # ajustado guardado en 'x$model'; si no es posible, del conjunto de
  # categorías ausentes en 'x$results$Outcome'.

  get_ref_outcome <- function(x, outcomes) {

    model <- x$model

    if (
      !is.null(model) &&
      !is.null(model$lev) &&
      length(model$lev) > 0L
    ) {
      return(as.character(model$lev)[1])
    }

    if (!is.null(model)) {

      model_frame <- tryCatch(
        stats::model.frame(model),
        error = function(e) NULL
      )

      if (
        !is.null(model_frame) &&
        ncol(model_frame) > 0L
      ) {

        response <- model_frame[[1]]

        response_levels <- if (is.factor(response)) {
          levels(droplevels(response))
        } else {
          as.character(sort(unique(stats::na.omit(response))))
        }

        missing_level <- setdiff(response_levels, outcomes)

        if (length(missing_level) == 1L) {
          return(missing_level)
        }

        if (length(response_levels) > 0L) {
          return(response_levels[1])
        }
      }
    }

    NULL
  }

  if (isTRUE(show.ref.outcome)) {

    if (is.null(ref_outcome)) {
      ref_outcome <- get_ref_outcome(x, outcomes)
    }

    ref_outcome <- as.character(ref_outcome)[1]

    if (
      !is.na(ref_outcome) &&
      length(ref_outcome) == 1L &&
      nzchar(ref_outcome)
    ) {

      ref_text <- paste0(ref_outcome_label, ": ", ref_outcome)

      subtitle <- if (is.null(subtitle) || !nzchar(subtitle)) {
        ref_text
      } else {
        paste0(subtitle, " | ", ref_text)
      }
    }
  }

  # -----------------------------------------------------------------------
  # 10. Información del modelo
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
  # 11. Construcción del gráfico
  # -----------------------------------------------------------------------

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = .data$`Relative Risk Ratio`,
      y = .data$y,
      colour = .data$Outcome,
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
        xmin = .data$Lower95,
        xmax = .data$Upper95
      ),
      width = 0.15,
      orientation = "y",
      linewidth = 0.5,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      size = point_size,
      na.rm = TRUE
    )

  # -----------------------------------------------------------------------
  # 12. Etiqueta anclada a cada punto (arriba y a la derecha)
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
          x = .data$`Relative Risk Ratio` * label_dx,
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
  # 13. Facetas, escalas y tema
  # -----------------------------------------------------------------------

  p +
    ggplot2::facet_grid(
      cols = ggplot2::vars(Outcome),
      scales = "fixed",
      space = "fixed"
    ) +
    ggplot2::scale_colour_manual(
      values = colors,
      drop = FALSE
    ) +
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
      colour = "Outcome",
      shape = "Significación",
      caption = model_caption
    ) +
    ggplot2::guides(
      colour = "none",
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

      strip.background =
        ggplot2::element_rect(
          fill = "grey85",
          colour = "grey40"
        ),

      strip.text =
        ggplot2::element_text(
          face = "bold"
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