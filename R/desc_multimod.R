
#' Resumen de modelos logísticos multivariables
#'
#' @description Resume un modelo logístico binomial ajustado con \code{stats::glm()} o
#' un modelo logístico multinomial ajustado con \code{nnet::multinom()}.
#' Devuelve resultados numéricos en formato largo, una tabla HTML con un
#' formato equivalente a \code{desc_unimods()} o
#' \code{desc_unimods_multi()}, las etiquetas de variables y niveles, el
#' número de observaciones y los pseudo-R2 de McFadden.
#'
#' En el caso multinomial, la tabla pivota la categoría del outcome para
#' mostrar, bajo una cabecera por categoría, las columnas
#' \\code{Relative Risk Ratio} y \\code{p.value}. El objeto \\code{results} se
#' conserva en formato largo para poder utilizarlo directamente en un
#' forest plot facetado.
#'
#' @param model Modelo ajustado de clase \\code{multinom} o un modelo
#'   \\code{glm} con familia binomial.
#' @param align Vector de alineaciones de columnas pasado a
#'   \\code{knitr::kable()}. Si es \\code{NULL}, todas las columnas se
#'   centran.
#' @param pval_cut Punto de corte para identificar asociaciones
#'   estadísticamente significativas. Por defecto, 0.05.
#' @param col.varsel Color de fondo aplicado a todas las filas de una
#'   variable que presenta al menos un p-valor inferior a
#'   \\code{pval_cut}. Si es \\code{NULL}, no se colorean filas completas.
#' @param col.varsel_pval Control del resaltado de las celdas de p-valor
#'   significativas. Puede ser \\code{NULL}, un color, \\code{"gradient"}
#'   o un vector de dos colores. Si se usa junto con el valor por defecto de
#'   \\code{col.varsel}, se desactiva el coloreado de fila completa.
#' @param font_size Tamaño de fuente de la tabla HTML.
#' @param conf_level Nivel de confianza. Por defecto, 0.95.
#' @param footnote Nota al pie opcional. A la nota indicada se añade el
#'   número de observaciones y los pseudo-R2 de McFadden.
#' @param caption Título opcional de la tabla. Si es \\code{NULL}, se genera
#'   automáticamente según el tipo de modelo.
#' @param col.background Color de fondo de la cabecera de la tabla.
#'
#' @author
#' Alba García Zarzoso \email{alba.garcia.zarzoso@vhir.org}
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#' 
#' @return Lista con los elementos \\code{table}, \\code{table_data},
#'   \\code{results}, \\code{model}, \\code{model_type}, \\code{N},
#'   \\code{R2_McFadden}, \\code{R2_McFadden_ajustado},
#'   \\code{lbls_vars} y \\code{lbls_levels}.
#'
#' @export
desc_multimod <- function(
    model,
    col.background = "#993489",
    align = NULL,
    pval_cut = 0.05,
    col.varsel = "#ebe0e9",
    col.varsel_pval = NULL,
    font_size = 12,
    conf_level = 0.95,
    footnote = NULL,
    caption = NULL
) {

  is_multinom <- inherits(model, "multinom")
  is_binomial <- inherits(model, "glm") &&
    identical(model$family$family, "binomial")

  if (!is_multinom && !is_binomial) {
    stop(
      "'model' debe ser un objeto nnet::multinom o stats::glm binomial.",
      call. = FALSE
    )
  }

  if (!is.numeric(pval_cut) || length(pval_cut) != 1L ||
      is.na(pval_cut) || pval_cut <= 0 || pval_cut >= 1) {
    stop("'pval_cut' debe ser un número entre 0 y 1.", call. = FALSE)
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' debe ser un número entre 0 y 1.", call. = FALSE)
  }

  if (!is.null(col.varsel_pval) &&
      !is.null(col.varsel) &&
      identical(col.varsel, "#ebe0e9")) {
    col.varsel <- NULL
  }

  model_type <- if (is_multinom) "multinomial" else "binomial"
  model_frame <- stats::model.frame(model)
  predictors <- attr(stats::terms(model), "term.labels")

  if (length(predictors) == 0L) {
    stop("El modelo no contiene variables explicativas.", call. = FALSE)
  }

  if (any(grepl(":|\\*|\\^", predictors))) {
    warning(
      "La separación automática entre Variable y Level está diseñada ",
      "para modelos aditivos sin interacciones."
    )
  }

  get_var_label <- function(var) {
    lab <- tryCatch(Hmisc::label(model_frame[[var]]), error = function(e) NULL)
    if (is.null(lab) || length(lab) == 0L || identical(lab, "")) {
      return(var)
    }
    as.character(lab)[1]
  }

  lbls_vars <- stats::setNames(
    vapply(predictors, get_var_label, character(1)),
    predictors
  )

  lbls_levels <- lapply(predictors, function(var) {
    if (!is.factor(model_frame[[var]])) return(NULL)
    lv <- levels(droplevels(model_frame[[var]]))
    stats::setNames(lv, lv)
  })
  names(lbls_levels) <- predictors

  tidy_results <- broom::tidy(
    model,
    exponentiate = TRUE,
    conf.int = TRUE,
    conf.level = conf_level
  ) |>
    dplyr::filter(.data$term != "(Intercept)")

### Gestión de 'Intercept' y 'levels' de variables.
  .parse_model_terms <- function(term, predictors, model) {

    clean_term <- gsub("`", "", as.character(term), fixed = TRUE)
    predictors <- gsub("`", "", predictors, fixed = TRUE)
    predictors <- predictors[order(nchar(predictors), decreasing = TRUE)]

    xlevels <- model$xlevels

    variable <- vapply(clean_term, function(tt) {
      hit <- predictors[startsWith(tt, predictors)]
      if (length(hit) == 0L) tt else hit[1]
    }, character(1))

    level <- mapply(
      FUN = function(tt, vv) {
        if (!vv %in% names(xlevels)) return(NA_character_)
        out <- sub(paste0("^", vv), "", tt)
        if (identical(out, "")) NA_character_ else out
      },
      tt = clean_term,
      vv = variable,
      USE.NAMES = FALSE
    )

    tibble::tibble(
      Variable = variable,
      Level = level
    )
  }


  parsed_terms <- .parse_model_terms(
    term = tidy_results$term,
    predictors = predictors,
    model = model
  )

  tidy_results <- dplyr::bind_cols(tidy_results, parsed_terms)

  if (is_multinom) {
    results <- tidy_results |>
      dplyr::transmute(
        Outcome = as.character(.data$y.level),
        Variable = as.character(.data$Variable),
        Level = as.character(.data$Level),
        `Relative Risk Ratio` = as.numeric(.data$estimate),
        Lower95 = as.numeric(.data$conf.low),
        Upper95 = as.numeric(.data$conf.high),
        P.value = as.numeric(.data$p.value)
      )
  } else {
    results <- tidy_results |>
      dplyr::transmute(
        Variable = as.character(.data$Variable),
        Level = as.character(.data$Level),
        `Odds Ratio` = as.numeric(.data$estimate),
        `CI (lower)` = as.numeric(.data$conf.low),
        `CI (upper)` = as.numeric(.data$conf.high),
        `Pr(>|z|)` = as.numeric(.data$p.value)
      )
  }

  n_model <- stats::nobs(model)
  loglik_full_obj <- stats::logLik(model)
  ll_full <- as.numeric(loglik_full_obj)
  k_full <- attr(loglik_full_obj, "df")

  response_name <- all.vars(stats::formula(model))[1]
  null_formula <- stats::reformulate("1", response = response_name)

  null_model <- if (is_multinom) {
    nnet::multinom(
      formula = null_formula,
      data = model_frame,
      trace = FALSE,
      na.action = stats::na.fail
    )
  } else {
    stats::glm(
      formula = null_formula,
      data = model_frame,
      family = stats::binomial(),
      na.action = stats::na.fail
    )
  }

  ll_null <- as.numeric(stats::logLik(null_model))
  r2_mcfadden <- 1 - ll_full / ll_null
  r2_mcfadden_adj <- 1 - (ll_full - k_full) / ll_null

  variable_order <- predictors
  variable_labels_order <- unname(lbls_vars[variable_order])

  format_p <- function(x) {
    dplyr::case_when(
      is.na(x) ~ "",
      x < 0.001 ~ "<0.001",
      TRUE ~ format.pval(x, digits = 3, eps = 0.001)
    )
  }

  if (is_multinom) {
    table_long <- results |>
      dplyr::mutate(
        Variable_code = .data$Variable,
        Variable = unname(lbls_vars[.data$Variable]),
        Variable = dplyr::coalesce(.data$Variable, .data$Variable_code),
        Nivel = dplyr::coalesce(.data$Level, ""),
        `Relative Risk Ratio` = sprintf(
          "%.3f (%.3f, %.3f)",
          .data$`Relative Risk Ratio`,
          .data$Lower95,
          .data$Upper95
        ),
        `p.value` = format_p(.data$P.value),
        row_key = paste(.data$Variable_code, .data$Nivel, sep = "\r")
      )

    outcome_order <- unique(table_long$Outcome)

    table_data <- table_long |>
      dplyr::select(
        .data$row_key,
        .data$Variable_code,
        .data$Variable,
        .data$Nivel,
        .data$Outcome,
        .data$`Relative Risk Ratio`,
        .data$`p.value`
      ) |>
      tidyr::pivot_wider(
        id_cols = c(
          .data$row_key,
          .data$Variable_code,
          .data$Variable,
          .data$Nivel
        ),
        names_from = .data$Outcome,
        values_from = c(.data$`Relative Risk Ratio`, .data$`p.value`),
        names_glue = "{Outcome}__{.value}",
        names_vary = "slowest"
      ) |>
      dplyr::mutate(
        variable_order = match(.data$Variable_code, variable_order)
      ) |>
      dplyr::arrange(.data$variable_order) |>
      dplyr::select(-.data$row_key, -.data$Variable_code, -.data$variable_order)

    desired_cols <- unlist(
      lapply(outcome_order, function(outcome_i) {
        c(
          paste0(outcome_i, "__Relative Risk Ratio"),
          paste0(outcome_i, "__p.value")
        )
      }),
      use.names = FALSE
    )

    table_data <- table_data |>
      dplyr::select(
        .data$Variable,
        .data$Nivel,
        dplyr::all_of(desired_cols)
      )

    n_by_variable <- table_long |>
      dplyr::distinct(.data$Variable_code) |>
      dplyr::mutate(N = n_model)

    table_data$N <- ""
    first_rows <- !duplicated(table_data$Variable)
    table_data$N[first_rows] <- as.character(n_model)

    p_col_indices <- grep("__p.value$", names(table_data))
    header_vector <- c(" " = 2)
    for (outcome_i in outcome_order) {
      header_vector <- c(header_vector, stats::setNames(2, outcome_i))
    }
    header_vector <- c(header_vector, " " = 1)

    display_names <- names(table_data)
    display_names <- sub("^[^_]+__", "", display_names)
    names(table_data) <- display_names

  } else {
    table_data <- results |>
      dplyr::mutate(
        Variable_code = .data$Variable,
        Variable = unname(lbls_vars[.data$Variable]),
        Variable = dplyr::coalesce(.data$Variable, .data$Variable_code),
        Nivel = dplyr::coalesce(.data$Level, ""),
        `OR (IC95%)` = sprintf(
          "%.3f (%.3f, %.3f)",
          .data$`Odds Ratio`,
          .data$`CI (lower)`,
          .data$`CI (upper)`
        ),
        `p.value` = format_p(.data$`Pr(>|z|)`),
        variable_order = match(.data$Variable_code, variable_order)
      ) |>
      dplyr::arrange(.data$variable_order) |>
      dplyr::select(
        .data$Variable,
        .data$Nivel,
        .data$`OR (IC95%)`,
        .data$`p.value`
      )

    table_data$N <- ""
    first_rows <- !duplicated(table_data$Variable)
    table_data$N[first_rows] <- as.character(n_model)

    p_col_indices <- which(names(table_data) == "p.value")
    header_vector <- NULL
  }

  sig_variables <- character(0)

  for (j in p_col_indices) {
    p_numeric <- suppressWarnings(
      as.numeric(sub("^<", "", table_data[[j]]))
    )
    significant_rows <- which(
      table_data[[j]] == "<0.001" |
        (!is.na(p_numeric) & p_numeric < pval_cut)
    )

    if (length(significant_rows) > 0L) {
      sig_variables <- union(
        sig_variables,
        table_data$Variable[significant_rows]
      )

      if (!is.null(col.varsel_pval)) {
        is_gradient <-
          (length(col.varsel_pval) == 1L &&
             identical(col.varsel_pval, "gradient")) ||
          length(col.varsel_pval) == 2L

        if (is_gradient) {
          gradient_colors <- if (length(col.varsel_pval) == 2L) {
            col.varsel_pval
          } else {
            c("#FFB3B3", "#B3D1FF")
          }

          color_ramp <- grDevices::colorRamp(gradient_colors)

          for (row_i in significant_rows) {
            p_i <- if (table_data[[j]][row_i] == "<0.001") {
              0
            } else {
              suppressWarnings(as.numeric(table_data[[j]][row_i]))
            }

            if (is.na(p_i)) p_i <- pval_cut
            normalized <- min(max(p_i / pval_cut, 0), 1)
            rgb_value <- color_ramp(normalized)
            cell_color <- grDevices::rgb(
              rgb_value[1, 1],
              rgb_value[1, 2],
              rgb_value[1, 3],
              maxColorValue = 255
            )

            table_data[[j]][row_i] <- kableExtra::cell_spec(
              table_data[[j]][row_i],
              bold = TRUE,
              background = cell_color
            )
          }
        } else if (length(col.varsel_pval) == 1L) {
          table_data[[j]][significant_rows] <- kableExtra::cell_spec(
            table_data[[j]][significant_rows],
            bold = TRUE,
            background = col.varsel_pval
          )
        }
      } else {
        table_data[[j]][significant_rows] <- kableExtra::cell_spec(
          table_data[[j]][significant_rows],
          bold = TRUE
        )
      }
    }
  }

  significant_table_rows <- which(table_data$Variable %in% sig_variables)
  table_data$Variable[duplicated(table_data$Variable)] <- ""

  if (is.null(align)) {
    align <- rep("c", ncol(table_data))
  } else if (length(align) == 1L) {
    align <- rep(align, ncol(table_data))
  } else if (length(align) != ncol(table_data)) {
    stop(
      "'align' debe tener longitud 1 o coincidir con el número de columnas de la tabla.",
      call. = FALSE
    )
  }

  if (is.null(caption)) {
    caption <- if (is_multinom) {
      "Multivariable multinomial regression."
    } else {
      "Multivariable binomial regression."
    }
  }

  model_footnote <- paste0(
    "R2 de McFadden = ", sprintf("%.3f", r2_mcfadden),
    "; R2 de McFadden ajustado = ", sprintf("%.3f", r2_mcfadden_adj),
    ".")

  final_footnote <- if (is.null(footnote) || length(footnote) == 0L ||
                        all(is.na(footnote)) || identical(footnote, "")) {
    model_footnote
  } else {
    c(footnote, model_footnote)
  }

  table_html <- knitr::kable(
    table_data,
    escape = FALSE,
    row.names = FALSE,
    align = align,
    caption = caption,
    format = "html",
    table.attr = "class=\"table-with-group-header\""
  )

  if (!is.null(col.varsel) && length(significant_table_rows) > 0L) {
    table_html <- table_html |>
      kableExtra::row_spec(
        significant_table_rows,
        background = col.varsel
      )
  }

  if (is_multinom) {
    table_html <- table_html |>
      kableExtra::add_header_above(
        header_vector,
        background = col.background,
        color = "white",
        bold = TRUE
      )
  }

  table_html <- table_html |>
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position", "repeat_header"),
      font_size = font_size,
      full_width = FALSE,
      fixed_thead = TRUE
    ) |>
    kableExtra::column_spec(1, bold = TRUE) |>
    kableExtra::row_spec(
      0,
      background = col.background,
      color = "white"
    ) |>
    kableExtra::add_footnote(
      final_footnote,
      escape = FALSE,
      notation = "symbol"
    )

  list(
    table = table_html,
    table_data = table_data,
    results = results,
    model = model,
    model_type = model_type,
    N = n_model,
    logLik = ll_full,
    logLik_null = ll_null,
    R2_McFadden = unname(r2_mcfadden),
    R2_McFadden_ajustado = unname(r2_mcfadden_adj),
    lbls_vars = lbls_vars,
    lbls_levels = lbls_levels
  )
}


# #' @export
# multivariable_labels <- function(model, predictors) {

#   mf <- stats::model.frame(model)

#   get_label <- function(v) {
#     lab <- tryCatch(Hmisc::label(mf[[v]]), error = function(e) NULL)
#     if (is.null(lab) || length(lab) == 0L || identical(lab, "")) v else as.character(lab)[1]
#   }

#   lbls_vars <- stats::setNames(
#     vapply(predictors, get_label, character(1)),
#     predictors
#   )

#   lbls_levels <- lapply(predictors, function(v) {
#     if (!is.factor(mf[[v]])) return(NULL)
#     lev <- levels(mf[[v]])
#     stats::setNames(lev, lev)
#   })
#   names(lbls_levels) <- predictors

#   list(vars = lbls_vars, levels = lbls_levels)
# }