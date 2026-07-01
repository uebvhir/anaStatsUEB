#' Multinomial univariate models for multiple predictors
#'
#' Fits a series of univariate multinomial logistic regression models using
#' \code{\link[nnet]{multinom}} and summarizes the results in a formatted HTML
#' table. For each predictor, relative risk ratios (RRR), confidence intervals,
#' and p-values are calculated for each outcome category relative to the
#' reference category. Significant associations can be highlighted either at
#' the row level or directly within p-value cells.
#'
#' The function returns both a formatted table suitable for reporting and the
#' underlying model results, including fitted models and coefficients.
#'
#' @param var_out Character string indicating the multinomial outcome variable.
#' @param var_comp Character vector containing the predictor variables to be
#'   evaluated in separate univariate multinomial models.
#' @param data Data frame containing the study variables.
#' @param col.background Character string specifying the background color used
#'   for table headers. Default is \code{"#993489"}.
#' @param align Character vector specifying column alignment in the output
#'   table. If \code{NULL}, all columns are centered.
#' @param pval_cut Numeric value indicating the significance threshold used
#'   for highlighting results. Default is \code{0.05}.
#' @param col.varsel Character string specifying the background color used to
#'   highlight rows corresponding to predictors with at least one significant
#'   p-value. Set to \code{NULL} to disable row highlighting. Default is
#'   \code{"#ebe0e9"}.
#' @param col.varsel_pval Optional specification for highlighting significant
#'   p-value cells. Can be:
#'   \itemize{
#'     \item \code{NULL}: no cell highlighting.
#'     \item A single color value: applies a fixed background color.
#'     \item \code{"gradient"}: applies a default color gradient according to
#'     significance.
#'     \item A vector of two colors defining a custom gradient.
#'   }
#' @param font_size Numeric value specifying the font size of the output table.
#'   Default is \code{12}.
#' @param width_lev Numeric value controlling the width of the level column.
#'   Default is \code{25}.
#' @param conf_level Numeric value indicating the confidence level used for
#'   confidence interval estimation. Default is \code{0.95}.
#' @param footnote Optional character vector containing footnotes to be added
#'   to the table.
#' @param caption Character string specifying the table caption. Default is
#'   \code{"Modelos Univariados"}.
#'
#' @return A list containing:
#' \describe{
#'   \item{table}{A formatted HTML table summarizing multinomial univariate
#'   model results.}
#'   \item{results}{A data frame containing relative risk ratios, confidence
#'   intervals, standard errors, and p-values for each predictor and outcome
#'   category.}
#'   \item{models}{A named list of fitted multinomial regression models.}
#' }
#'
#' @details
#' For each predictor in \code{var_comp}, a multinomial logistic regression
#' model is fitted using:
#'
#' \deqn{Outcome \sim Predictor}
#'
#' Relative risk ratios are obtained by exponentiating regression coefficients.
#' Confidence intervals are calculated using the normal approximation:
#'
#' \deqn{\exp(\beta \pm z_{\alpha/2} \times SE)}
#'
#' where \eqn{SE} is the standard error of the coefficient estimate.
#'
#' The resulting table groups estimates by outcome category and includes:
#' \itemize{
#'   \item Relative Risk Ratio (RRR)
#'   \item Confidence interval
#'   \item p-value
#'   \item Sample size used in each model
#' }
#'
#' Significant predictors may be highlighted according to \code{pval_cut}.
#' If both \code{col.varsel} and \code{col.varsel_pval} are specified, cell-level
#' highlighting takes precedence and row highlighting is disabled.
#'
#' @importFrom nnet multinom
#' @importFrom stats qnorm pnorm as.formula
#' @importFrom Hmisc label
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr bind_rows mutate filter select
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom purrr map
#' @importFrom kableExtra kable_styling add_header_above row_spec
#'   column_spec add_footnote cell_spec
#'
#' @author Àlex Martí Barrera, Miquel Vazquez-Santiago, Alba García Zarzoso, Miriam Mota
#'
#' @examples
#'
#' set.seed(123)
#'
#' n <- 300
#'
#' mydata <- data.frame(
#'   outcome = factor(
#'     sample(c("Bajo", "Medio", "Alto"),
#'            n,
#'            replace = TRUE,
#'            prob = c(0.3, 0.4, 0.3))
#'   ),
#'   age = round(rnorm(n, mean = 55, sd = 12)),
#'   bmi = round(rnorm(n, mean = 27, sd = 4), 1),
#'   sex = factor(sample(c("Hombre", "Mujer"), n, replace = TRUE)),
#'   smoker = factor(sample(c("No", "Si"), n, replace = TRUE, prob = c(0.7, 0.3))),
#'   hypertension = factor(sample(c("No", "Si"), n, replace = TRUE, prob = c(0.6, 0.4)))
#' )
#'
#' # Etiquetas opcionales
#' Hmisc::label(mydata$outcome) <- "Nivel de riesgo"
#' Hmisc::label(mydata$age) <- "Edad"
#' Hmisc::label(mydata$bmi) <- "Índice de masa corporal"
#' Hmisc::label(mydata$sex) <- "Sexo"
#' Hmisc::label(mydata$smoker) <- "Tabaquismo"
#' Hmisc::label(mydata$hypertension) <- "Hipertensión"
#'
#' str(mydata)
#' head(mydata)
#'
#' result <- desc_unimods_multi(
#'   var_out = "outcome",
#'   var_comp = c("age", "sex", "bmi"),
#'   data = mydata
#' )
#'
#' result$table
#'
#' head(result$results)
#'
#' names(result$models)
#'

desc_unimods_multi <- function(
    var_out,
    var_comp,
    data,
    col.background = "#993489",
    align = NULL,
    pval_cut = 0.05,
    col.varsel = "#ebe0e9",
    col.varsel_pval = NULL,
    font_size = 12,
    width_lev = 25,
    conf_level = 0.95,
    footnote = NULL,
    caption = "Modelos Univariados"
){
  require(nnet)
  # Lògica d'interacció: si s'activa color a la cel·la, es desactiva la fila
  if (!is.null(col.varsel_pval) && !is.null(col.varsel) && col.varsel == "#ebe0e9") {
    col.varsel <- NULL
  }


  # Funciones auxiliares

  get_var_label <- function(var, data){

    lab <- tryCatch(label(data[[var]]), error = function(e) NULL)
    if(is.null(lab) || length(lab) == 0){ return(var) }
    as.character(lab)
  }

  process_df <- function(df){
    df <- df |> tibble::rownames_to_column("Grupo")
    df_long <- df |>
      pivot_longer(-Grupo, names_to = "Metric", values_to = "Value") |>
      unite("col", Grupo, Metric)
    df_long |> pivot_wider(names_from = col, values_from = Value)
  }


  # Ajuste de modelos

  coef_list <- list()
  p_list    <- list()
  ici_list  <- list()
  ics_list  <- list()
  df_list   <- list()

  z_value <- qnorm(1 - (1 - conf_level) / 2)
  results_df <- data.frame()
  model_list<-list()
  n_list<-list()

  for(i in seq_along(var_comp)){
    frm <- as.formula(paste0(var_out, " ~ ", var_comp[i]))
    mod <- multinom(frm, data = data, trace = FALSE)
    model_list[[var_comp[i]]] <- mod
    n_list[[var_comp[i]]] <- nrow(model.frame(mod))
    sm <- summary(mod)

    coef_mat <- sm$coefficients[, -1, drop = FALSE]
    se_mat   <- sm$standard.errors[, -1, drop = FALSE]

    z_mat <- coef_mat / se_mat
    p_mat <- 2 * (1 - pnorm(abs(z_mat)))

    ici_mat <- coef_mat - z_value * se_mat
    ics_mat <- coef_mat + z_value * se_mat

    # Guardar resultados en formato largo


    coef_long <- as.data.frame(as.table(exp(coef_mat)))
    names(coef_long) <- c("Outcome", "Level", "Relative Risk Ratio")

    coef_long$SE <- as.vector(exp(se_mat))
    coef_long$Lower95 <- as.vector(exp(ici_mat))
    coef_long$Upper95 <- as.vector(exp(ics_mat))
    coef_long$P.value <- as.vector(p_mat)
    coef_long$Variable <- var_comp[i]

    results_df <- dplyr::bind_rows(results_df, coef_long)


    #Lo pasamos a ODDS RATIO Y INTERVALO DE CONFIANZA DE ODDS RATIO

    coef_list[[i]] <- round(exp(coef_mat), 3)
    p_list[[i]]    <- round(p_mat, 3)
    ici_list[[i]]  <- round(exp(ici_mat), 3)
    ics_list[[i]]  <- round(exp(ics_mat), 3)

    est <- as.matrix(coef_list[[i]])
    p   <- as.matrix(p_list[[i]])
    ici <- as.matrix(ici_list[[i]])
    ics <- as.matrix(ics_list[[i]])

    out <- as.data.frame(
      do.call(cbind, lapply(seq_len(ncol(est)), function(j){
        cbind(
          `Relative Risk Ratio` = paste0(est[, j], " (", ici[, j], ", ", ics[, j], ")"),
          p.value = p[, j]
        )
      }))
    )

    colnames(out) <- as.vector(rbind(paste0("Relative Risk Ratio", colnames(est)), paste0("p.value", colnames(est))))
    df_list[[i]] <- out
  }


  # Transformación resultados

  df_processed <- purrr::map(df_list, process_df)
  df_combined <- dplyr::bind_rows(df_processed)
  df_combined <- df_combined |> dplyr::mutate(Variable = var_comp, .before = 1)

  df_clean <- df_combined |>
    dplyr::mutate(across(everything(), ~ na_if(as.character(.), "NULL"))) |>
    dplyr::mutate(row_id = row_number()) |>
    tidyr::pivot_longer(
      cols = -c(Variable, row_id),
      names_to = c("Grupo", "Tipo", "Nivel"),
      names_pattern = "^(.*?)_(Relative Risk Ratio|p.value)(.*)$",
      values_to = "valor"
    ) |>
    dplyr::filter(!is.na(valor), valor != "NA") |>
    dplyr::mutate(Nivel = ifelse(Nivel == "", "base", Nivel)) |>
    tidyr::pivot_wider(
      id_cols = c(Variable, row_id, Nivel),
      names_from = c(Grupo, Tipo),
      names_glue = "{Grupo}_{Tipo}",
      values_from = "valor"
    ) |>
    dplyr::select(-row_id)


  # Limpieza niveles

  df_clean$Nivel <- ifelse(
    df_clean$Nivel == "base",
    df_clean$Nivel,
    mapply(function(var, niv){ sub(paste0("^", var), "", niv) }, df_clean$Variable, df_clean$Nivel)
  )

  idx_base <- which(df_clean$Nivel == "base")
  if(length(idx_base) > 0){
    for(i in idx_base){
      var_i <- df_clean$Variable[i]
      if(is.factor(data[[var_i]]) && nlevels(data[[var_i]]) >= 2){
        df_clean$Nivel[i] <- levels(data[[var_i]])[2]
      }
    }
  }


  # Etiquetas variables

  df_clean$N <- unlist(n_list[df_clean$Variable], use.names = FALSE)
  df_clean$N[duplicated(df_clean$Variable)] <- ""
  df_clean$Variable <- sapply(df_clean$Variable, get_var_label, data = data)

  # Formato p-values

  p_cols <- grep("_p.value$", names(df_clean), value = TRUE)
  df_clean <- df_clean |>
    dplyr::mutate(across(all_of(p_cols), ~ case_when(. == 0 ~ "<0.001", TRUE ~ as.character(.))))


  # Resaltado p-values i cerca de fileres per Variable completa

  df_tabla <- df_clean
  cols_p <- grep("value$", names(df_tabla))

  sig_vars <- c()
  for(i in cols_p){
    vals_num <- suppressWarnings(as.numeric(df_tabla[[i]]))
    idx <- which(vals_num < pval_cut | df_tabla[[i]] == "<0.001")

    if(length(idx) > 0){
      sig_vars <- union(sig_vars, df_tabla$Variable[idx])

      if(!is.null(col.varsel_pval)){
        # COMPROVACIÓ SEGURA DE TIPUS DE GRADIENT (Evita l'error de longitud)
        es_gradient <- (length(col.varsel_pval) == 1 && col.varsel_pval == "gradient") || length(col.varsel_pval) == 2

        if(es_gradient){
          # Si l'usuari passa 2 colors, els usem. Si posa "gradient", usem els pastels base.
          colors_escala <- if(length(col.varsel_pval) == 2) col.varsel_pval else c("#FFB3B3", "#B3D1FF")
          col_ramp <- colorRamp(colors_escala)

          for(row_idx in idx){
            p_str <- df_tabla[[i]][row_idx]
            p_num <- if(p_str == "<0.001") { 0 } else { suppressWarnings(as.numeric(p_str)) }
            p_num <- ifelse(is.na(p_num), pval_cut, p_num)

            val_norm <- min(max(p_num / pval_cut, 0), 1)

            rgb_matrix <- col_ramp(val_norm)
            cell_color <- rgb(rgb_matrix[1,1], rgb_matrix[1,2], rgb_matrix[1,3], maxColorValue = 255)

            df_tabla[[i]][row_idx] <- cell_spec(df_tabla[[i]][row_idx], bold = TRUE, background = cell_color)
          }
        } else if (length(col.varsel_pval) == 1) {
          # Color fix independent
          df_tabla[[i]][idx] <- cell_spec(df_tabla[[i]][idx], bold = TRUE, background = col.varsel_pval)
        }
      } else {
        df_tabla[[i]][idx] <- cell_spec(df_tabla[[i]][idx], bold = TRUE)
      }
    }
  }

  sig_rows <- which(df_clean$Variable %in% sig_vars)
  df_tabla$Variable[duplicated(df_tabla$Variable)] <- ""


  # Cabecera agrupada

  cols_data <- names(df_tabla)[!names(df_tabla) %in% c("Variable", "Nivel")]
  cols_stat <- setdiff(cols_data, "N")
  strat_names <- sub("_.*$", "", cols_stat)
  tab_header <- table(factor(strat_names, levels = unique(strat_names)))

  header_vector <- c(" " = 2, as.numeric(tab_header))
  names(header_vector)[-1] <- names(tab_header)
  if("N" %in% cols_data){
    header_vector <- c(header_vector, " " = 1)
  }

  names(df_tabla) <- c(names(df_tabla)[1:2], sub("^[^_]+_", "", names(df_tabla)[-c(1:2)]))


  # Alineación

  if(is.null(align)){ align <- rep("c", ncol(df_tabla)) }


  # Tabla final

  tabla <- df_tabla |>
    kable(
      escape = FALSE,
      row.names = FALSE,
      align = align,
      caption = caption,
      format = "html",
      table.attr = 'class="table-with-group-header"'
    )

  if(!is.null(col.varsel) && length(sig_rows) > 0){
    tabla <- tabla |> row_spec(sig_rows, background = col.varsel)
  }

  tabla <- tabla |>
    add_header_above(header_vector, background = col.background, color = "white", bold = TRUE) |>
    kable_styling(
      latex_options = c("striped", "hold_position", "repeat_header"),
      font_size = font_size,
      full_width = FALSE,
      fixed_thead = TRUE
    ) |>
    column_spec(1, bold = TRUE) |>
    row_spec(0, background = col.background, color = "white") |>
    add_footnote(footnote, escape = FALSE, notation = "symbol")

  return(
    list(
      table = tabla,
      results = results_df,
      models = model_list
    )
  )
}
