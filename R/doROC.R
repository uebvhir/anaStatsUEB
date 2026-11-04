#' ROC analysis and optimal cut-off estimation
#'
#' Performs ROC curve analysis for a continuous marker or for the predicted
#' probabilities from a logistic regression model. The function estimates
#' optimal cut-off points according to different criteria, computes diagnostic
#' accuracy measures and optionally displays the ROC curve, diagnostic tables
#' and boxplots.
#'
#' Bootstrap confidence intervals for the AUC can also be estimated using
#' stratified resampling. When a logistic regression model is specified, the
#' model is refitted in each bootstrap sample.
#'
#' @param frml Formula specifying the logistic regression model. Required when
#'   `modGLM = TRUE`.
#' @param x Character string specifying the predictor variable used for the
#'   ROC analysis. When `modGLM = TRUE`, this variable must be included in
#'   `frml`.
#' @param group Character string specifying the binary outcome variable.
#' @param dat Data frame containing the variables used in the analysis.
#' @param tag.healthy Optional character string specifying the label of the
#'   healthy/control group in `group`. If `NULL`, the function uses the
#'   default group identification.
#' @param title Optional character string specifying the title displayed in
#'   the plots and diagnostic tables.
#' @param model.name Optional character string specifying the model name
#'   displayed in the results. If `NULL`, the formula is used as the model
#'   name.
#' @param modGLM Logical. If `TRUE`, a logistic regression model is fitted
#'   using `frml`, and its predicted probabilities are used as the marker
#'   for the ROC analysis. If `FALSE`, the original predictor specified by
#'   `x` is used directly.
#' @param method.cutoff Character string specifying the criterion used to
#'   determine the optimal cut-off point. See
#'   `OptimalCutpoints::optimal.cutpoints()` for the available criteria.
#'   Defaults to `"Youden"`.
#' @param sens_target Numeric value specifying the target sensitivity when
#'   using a sensitivity-based cut-off criterion. Defaults to `0.90`.
#' @param spec_target Numeric value specifying the target specificity when
#'   using a specificity-based cut-off criterion. Defaults to `NULL`.
#' @param n.boot Optional integer specifying the number of bootstrap
#'   resamples used to estimate the confidence interval for the AUC.
#'   If `NULL`, bootstrap estimation is not performed.
#' @param doPlot Logical. If `TRUE`, the ROC curve is displayed.
#' @param show.boxplot Logical. If `TRUE`, a boxplot of the original predictor
#'   according to the outcome group is displayed when applicable.
#' @param show.tables Logical. If `TRUE`, diagnostic tables for the optimal
#'   cut-off points are generated.
#' @param cex.main Numeric value specifying the relative size of plot titles.
#' @param cex.text Numeric value specifying the relative size of plot text.
#' @param cex.sub Numeric value specifying the relative size of plot subtitles.
#' @param cex Numeric value specifying the relative size of additional plot
#'   elements.
#' @param show.lg Logical. If `TRUE`, the legend is displayed in the ROC plot.
#' @param show.cascon Logical. If `TRUE`, the number of cases and controls is
#'   displayed in the ROC plot.
#' @param xtab Logical. If `TRUE`, an additional diagnostic table is generated
#'   using `kable_ueb()`.
#' @param direction Character vector specifying the direction of the ROC
#'   analysis. Defaults to `c("<", ">")`.
#' @param ... Additional arguments passed to
#'   `OptimalCutpoints::optimal.cutpoints()`.
#'
#' @importFrom OptimalCutpoints optimal.cutpoints
#' @import BMisc
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{mod}{Fitted logistic regression model, when `modGLM = TRUE`.}
#'   \item{dat}{Analysis data frame, including predicted probabilities and
#'   classifications when applicable.}
#'   \item{res_sum}{Summary results returned by
#'   `OptimalCutpoints::optimal.cutpoints()`.}
#'   \item{cutoff.probability}{Optimal cut-off point(s) on the predicted
#'   probability scale, when applicable.}
#'   \item{cutoff.prediction}{Optimal cut-off point(s) on the prediction
#'   scale, when applicable.}
#'   \item{cutoff.variable}{Optimal cut-off point(s) expressed on the original
#'   predictor scale, when this conversion is possible.}
#'   \item{cutoff.type}{Character string indicating the scale of the reported
#'   cut-off point(s): original predictor scale or predicted probability
#'   scale.}
#'   \item{classifications}{Classification results for each optimal cut-off
#'   point.}
#'   \item{table}{Confusion matrices for each optimal cut-off point.}
#'   \item{diagnostics}{Diagnostic measures across all evaluated cut-off
#'   points.}
#'   \item{diagnostics_ci}{Diagnostic measures and their confidence intervals
#'   across all evaluated cut-off points.}
#'   \item{Criterion}{Optimal value(s) of the selected criterion specified by
#'   `method.cutoff`.}
#'   \item{auc}{AUC estimate and its confidence interval.}
#'   \item{tag.healthy}{Label identifying the healthy/control group.}
#'   \item{kable_tables}{Diagnostic tables for the optimal cut-off point(s), when
#'   `show.tables = TRUE`.}
#'   \item{boxplot}{Boxplot of the original predictor, when generated.}
#'   \item{bootstrap}{Bootstrap results, when `n.boot` is specified. Includes
#'   the number of valid and failed resamples, bootstrap AUC estimates,
#'   standard error and percentile confidence interval.}
#'   \item{metrics}{Data frame containing one row per optimal cut-off point,
#'   with the model name, number of observations, AUC and confidence
#'   intervals, bootstrap confidence interval when available, selected
#'   criterion value, sensitivity, specificity, positive and negative
#'   predictive values, positive and negative diagnostic likelihood ratios,
#'   accuracy and the corresponding cut-off point and its scale.}
#'   \item{xtab}{Additional diagnostic table generated when `xtab = TRUE`.}
#' }
#'
#' @details
#' When `modGLM = FALSE`, the ROC analysis is performed directly using the
#' predictor specified by `x`. When `modGLM = TRUE`, a logistic regression
#' model is fitted and the predicted probabilities are used as the marker.
#'
#' For univariable logistic regression models with a continuous predictor,
#' the probability cut-off can be transformed back to the original predictor
#' scale. For multivariable models, or when this transformation is not
#' possible, the cut-off is reported on the predicted probability scale.
#'
#' The `diagnostics` and `diagnostics_ci` elements contain diagnostic
#' measures across all evaluated cut-off points. In contrast, `metrics`
#' provides a compact summary restricted to the optimal cut-off point(s)
#' selected according to `method.cutoff`.
#'
#' Accuracy confidence intervals in `metrics` are calculated using the
#' exact binomial method based on the corresponding confusion matrix.
#'
#' When bootstrap estimation is requested, stratified resampling is used
#' to preserve the number of cases and controls in each resample. For
#' logistic regression models, the model is refitted in each bootstrap
#' sample. The resulting percentile confidence interval describes the
#' bootstrap distribution of the AUC; it does not correct for optimism
#' or replace external validation.
#'
#' Multiple optimal cut-off points may be returned when several values
#' achieve the same optimal criterion.
#'
#' @author Marcos Esteve, Miriam Mota
#'
#' @seealso \code{\link[OptimalCutpoints]{optimal.cutpoints}}
#'
#' @examples
#' \dontrun{
#' # ROC analysis using a continuous predictor
#' res <- doROC(
#'   x = "marker",
#'   group = "outcome",
#'   dat = data,
#'   modGLM = FALSE
#' )
#'
#' # ROC analysis using predicted probabilities from a logistic model
#' res <- doROC(
#'   frml = outcome ~ marker + age,
#'   x = "marker",
#'   group = "outcome",
#'   dat = data,
#'   modGLM = TRUE,
#'   n.boot = 1000
#' )
#' }
#'
#' @export
doROC <- function(frml, x, group, dat,
                  tag.healthy = NULL,
                  title = NULL,
                  model.name = NULL,
                  modGLM = NULL,
                  method.cutoff = "Youden",
                  sens_target = 0.90,
                  spec_target = NULL,
                  n.boot = NULL,
                  doPlot = TRUE,
                  show.boxplot = FALSE,
                  show.tables = TRUE,
                  cex.main = 2,
                  cex.text = 1.4,
                  cex.sub = 0.9,
                  cex = 0.5,
                  show.lg = TRUE,
                  show.cascon = TRUE,
                  xtab = FALSE,
                  direction = c("<", ">"), ...) {

  ## Comprobaciones, warnings y errores
  if (exists(deparse(substitute(show.ci))) && !missing(show.ci)) message("\n UEBmessage: Argument 'show.ci' is deprecated \n")
  if (exists(deparse(substitute(validation))) && !missing(validation)) message("\n UEBmessage: Argument 'validation' is deprecated \n")
  if (exists(deparse(substitute(test_y))) && !missing(test_y)) message("\n UEBmessage: Arguments 'test' and 'test_y' are deprecated \n")
  if (exists(deparse(substitute(col.thres))) && !missing(col.thres)) message("\n UEBmessage: Argument 'col.thres' is deprecated \n")
  if (exists(deparse(substitute(col.ic))) && !missing(col.ic)) message("\n UEBmessage: Argument 'col.ic' is deprecated \n")
  if (exists(deparse(substitute(x.axes))) && !missing(x.axes)) message("\n UEBmessage: Argument 'x.axes' is deprecated \n")
  if (exists(deparse(substitute(show.thr))) && !missing(show.thr)) message("\n UEBmessage: Argument 'show.thr' is deprecated \n")

  if (is.null(modGLM)) stop("Es necesario indicar TRUE o FALSE para el parametro modGLM.")
  if (!is.logical(modGLM) || length(modGLM) != 1 || is.na(modGLM)) {
    stop("'modGLM' must be TRUE or FALSE.", call. = FALSE)
  }
  if ((missing(x) | missing(group)) & missing(frml)) {
    stop("'x' and 'group' argument required, or 'frml' argument required", call. = FALSE)
  }
  if (!is.null(n.boot)) {
    if (length(n.boot) != 1 || !is.numeric(n.boot) || is.na(n.boot) ||
        n.boot < 1 || n.boot != as.integer(n.boot)) {
      stop("'n.boot' must be NULL or a positive integer.", call. = FALSE)
    }
    n.boot <- as.integer(n.boot)
  }

  if (!is.logical(show.boxplot) || length(show.boxplot) != 1 || is.na(show.boxplot)) {
    stop("'show.boxplot' must be TRUE or FALSE.", call. = FALSE)
  }

  direction <- match.arg(direction)

  ## Asignación de variables
  if (!missing(frml)) {
    if (is.character(frml)) {
      frml <- stats::as.formula(frml)
    } else if (!inherits(frml, "formula")) {
      stop("'frml' must be a formula or a character string representing a formula.", call. = FALSE)
    }
  }
  if (missing(x)) x <- all.vars(delete.response(terms(frml)))
  if (missing(frml)) frml <- as.formula(paste(group, "~", paste(x, collapse = " + ")))
  if (missing(group)) group <- all.vars(frml)[1]
  if (is.null(title)) title <- paste(group, "-", paste(x, collapse = "+"))
  if (is.null(model.name)) model.name <- paste(deparse(frml), collapse = "")

  original_predictors <- if (modGLM) {
    all.vars(delete.response(terms(frml)))
  } else {
    x
  }

  if (!group %in% names(dat)) stop("Outcome variable not found in dat.", call. = FALSE)
  if (!all(original_predictors %in% names(dat))) {
    stop("One or more predictor variables were not found in dat.", call. = FALSE)
  }

  if (!is.factor(dat[[group]])) dat[[group]] <- factor(dat[[group]])
  if (is.null(tag.healthy)) tag.healthy <- levels(dat[[group]])[1]
  if (!tag.healthy %in% levels(dat[[group]])) {
    stop("'tag.healthy' must be a valid level of the outcome variable.", call. = FALSE)
  }

  dat[[group]] <- relevel(dat[[group]], ref = tag.healthy)
  positive.class <- setdiff(levels(dat[[group]]), tag.healthy)

  if (length(positive.class) != 1) {
    stop("The outcome variable must have exactly two categories.", call. = FALSE)
  }
  if (!modGLM && length(x) != 1) {
    stop("For a ROC analysis without GLM, only one predictor is allowed.", call. = FALSE)
  }

  results <- list()

  ## Ajuste del modelo logístico
  if (modGLM) {
    mod <- glm(frml, data = dat, family = binomial, na.action = "na.omit")
    results$mod <- mod

    pred <- predict(mod, type = "response")
    dat$pred <- NA_real_
    dat[names(pred), "pred"] <- pred
    x <- "pred"
  }

  results$dat <- dat

  ## Parámetros del método de selección del punto de corte
  control_args <- switch(
    method.cutoff,
    "MinValueSe" = control.cutpoints(valueSe = sens_target),
    "ValueSe" = control.cutpoints(valueSe = sens_target),
    "MinValueSp" = control.cutpoints(valueSp = spec_target),
    "ValueSp" = control.cutpoints(valueSp = spec_target),
    "MinValueSpSe" = control.cutpoints(valueSe = sens_target, valueSp = spec_target),
    control.cutpoints()
  )

  ## Cálculo de la curva ROC y de los puntos de corte
  dat <- as.data.frame(dat)

  clasRes <- optimal.cutpoints(
    X = x,
    status = group,
    methods = method.cutoff,
    data = dat,
    tag.healthy = tag.healthy,
    ci.fit = TRUE,
    direction = direction,
    control = control_args
  )

  results$res_sum <- summary(clasRes)

  cutoffs <- clasRes[[method.cutoff]]$Global$optimal.cutoff$cutoff
  cutoffs <- as.numeric(cutoffs)
  n_cutoffs <- length(cutoffs)

  if (n_cutoffs == 0 || all(!is.finite(cutoffs))) {
    stop("No valid optimal cut-off was returned by optimal.cutpoints().", call. = FALSE)
  }

  ## Gráfico ROC
  if (doPlot) {
    opt <- par(cex = cex, cex.main = cex.main, ...)
    plot(clasRes, which = 1, legend = show.lg, ylim = c(0, 1))
    par(opt)
    mtext(title, side = 3, cex = cex.sub)

    if (show.cascon) {
      text(.85, .25,
           paste0("controls: ", clasRes[[method.cutoff]]$Global$measures.acc$n$h,
                  "\n cases: ", clasRes[[method.cutoff]]$Global$measures.acc$n$d),
           cex = cex.text)
    }
  }

  ## Tabla resumen de optimal.cutpoints
  if (xtab) {
    results$xtab <- kable_ueb(
      results$res_sum$p.table$Global[[method.cutoff]],
      booktabs = TRUE,
      caption = paste(title, ". AUC ", results$res_sum$p.table$Global$AUC_CI),
      longtable = TRUE,
      escape = FALSE
    )
  }

  ## Puntos de corte
  if (modGLM) {

    results$cutoff.probability <- cutoffs
    results$cutoff.prediction <- cutoffs
    results$cutoff.variable <- rep(NA_real_, n_cutoffs)

    ## Conversión a la escala original si el GLM es univariable
    term_labels <- attr(terms(frml), "term.labels")

    if (length(original_predictors) == 1 &&
        length(term_labels) == 1 &&
        identical(term_labels, original_predictors) &&
        is.numeric(dat[[original_predictors]]) &&
        all(is.finite(cutoffs)) &&
        all(cutoffs > 0 & cutoffs < 1)) {

      coefs <- coef(mod)
      intercept <- unname(coefs["(Intercept)"])
      slope <- unname(coefs[original_predictors])

      if (length(intercept) == 1 && length(slope) == 1 &&
          is.finite(intercept) && is.finite(slope) && slope != 0) {
        results$cutoff.variable <- (qlogis(cutoffs) - intercept) / slope
      }
    }

    results$cutoff.type <- if (all(is.finite(results$cutoff.variable))) {
      "Original predictor scale"
    } else {
      "Predicted probability scale"
    }

  } else {

    results$cutoff.variable <- cutoffs
    results$cutoff.probability <- NULL
    results$cutoff.prediction <- NULL
    results$cutoff.type <- "Original predictor scale"
  }

  ## Clasificación y matrices de confusión para cada punto de corte
  results$classifications <- vector("list", n_cutoffs)
  results$table <- vector("list", n_cutoffs)
  names(results$classifications) <- names(results$table) <- paste0("cutoff_", seq_len(n_cutoffs))

  for (i in seq_len(n_cutoffs)) {

    if (identical(direction, "<")) {
      predicted <- ifelse(dat[[x]] >= cutoffs[i], positive.class, tag.healthy)
    } else {
      predicted <- ifelse(dat[[x]] >= cutoffs[i], tag.healthy, positive.class)
    }

    predicted <- factor(predicted, levels = c(tag.healthy, positive.class))

    results$classifications[[i]] <- predicted

    results$table[[i]] <- table(
      Group = results$dat[[group]],
      predict = predicted
    )

    ## Mantener compatibilidad con la salida anterior
    results$dat[[paste0("outcome.predict_cutoff_", i)]] <- predicted
  }

  # results$dat$outcome.predict <- results$classifications[[1]]

  ## Métricas diagnósticas e intervalos de confianza
  ma <- results$res_sum[[method.cutoff]]$Global$measures.acc

  # Correspondencia entre puntos de corte óptimos y measures.acc
  cutoff_idx <- vapply(cutoffs, function(z) {
    which.min(abs(ma$cutoffs - z))
  }, integer(1))

  if (!isTRUE(all.equal(
    unname(ma$cutoffs[cutoff_idx]),
    unname(cutoffs),
    tolerance = 1e-8
  ))) {
    stop("Could not match the optimal cutoffs with measures.acc.")
  }

  metric_names <- c(
    Se = "Sensitivity",
    Sp = "Specificity",
    PPV = "PPV",
    NPV = "NPV",
    DLR.Positive = "LRpositive",
    DLR.Negative = "LRnegative"
  )

  metric_data <- lapply(names(metric_names), function(metric) {
    z <- ma[[metric]]
    data.frame(
      Estimate = z[, "Value"],
      Lower = z[, "ll"],
      Upper = z[, "ul"]
    )
  })
  names(metric_data) <- unname(metric_names)

  prev <- ma$pop.prev

  metric_data$Prevalence <- data.frame(
    Estimate = rep(prev, length(ma$cutoffs)),
    Lower = rep(NA_real_, length(ma$cutoffs)),
    Upper = rep(NA_real_, length(ma$cutoffs))
  )

  metric_data$Accuracy <- data.frame(
    Estimate = metric_data$Sensitivity$Estimate * prev +
      metric_data$Specificity$Estimate * (1 - prev),
    Lower = rep(NA_real_, length(ma$cutoffs)),
    Upper = rep(NA_real_, length(ma$cutoffs))
  )

  results$diagnostics <- data.frame(
    Cutpoint = ma$cutoffs,
    do.call(cbind, lapply(metric_data, function(z) z$Estimate)),
    check.names = FALSE
  )

  results$diagnostics_ci <- metric_data

  ## Índice del criterio de selección
  results[[method.cutoff]] <- clasRes[[method.cutoff]]$Global$optimal.criterion

  ## AUC original
  results$auc <- results$res_sum[[method.cutoff]]$Global$measures.acc$AUC

  results$tag.healthy <- tag.healthy

  ## Formato de métricas e IC para siguientes bloques
  format_metric <- function(z, percentage = FALSE) {
    if (percentage) z <- z * 100
    ifelse(
      is.finite(z),
      format(signif(z, 4), trim = TRUE, scientific = FALSE),
      "NA"
    )
  }

  ## Tablas con kable_ueb
  if (show.tables) {
    results$kable_tables <- list(
      classification = vector("list", n_cutoffs),
      diagnostics = vector("list", n_cutoffs),
      combined = vector("list", n_cutoffs)
    )

    ## Escala del cutoff que se mostrará en las tablas
    if (length(original_predictors) == 1 &&
        is.numeric(dat[[original_predictors]]) &&
        all(is.finite(results$cutoff.variable))) {
      cutoff_labels <- paste0(
        vapply(results$cutoff.variable, format_metric, character(1)),
        " (", original_predictors, ")"
      )
    } else {
      cutoff_labels <- paste0(
        vapply(cutoffs, format_metric, character(1)),
        " (predicted probability)"
      )
    }

    for (i in seq_len(n_cutoffs)) {

      idx <- cutoff_idx[i]
      cutoff_label <- cutoff_labels[i]

      ## Tabla de clasificación: filas predichas, columnas reales
      tab_class <- t(results$table[[i]])

      kb1 <- kable_ueb(
        tab_class,
        caption = paste0(
          "Cut-off (", method.cutoff, ") = ", cutoff_label,
          "<br>Columns: Real value <br> Rows: Predicted value"
        )
      )

      ## Accuracy e IC binomial exacto
      n_total <- sum(tab_class)
      n_correct <- sum(diag(tab_class))
      ci_acc <- binom.test(n_correct, n_total)$conf.int

      ## Prevalencia e IC binomial exacto
      n_positive <- sum(tab_class[, positive.class])
      ci_prev <- binom.test(n_positive, n_total)$conf.int

      ## Métricas e IC obtenidos de OptimalCutpoints
      metric_data <- list(
        Accuracy = c(n_correct / n_total, ci_acc),
        Sensitivity = unlist(ma$Se[idx, c("Value", "ll", "ul")]),
        Specificity = unlist(ma$Sp[idx, c("Value", "ll", "ul")]),
        PPV = unlist(ma$PPV[idx, c("Value", "ll", "ul")]),
        NPV = unlist(ma$NPV[idx, c("Value", "ll", "ul")]),
        LRpositive = unlist(ma$DLR.Positive[idx, c("Value", "ll", "ul")]),
        LRnegative = unlist(ma$DLR.Negative[idx, c("Value", "ll", "ul")]),
        Prevalence = c(n_positive / n_total, ci_prev)
      )

      ## Tabla de métricas diagnósticas
      diag_values <- vapply(names(metric_data), function(metric) {

        z <- metric_data[[metric]]
        percentage <- !metric %in% c("LRpositive", "LRnegative")

        paste0(
          format_metric(z[1], percentage),
          " (",
          format_metric(z[2], percentage),
          "; ",
          format_metric(z[3], percentage),
          ")"
        )

      }, character(1))

      diag_table <- data.frame(
        Values = diag_values,
        row.names = names(metric_data),
        check.names = FALSE
      )

      kb2 <- kable_ueb(
        diag_table,
        col.names = "Values",
        row.names = TRUE,
        caption = paste0(
          "Cut-off (", method.cutoff, ") = ", cutoff_label,
          "<br>Reference class = ", tag.healthy,
          "<br>Positive class = ", positive.class
        ),
        escape = FALSE
      )

      results$kable_tables$classification[[i]] <- kb1
      results$kable_tables$diagnostics[[i]] <- kb2

      ## Tabla conjunta
      tabla_unida <- dplyr::bind_cols(
        Clasificación = kb1,
        Diagnósticos = kb2
      )

      results$kable_tables$combined[[i]] <- kable_ueb(tabla_unida) |>
        kable_styling(
          full_width = FALSE,
          position = "center",
          bootstrap_options = c("striped", "hover", "bordered")
        )
    }

    names(results$kable_tables$classification) <- paste0("cutoff_", seq_len(n_cutoffs))
    names(results$kable_tables$diagnostics) <- paste0("cutoff_", seq_len(n_cutoffs))
    names(results$kable_tables$combined) <- paste0("cutoff_", seq_len(n_cutoffs))
  }

  ## Boxplot de la variable explicativa
  if (show.boxplot) {
    if (length(original_predictors) != 1) {
      warning("Boxplot was not generated: only one explanatory variable is allowed.")
    } else if (!is.numeric(dat[[original_predictors]]) || !all(is.finite(results$cutoff.variable))) {
      warning("Boxplot was not generated: an original-scale numeric cut-off is required.")
    } else {
      predictor <- original_predictors

      cutoff_lines <- data.frame(
        cutoff = results$cutoff.variable,
        label = paste0(
          "Cut-off = ",
          vapply(results$cutoff.variable, format_metric, character(1))
        )
      )

      results$boxplot <- ggplot2::ggplot(
        dat,
        ggplot2::aes(x = .data[[group]], y = .data[[predictor]])
      ) +
        ggplot2::geom_boxplot(outlier.shape = NA) +
        ggplot2::geom_jitter(width = 0.15, alpha = 0.5) +
        ggplot2::geom_hline(
          data = cutoff_lines,
          ggplot2::aes(yintercept = cutoff, linetype = label),
          color = "#993489"
        ) +
        ggplot2::scale_linetype_discrete(name = method.cutoff) +
        ggplot2::labs(
          title = title,
          x = group,
          y = predictor
        ) +
        ggplot2::theme_bw()
    }
  }

  ## Bootstrap del AUC
  if (!is.null(n.boot)) {

    auc_rank <- function(score, outcome, healthy, direction) {
      outcome <- as.character(outcome)
      positive <- outcome != healthy

      if (length(unique(positive)) != 2 || anyNA(score) || any(!is.finite(score))) {
        return(NA_real_)
      }

      if (identical(direction, ">")) score <- -score

      n_pos <- sum(positive)
      n_neg <- sum(!positive)
      ranks <- rank(score, ties.method = "average")

      auc_value <- (sum(ranks[positive]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
      as.numeric(auc_value)
    }

    ## Datos completos para el bootstrap
    if (modGLM) {
      mf <- model.frame(frml, data = dat, na.action = na.omit)
      boot_dat <- dat[rownames(mf), , drop = FALSE]
    } else {
      boot_dat <- dat[complete.cases(dat[, c(group, x), drop = FALSE]), , drop = FALSE]
    }

    boot_y <- as.character(boot_dat[[group]])
    idx_healthy <- which(boot_y == tag.healthy)
    idx_positive <- which(boot_y == positive.class)

    if (length(idx_healthy) == 0 || length(idx_positive) == 0) {
      warning("Bootstrap could not be performed: both outcome categories are required.")
      boot_auc <- numeric(0)
    } else {

      boot_auc <- rep(NA_real_, n.boot)

      for (i in seq_len(n.boot)) {

        idx_boot <- c(
          sample(idx_healthy, length(idx_healthy), replace = TRUE),
          sample(idx_positive, length(idx_positive), replace = TRUE)
        )

        dat_boot <- boot_dat[idx_boot, , drop = FALSE]

        if (modGLM) {

          mod_boot <- tryCatch(
            suppressWarnings(glm(frml, data = dat_boot, family = binomial)),
            error = function(e) NULL
          )

          if (is.null(mod_boot) || !isTRUE(mod_boot$converged)) next

          pred_boot <- tryCatch(
            suppressWarnings(predict(mod_boot, type = "response")),
            error = function(e) NULL
          )

          if (is.null(pred_boot) || any(!is.finite(pred_boot))) next

          score_boot <- as.numeric(pred_boot)

        } else {
          score_boot <- dat_boot[[x]]
        }

        boot_auc[i] <- auc_rank(
          score = score_boot,
          outcome = dat_boot[[group]],
          healthy = tag.healthy,
          direction = direction
        )
      }
    }

    valid_auc <- boot_auc[is.finite(boot_auc)]

    if (length(valid_auc) >= 2) {

      results$bootstrap <- list(
        n.boot = n.boot,
        n.valid = length(valid_auc),
        n.failed = n.boot - length(valid_auc),
        auc.boot = valid_auc,
        auc.mean = mean(valid_auc),
        auc.se = sd(valid_auc),
        auc.ci = unname(quantile(valid_auc, probs = c(0.025, 0.975)))
      )

      names(results$bootstrap$auc.ci) <- c("2.5%", "97.5%")

    } else {

      warning("Fewer than two valid bootstrap iterations. Bootstrap CI could not be estimated.")

      results$bootstrap <- list(
        n.boot = n.boot,
        n.valid = length(valid_auc),
        n.failed = n.boot - length(valid_auc),
        auc.boot = valid_auc,
        auc.mean = NA_real_,
        auc.se = NA_real_,
        auc.ci = c("2.5%" = NA_real_, "97.5%" = NA_real_)
      )
    }
  }

  ## Tabla resumen de métricas de los puntos de corte óptimos

  ## AUC e IC 95% de OptimalCutpoints
  auc_info <- as.numeric(ma$AUC[1:3])

  ## IC 95% del bootstrap
  if (!is.null(results$bootstrap)) {
    bootstrap_ci <- as.numeric(results$bootstrap$auc.ci)
    n_boot_valid <- results$bootstrap$n.valid
    n_boot <- results$bootstrap$n.boot
  } else {
    bootstrap_ci <- c(NA_real_, NA_real_)
    n_boot_valid <- NA_integer_
    n_boot <- NA_integer_
  }

  ## Criterion (Índice de Youden o el que sea)
  criterion <- rep_len(as.numeric(results[[method.cutoff]]), n_cutoffs)

  ## Escala y valor del punto de corte
  if (identical(results$cutoff.type, "Original predictor scale")) {
    cutoff_values <- results$cutoff.variable
  } else {
    cutoff_values <- results$cutoff.probability
  }

  ## Métricas para cada punto de corte óptimo
  results$metrics <- purrr::map_dfr(seq_len(n_cutoffs), function(i) {

    idx <- cutoff_idx[i]

    ## Matriz de confusión
    tab <- results$table[[i]]
    n_total <- sum(tab)
    n_correct <- sum(diag(tab))

    ## Accuracy e IC 95% binomial exacto
    acc <- n_correct / n_total
    acc_ci <- binom.test(n_correct, n_total)$conf.int

    data.frame(
      Model = model.name,
      Observations = n_total,

      AUC = auc_info[1],
      AUC_CI_low = auc_info[2],
      AUC_CI_high = auc_info[3],

      AUC_boot_CI_low = bootstrap_ci[1],
      AUC_boot_CI_high = bootstrap_ci[2],
      n_boot_valid = n_boot_valid,
      n_boot = n_boot,

      setNames(data.frame(criterion[i]), paste0("Criterion (", method.cutoff, ")")),

      Sensitivity = ma$Se[idx, "Value"],
      Sensitivity_CI_low = ma$Se[idx, "ll"],
      Sensitivity_CI_high = ma$Se[idx, "ul"],

      Specificity = ma$Sp[idx, "Value"],
      Specificity_CI_low = ma$Sp[idx, "ll"],
      Specificity_CI_high = ma$Sp[idx, "ul"],

      PPV = ma$PPV[idx, "Value"],
      PPV_CI_low = ma$PPV[idx, "ll"],
      PPV_CI_high = ma$PPV[idx, "ul"],

      NPV = ma$NPV[idx, "Value"],
      NPV_CI_low = ma$NPV[idx, "ll"],
      NPV_CI_high = ma$NPV[idx, "ul"],

      LR_positive = ma$DLR.Positive[idx, "Value"],
      LR_positive_CI_low = ma$DLR.Positive[idx, "ll"],
      LR_positive_CI_high = ma$DLR.Positive[idx, "ul"],

      LR_negative = ma$DLR.Negative[idx, "Value"],
      LR_negative_CI_low = ma$DLR.Negative[idx, "ll"],
      LR_negative_CI_high = ma$DLR.Negative[idx, "ul"],

      Accuracy = acc,
      Accuracy_CI_low = acc_ci[1],
      Accuracy_CI_high = acc_ci[2],

      Cutoff = cutoff_values[i],
      Cutoff_scale = results$cutoff.type,

      check.names = FALSE
    )
  })


  ## Mensajes de compatibilidad
  message(" !!!!!!!!! \n UEBmessage: Output 'thres.best' are deprecated, new same output is 'cutoff.probability' \n !!!!!!!!")
  message(" !!!!!!!!! \n UEBmessage: tag.healthy: ", tag.healthy, " \n !!!!!!!!")

  return(results)
}
