#' ROC analysis and optimal cut-off estimation
#'
#' Performs ROC curve analysis for a continuous marker or for the predicted
#' probabilities of a logistic regression model. The function estimates the
#' optimal cut-off according to different criteria (e.g. Youden index, minimum
#' sensitivity or specificity), computes diagnostic accuracy measures and
#' optionally displays the ROC curve.
#'
#' If `modGLM = TRUE`, a logistic regression model is first fitted using
#' `frml`, and the ROC analysis is performed on the predicted probabilities.
#'
#' @param frml A model formula. The left-hand side must be the binary outcome
#'   and the right-hand side one or more predictor variables.
#' @param x Character string indicating the quantitative predictor. Ignored
#'   when `modGLM = TRUE`.
#' @param group Character string indicating the binary outcome variable.
#' @param dat A data frame containing the variables.
#' @param tag.healthy Character string indicating the reference (healthy)
#'   category of the outcome. By default, the first factor level is used.
#' @param title Character string used as the plot title.
#' @param modGLM Logical. If `TRUE`, a logistic regression model is fitted
#'   before computing the ROC curve using the predicted probabilities.
#' @param method.cutoff Character string indicating the criterion used to
#'   determine the optimal cut-off. Passed to
#'   `OptimalCutpoints::optimal.cutpoints()`. Common options include:
#'   `"Youden"`, `"MinValueSe"`, `"ValueSe"`, `"MinValueSp"`,
#'   `"ValueSp"` and `"MinValueSpSe"`.
#' @param sens_target Target sensitivity when required by the selected
#'   cut-off method.
#' @param spec_target Target specificity when required by the selected
#'   cut-off method.
#' @param doPlot Logical indicating whether the ROC curve should be plotted.
#' @param cex.main Numeric. Expansion factor for the main title.
#' @param cex.text Numeric. Expansion factor for the annotation text.
#' @param cex.sub Numeric. Expansion factor for the subtitle.
#' @param cex Numeric. General graphical expansion factor.
#' @param show.lg Logical indicating whether the legend should be displayed.
#' @param show.cascon Logical indicating whether the number of cases and
#'   controls should be displayed on the plot.
#' @param show.detail Logical indicating whether the detailed table of
#'   cut-off performance should be returned.
#' @param xtab Logical indicating whether the summary results should be
#'   formatted using `kable()`.
#' @param xtab.type Output format passed to `knitr::kable()`.
#' @param direction Character indicating the direction of the comparison.
#'   Either `"<"` or `">"`. Passed to
#'   `OptimalCutpoints::optimal.cutpoints()`.
#' @param ... Additional graphical parameters passed to `plot()`.
#'
#' @return A list containing:
#' \describe{
#'   \item{mod}{Fitted logistic regression model (`modGLM = TRUE`).}
#'   \item{dat}{Original data including predicted probabilities and predicted
#'     classes.}
#'   \item{res_sum}{Summary returned by
#'     `OptimalCutpoints::summary()`.}
#'   \item{res_detail}{Detailed table of cut-offs, sensitivity,
#'     specificity, LR+ and LR-. Returned only if
#'     `show.detail = TRUE`.}
#'   \item{cutoff.probability}{Optimal probability threshold when
#'     `modGLM = TRUE`.}
#'   \item{cutoff.variable}{Optimal cut-off for the original predictor, when
#'     available.}
#'   \item{auc}{Area under the ROC curve (AUC).}
#'   \item{table}{Confusion matrix based on the selected cut-off.}
#'   \item{tag.healthy}{Reference category used as healthy controls.}
#'   \item{xtab}{Formatted summary table (if `xtab = TRUE`).}
#' }
#'
#' @details
#' The function is a wrapper around
#' `OptimalCutpoints::optimal.cutpoints()`. It allows ROC analyses to be
#' performed directly on a biomarker or on the predicted probabilities from
#' a logistic regression model. Confidence intervals for the ROC statistics
#' are computed by the underlying package.
#'
#' @author
#' Marcos Esteve, Miriam Mota
#'
#' @seealso
#' \code{\link[OptimalCutpoints]{optimal.cutpoints}},
#' \code{\link[stats]{glm}}
#'
#' @examples
#' ## ROC from a single biomarker
#' res <- doROC(
#'   x = "marker",
#'   group = "status",
#'   dat = mydata,
#'   modGLM = FALSE
#' )
#'
#' ## ROC from a logistic regression model
#' res <- doROC(
#'   frml = status ~ age + marker,
#'   dat = mydata,
#'   modGLM = TRUE
#' )
#'
#' @export
doROC <- function(frml, x , group  , dat,
                  tag.healthy = NULL,
                  title = NULL,
                  modGLM = NULL,
                  method.cutoff = "Youden",
                  sens_target = 0.90,
                  spec_target = NULL,
                  doPlot = TRUE,
                  cex.main = 2,
                  cex.text = 1.4,
                  cex.sub = 0.9,
                  cex = 0.5,
                  show.lg = TRUE,
                  show.cascon = TRUE,
                  show.detail = TRUE,
                  xtab = FALSE,
                  xtab.type = "latex",
                  direction = c("<", ">"), ...)
{

  ## comprovacions varies, warnings i errors
  if (exists(deparse(substitute(show.ci)))) message("\n UEBmessage: Argument 'show.ci' is deprecated \n")
  if (exists(deparse(substitute(validation)))) message("\n UEBmessage: Argument 'validation' is deprecated \n")
  if (exists(deparse(substitute(test_y)))) message("\n UEBmessage: Arguments 'test' and 'test_y' are deprecated \n")
  if (exists(deparse(substitute(col.thres)))) message("\n UEBmessage: Argument 'col.thres' is deprecated \n")
  if (exists(deparse(substitute(col.ic)))) message("\n UEBmessage: Argument 'col.ic' is deprecated \n")
  if (exists(deparse(substitute(x.axes)))) message("\n UEBmessage: Argument 'x.axes' is deprecated \n")
  if (exists(deparse(substitute(show.thr)))) message("\n UEBmessage: Argument 'show.thr' is deprecated \n")
  if (is.null(modGLM)) stop("Es necesario indicar, TRUE o FALSE para el parametro modGLM.")
  if ((missing(x) | missing(group)) & missing(frml))  stop("'x' and 'group' argument required, or 'frml' argument required", call. = FALSE)

  # if (missing(x)) x <- strsplit(as.character(frml), "~", fixed = T)[[3]]
  if (missing(x)) x <-  rhs.vars(frml)
  if (missing(frml)) frml <- as.formula(paste(group, "~", paste0(x, collapse = " + ")))
  if (missing(group)) group <- lhs.vars(frml)
  if (is.null(title)) title <- paste(group, "-",paste0(x, collapse = "+"))
  if (is.null(tag.healthy)) tag.healthy <- levels(dat[,group])[1]

  dat[,group] <- relevel(dat[,group], ref = tag.healthy)

  results <- list()

  ## assignació variable resposta (group) i variable cuantitativa (x) o formula (frml). També titol y nivell de referencia (tag.healthy)
  if (modGLM) {
    mod <- glm(frml, data = dat, family = binomial, na.action = "na.omit")
    results$mod <- mod
    pred <- predict(mod, type = "response")
    dat$pred <- NA
    dat[names(pred),]$pred <- pred
    x <- "pred"
  }
  results$dat <- dat

  # Para otros métodos que no son Youden y que requieren parámetros
  control_args <- switch(
    method.cutoff,
    "MinValueSe" = control.cutpoints(valueSe = sens_target),
    "ValueSe"    = control.cutpoints(valueSe = sens_target),
    "MinValueSp" = control.cutpoints(valueSp = spec_target),
    "ValueSp"    = control.cutpoints(valueSp = spec_target),
    "MinValueSpSe" = control.cutpoints(valueSe = sens_target, valueSp = spec_target),
    control.cutpoints()  # default
  )



  # calcul corba ROC, punt optim amb index de youden i mesures de clasificacio
  positive.class <- levels(dat[,group])[levels(dat[,group]) != tag.healthy]

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


  if (doPlot) {
    opt <- par(cex = cex, cex.main = cex.main, ...)
    plot(clasRes, which = 1, legend = show.lg,
         ylim = c(0,1))
    par(opt)
    mtext(title, side = 3, cex = cex.sub)
    if (show.cascon) {
      text(.85, .25,
           paste0("controls: ", clasRes[[method.cutoff]]$Global$measures.acc$n$h,
                  "\n cases: ", clasRes[[method.cutoff]]$Global$measures.acc$n$d),
           cex = cex.text)
    }

  }

  if (show.detail) {
    results$res_detail <- cbind(results$res_sum[[method.cutoff]]$Global$measures.acc$cutoffs,
                                results$res_sum[[method.cutoff]]$Global$measures.acc$Se[,1],
                                results$res_sum[[method.cutoff]]$Global$measures.acc$Sp[,1],
                                results$res_sum[[method.cutoff]]$Global$measures.acc$DLR.Positive[,1],
                                results$res_sum[[method.cutoff]]$Global$measures.acc$DLR.Negative[,1])
    colnames(results$res_detail) <- c("Cutpoint", "Sensitivity", "Specificity", "LR+", "LR-")
  }

  ## es mostren els resultats general com xtable per a latex
  if (xtab) {
    # print(xtable(results$res_sum$p.table$Global[[method.cutoff]][[1]],
    #        caption = paste(title,". AUC ", results$res_sum$p.table$Global$AUC_CI )))

    res_xtab <- kable(results$res_sum$p.table$Global[[method.cutoff]][[1]], format = xtab.type, booktabs = T,
                      caption = paste(title,". AUC ", results$res_sum$p.table$Global$AUC_CI ), longtable = TRUE,
                      escape = F)
    results$xtab <- res_xtab

  }

  ## punts de talls
  if (modGLM) {
    results$cutoff.probability <- clasRes[[method.cutoff]]$Global$optimal.cutoff$cutoff # threshold  de Youden probability
    name_var_cuanti <-  rhs.vars(frml)

    if (length(rhs.vars(frml)) == 1) {
      results$cutoff.variable <- unique(results$dat[,name_var_cuanti][which(results$dat$pred == results$cutoff.probability)])
    }else{
      results$cutoff.variable <- "No se puede calcular debido a que existe más de una variable explicativa."
    }
    if (identical(direction, ">") ) {
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.probability, tag.healthy, positive.class ))
    }else{
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.probability, positive.class, tag.healthy ))
    }

  }else{
    results$cutoff.variable <- clasRes[[method.cutoff]]$Global$optimal.cutoff$cutoff # punto de corte optimo, segun Youden para variable numerica

    if (identical(direction, ">") ) {
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.variable, tag.healthy, positive.class ))
    }else{
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.variable, positive.class, tag.healthy ))
    }
  }
  results$dat$outcome.predict <- factor(results$dat$outcome.predict, c(tag.healthy,positive.class))

  results[[method.cutoff]] <- clasRes[[method.cutoff]]$Global$optimal.criterion
  results$auc <- results$res_sum[[method.cutoff]]$Global$measures.acc$AUC
  results$table <- table(Group = results$dat[,group], predict = results$dat$outcome.predict)
  results$tag.healthy <- tag.healthy

  # missatge canvi de nom a output
  message(" !!!!!!!!! \n UEBmessage: Output 'thres.best' are deprecated, new same output is 'cutoff.probability' \n !!!!!!!!!")

  message(" !!!!!!!!! \n UEBmessage: tag.healthy: ", tag.healthy , " \n !!!!!!!!!")
  return(results)
}
