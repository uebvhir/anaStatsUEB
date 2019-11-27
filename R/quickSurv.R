#' quickSurv Function
#'
#' Realiza supervivencia Kaplan Meier, cox para variables cuali y cuanti
#'
#' @param time for right censored data, this is the follow up time. For interval data, the first argument is the starting time for the interval.
#' @param event the status indicator. Factor with two levels
#' @param tag.event A character value. Level of event
#' @param covariate variables a evaluar
#' @param data an optional matrix or data frame (or similar: see model.frame) containing the variables in the formula formula.
#' @param summary.km type of table to produce for Kaplan-Meier summary. Possible values for type are "ht", "r" or "no". Default value is "ht".
#' @param summary.cox type of table to produce for Kaplan-Meier summary. Possible values for type are "ht", "r" or "no". Default value is "ht".
#' @param title.plot a title for the plot.
#' @param title.cox a title for the Cox regression.
#' @param do.plot A logical value indicating if show plot. Default value is TRUE.
#' @param pval.plot A logical value indicating if show pvalue at plot. Default value is TRUE.
#' @param do.cox A logical value indicating if show cox regression. Default value is TRUE.
#' @param pval.cut Pvalor que s'utlitza per mostrar el Cox. Per defecte 0.05
#' @export quickSurv
#' @usage #' @usage \\method{names}{mtc_bis}(x) <- value
#' @import survival
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#'
#' # set.seed(81)
#' # dat <- data.frame(tm = c(rnorm(50,12*4,19), rnorm(50,12*2,12)),
#' # ev = factor(sample(c("Vivo", "Muerto"), 100, replace = TRUE)) ,
#' #                   iq = factor(c(rep("A",50), rep("B",50))), age = rnorm(100,22,6))
#' #
#' # quickSurv(time = tm,event = ev, covariate = age, tag.event = "Muerto",
#' #          data = dat, do.cox = TRUE,summary.cox = "ht")
#'
#'
#' # quickSurv(time = tm,event = ev, covariate = iq, tag.event = "Muerto",
#' #           data = dat, surv.median.line = "hv",
#' #           cox = TRUE, summary.cox = "ht" )
#'
#' # quickSurv(time = tm,event = ev, covariate = iq, tag.event = "Muerto",
#' #           data = dat, surv.median.line = "hv", cumevents = TRUE,
#' #          cox = TRUE, summary.cox = "ht")
#' @return results:
#' @return coeff:
#' @return plot
#' @keywords quickCor pearson sperman plotcor correlation


quickSurv <- function(time, event, tag.event,
                      covariate ,
                      data,
                      do.plot = TRUE,
                      pval.plot = TRUE,
                      title.plot = NULL,
                      summary.km = getOption(c("ht","r","no"), "ht"),
                      do.cox = TRUE,
                      summary.cox = getOption(c("ht","r","no"), "ht"),
                      title.cox = NULL,
                      pval.cut = 0.05, ...) {



  arguments <- as.list(match.call())
  event = eval(arguments$event, data)
  time = eval(arguments$time, data)
  covariate = eval(arguments$covariate, data)
  results = list()
  results$pval.cut <- pval.cut
  pval.cut.logical <- FALSE
  # fits <- NULL

  if (length(unique(na.omit(event))) != 2) stop("La variable event debe tener dos niveles")
  if (!is.numeric(time)) stop("La variable time debe ser numérica")
  if (!grepl("ht|r|no", summary.km)) warning("La variable summary.km debe contener alguno de los siguientes valores: ht, r o no")
  if (!grepl("ht|r|no", summary.cox)) warning("La variable summary.km debe contener alguno de los siguientes valores: ht, r o no")


  ## Estimem supervivencia
  namevar <- ifelse(Hmisc::label(covariate) != "", Hmisc::label(covariate), deparse(arguments$covariate))
  tag.healthy <- levels(event)[!levels(event) %in% tag.event]
  event <- relevel(event, ref = tag.healthy)
  my_surv <- results$my_surv <-  Surv(time = time, event = as.numeric(event) - 1)

  ##
  ## Per a variables qualitatives
  if (is.factor(covariate)) {
    fits <- surv_fit(my_surv ~ covariate, data = data )
    pval_fits <- surv_pvalue(fits, data = data)$pval
    pval.cut.logical <- pval_fits < pval.cut

    if (do.plot) {
      # Dibuixem Kaplan-Meier
      title.plot <- ifelse(is.null(title.plot),paste0("Kaplan-Meier estimate (",namevar, ")",title.plot))
      results$pplot <- ggsurvplot(fits, data = data,
                                  risk.table = TRUE,
                                  xlab = "Years",
                                  risk.table.col = "strata",
                                  pval = pval.plot,
                                  legend.labs = levels(covariate),
                                  title = title.plot, ...)
      print(results$pplot)

      # Resum corba KM
      sum_km <- summary(fits)$table[,c("records", "events", "median", "0.95LCL", "0.95UCL")]
      switch(summary.km,
             "r" = results$sum_km,
             "ht" = (results$sum_km_ht <- kable_ueb(sum_km)),
             "no" = cat("")  )

    }
  }
  # Cox Model
  # Si la variable es quantitativa o volem realitzar cox
  if (is.numeric(covariate) | (do.cox != "no" & pval.cut.logical) ) {

    results$cox <- coxPretty(as.formula(paste0("my_surv ~",arguments$covariate)), data = data, summary.cox = summary.cox,
                             title = title.cox , namevar = namevar)
  }
  return(results)
}

