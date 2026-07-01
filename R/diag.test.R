#' A diag.test Function
#'
#' mesures de clasificació
#' @param pred variable factor outcome predit.
#' @param y variable factor outcome original
#' @param tag.healthy the value codifying healthy individuals in the status variable. Por defecto nivel de referencia levels(dat[,group])[1]
#' @param nround integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed
#' @export diag.test
#' @import epiR caret e1071
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' # data(aSAH)
#' # res_roc <- doROC (frml = outcome ~ s100b, title = 'prova1',
#' # cex.main = 0.6, dat = aSAH, modGLM = TRUE)
#' # diag.test(y = aSAH$outcome, pred = res_roc$dat$outcome.predict, tag.healthy = "Good"  )
#'
#' # df_pred <- data.frame(p1 = res_roc$dat$outcome.predict,
#' # p2 = factor(ifelse(aSAH$s100b >= 0.3, levels(aSAH$outcome)[2], levels(aSAH$outcome)[1]  )))
#' # diag.test(y = aSAH$outcome, pred = df_pred)
#' @return variable$table:  tabla 2x2 de la predicción y referencia.
#' @return variable$positive.class:  factor level that corresponds to a "positive" result (if that makes sense for your data)
#' @return summary: taula detallada amb totes les mesures de classificació per a cada una de les variables
#' @keywords roc glm test


diag.test <- function(pred, y, tag.healthy = levels(y)[1] , nround = 2){
  if (class(y)[length(class(y))] != "factor")
    stop("La variable 'y' debe ser factor")
  n_var <- ifelse(is.null(ncol(pred)), 1, ncol(pred))
  sum_ac_l <- list()
  classification <- list()
  pos.class <- NA
  for (i in 1:n_var) {
    if (n_var == 1) {
      name_var <- deparse(substitute(pred))
      pred_i <- pred
    }
    else {
      name_var <- names(pred)[i]
      pred_i <- pred[, i]
    }
    classification[["variable"]][[name_var]] <- list()
    if (!all(levels(pred_i) %in% levels(y)))
      stop("\n ERROR: los niveles de las variables", name_var,
           " e 'y' deben ser los mismos \n ")
    pred_i <- factor(pred_i, c(levels(pred_i)[levels(pred_i) !=
                                                tag.healthy], tag.healthy))
    y <- factor(y, c(levels(y)[levels(y) != tag.healthy],
                     tag.healthy))
    tab2test <- table(pred_i, y)
    classification[["variable"]][[name_var]][["table"]] <- tab2test
    epiRes <- epi.tests(tab2test)
    if (epiRes$detail %>% dplyr::filter(statistic == "se") %>% dplyr::select(est) == confusionMatrix(table(pred_i,
                                                                                             y))$byClass[["Sensitivity"]]) {
      positive <- confusionMatrix(table(pred_i, y))$positive
      classification[["variable"]][[name_var]][["positive.class"]] <- positive
      pos.class[i] <- positive
    }
    else {
      stop("Error: Problemas calculo diag test! ")
    }
    if (!identical(rownames(tab2test), colnames(tab2test)))
      stop("Error en l'ordre de les variables")
    ll <- list()
    ll[["Accuracy"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "diag.ac") %>% dplyr::select(-statistic))) *
      100
    ll[["Sensitivity"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "se") %>% dplyr::select(-statistic))) *
      100
    ll[["Specificity"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "sp") %>% dplyr::select(-statistic))) *
      100
    ll[["PPV"]] <-  unlist(c(epiRes$detail %>% dplyr::filter(statistic == "pv.pos") %>% dplyr::select(-statistic))) * 100
    ll[["NPV"]] <- c(unlist(c(epiRes$detail %>% dplyr::filter(statistic == "pv.neg") %>% dplyr::select(-statistic)))) * 100
    ll[["LRpositive"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "lr.pos") %>% dplyr::select(-statistic)))
    ll[["LRnegative"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "lr.neg") %>% dplyr::select(-statistic)))
    ll[["Prevalence"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "tp") %>% dplyr::select(-statistic))) *
      100
    res <- data.frame(matrix(unlist(ll), nrow = length(ll),
                             byrow = T), stringsAsFactors = FALSE)
    rownames(res) <- names(ll)
    colnames(res) <- c("Value", "IC low 95%", "IC up 95% ")
    classification[["variable"]][[name_var]][[name_var]] <- res
    sum_ac_l[[name_var]] <- apply(res, 1, function(x) paste0(round(x[1],
                                                                   nround), " (", round(x[2], nround), "; ", round(x[3],
                                                                                                                   nround), ")"))
  }
  classification[["summary"]] <- list()

  ## comprovem que per a totes les variables la positive.class (es a dir, els 'casos') sigui sempre el mateix
  if (length(unique(pos.class)) == 1) {
    classification[["summary"]][["positive.class.all"]] <- unique(pos.class)
  }
  else {
    stop("Existen distintas 'positive.class'")
  }
  sum_ac <- data.frame(matrix(unlist(sum_ac_l), nrow = length(sum_ac_l),
                              byrow = T), stringsAsFactors = FALSE)
  rownames(sum_ac) <- names(sum_ac_l)
  colnames(sum_ac) <- names(sum_ac_l[[1]])
  sum_ac <- t(sum_ac)
  classification[["summary"]][["table"]] <- sum_ac
  return(classification)
}
