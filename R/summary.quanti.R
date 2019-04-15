#' A summary.quanti Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param x numeric or integer variable
#' @param group factor variable. Outcome
#' @param data data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.
#' @param format  a character string; possible values are ht, r, no. Default value is "ht".
#' @param method character string indicating the method to test use; possible values are 'param' or 'nonparam'. Default values is 'param'.
#' @param test character strin indicating the test to use. Possible values are 'anova','t.test','wilcox','kruskal'. Default value is NULL
#' @param nround integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed (see ‘Details’). Default value is 2.
#' @param show.pval logical indicating whether p-value of overall groups significance ('p.overall' column) is displayed or not. Default value is TRUE.
#' @param show.all logical indicating whether the 'ALL' column (all data without stratifying by groups) is displayed or not. Default value is FALSE if grouping variable is defined, and FALSE if there are no groups.
#' @param show.n ogical indicating whether number of individuals analyzed for each row-variable is displayed or not in the 'descr' table. Default value is TRUE.
#' @param byrow logical or NA. Percentage of categorical variables must be reported by rows (TRUE), by columns (FALSE) or by columns and rows to sum up 1 (NA). Default value is FALSE, which means that percentages are reported by columns (withing groups).
#' @keywords summary ci qualitative descriptive exploratory
#' @export summary.quanti
#' @import Publish
#' @examples
#'  # set.seed(1)
#'  # data <- df <- data.frame(MUT = factor(c(rep("A", 12),rep("B",13))),
#'  #                          var = rnorm(25))
#'
#'  # summary.quanti(x = "var", data = df)
#'  # summary.quanti(x = "var",group = "MUT",data = df)


summary.quanti <- function(x,
                           group = NULL,
                           data,
                           method = "param",
                           format = "html",
                           nround = 1,
                           test = NULL,
                           show.pval = TRUE,
                           show.all = TRUE,
                           show.n = TRUE,
                           byrow = FALSE){


  new_line <- switch(format, "html" = " <br> ", "latex" = " \\\\ " , "R" = " \n ")
  xx <- data[,x]
  if (!is.null(group)) yy <- data[, group]

  ## Resum univariat mean(sd) \\ IC mean \\ median[IQR]
  ci_uni <- ci.mean(xx)
  mn_sd <- paste0(round(mean(xx,na.rm = T),nround), " (", round(sd(xx,na.rm = T),nround), ")")
  md_iqr <- paste0(round(median(xx,na.rm = T),nround), " [",
                   round(quantile(xx,na.rm = T, probs = 0.25),nround),",",
                   round(quantile(xx,na.rm = T, probs = 0.75),nround),"]")

  ci_uni <- paste0("CI[",round(ci_uni$lower, nround), ";", round(ci_uni$upper, nround), "]")
  res_uni <- data.frame( ALL = paste0(mn_sd, new_line, ci_uni, new_line, md_iqr))
  res_uni <- cbind(variable = x, res_uni)
  if (show.n) res_uni$n <- length(xx[complete.cases(xx)])

  ### Análisis per grup
  if (!is.null(group)) {
    sum_bi <- aggregate(xx ~ yy, data = data, FUN = function(x) c(mean = round(mean(x, na.rm = T),nround),
                                                                  sd = round(sd(x, na.rm = T),nround),
                                                                  median = round(median(x,na.rm = T),nround),
                                                                  q25 = round(quantile(x,na.rm = T, probs = 0.25),nround),
                                                                  q75 = round(quantile(x,na.rm = T, probs = 0.75),nround)))
    ci_bi <- ci.mean(xx ~ yy, data = data)
    res_all <- data.frame(t(paste0(paste0( sum_bi$xx[,"mean"]," (", sum_bi$xx[,"sd"], ")" ), new_line,
                                   paste0("IC[",round(ci_bi$lower,nround), "; ", round(ci_bi$upper,nround),"]" ), new_line,
                                   paste0( sum_bi$xx[,"median"]," [", sum_bi$xx[,"q25.25%"],", ", sum_bi$xx[,"q75.75%"], "]" ))))
    colnames(res_all) <- levels(yy)
    rownames(res_all) <- x
    res_all <- cbind(variable = x, res_all)

    ### Es mostra columna ALL
    if (show.all)    res_all$ALL  <- res_uni$ALL

    ### Test
    if (show.pval) {
      ## Decidim test que es realitza
      if (is.null(test))    test <- switch(method,
                                           "param" = ifelse(length(levels(yy)) > 2, "anova","t.test"),
                                           "non-param" = ifelse(length(levels(yy)) > 2, "kruskal","wilcox"))
      ## Calculem test
      pval <- switch(test,
                     "t.test" = t.test(xx~yy)$p.va,
                     "wilcox" = wilcox.test(xx~yy)$p.va,
                     "anova" = summary(aov(xx~yy))[[1]][["Pr(>F)"]][1],
                     "kruskal" = kruskal.test(xx~yy)$p.va)

      res_all$p.value <- ifelse(pval < 0.001, "0.001", round(pval,3) )

    }

    if (show.n) res_all$n <- length(xx[complete.cases(yy)])

  }
  caption = c("mean(sd) <br> [IC95% mean] <br> median[IQR] ")

  ## RESULTATS
  ifelse(exists("res_all"),
         return(list(rows = x, columns = group, caption = caption, summary = res_all )),
         return(list(variable = x,caption = caption,  summary = res_uni)))

}











