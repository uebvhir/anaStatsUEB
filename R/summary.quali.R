#' A summary.quali Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param x factor variable.
#' @param group factor variable. Outcome
#' @param data data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.
#' @param format  a character string; possible values are ht, r, no. Default value is "ht".
#' @param nround integer indicating the number of decimal places (round) or significant digits (signif) to be used. Negative values are allowed (see ‘Details’). Default value is 2.
#' @param test character string indicating the test to use. Possible values are 'Fisher','Chi'. Default value is NULL
#' @param show.pval logical indicating whether p-value of overall groups significance ('p.overall' column) is displayed or not. Default value is TRUE.
#' @param show.all logical indicating whether the 'ALL' column (all data without stratifying by groups) is displayed or not. Default value is FALSE if grouping variable is defined, and FALSE if there are no groups.
#' @param show.n ogical indicating whether number of individuals analyzed for each row-variable is displayed or not in the 'descr' table. Default value is TRUE.
#' @param byrow logical or NA. Percentage of categorical variables must be reported by rows (TRUE), by columns (FALSE) or by columns and rows to sum up 1 (NA). Default value is FALSE, which means that percentages are reported by columns (withing groups).
#' @keywords summary ci qualitative descriptive exploratory
#' @export summary.quali
#' @import binom
#' @examples
#'  # set.seed(1)
#'  # data <- df <- data.frame(MUT = factor(c(rep("A", 12),rep("B",13))),
#'  #                           var = factor(sample(c("Yes", "no"), 25, replace = T)))
#'  # summary.quali(x = "var", data = df)
#'  # summary.quali(group = "MUT",x = "var", data = df, show.all = F)
#'  # summary.quali(group = "MUT",x = "var", data = df, byrow = T)


summary.quali <- function(x,
                          group = NULL,
                          data,
                          format = "html",
                          nround = 1,
                          test = NULL,
                          show.pval = TRUE,
                          show.all = TRUE,
                          show.n = TRUE,
                          byrow = FALSE){

  ## Comprovacions variades
  if (!is.null(group)) {
    if (length(length(table(data[,group]))) > 10) warning("La variable group tiene mas de 10 niveles")
    if (class(data[,group])[length(class(data[,group]))] != "factor") stop("La variable group debe ser factor")}
  if (class(data[,x])[length(class(data[,x]))] != "factor") stop("La variable x debe ser factor")

  ## Assignació paametres i variables
  new_line <- switch(format, "html" = " <br> ", "latex" = " \\\\ " , "R" = " \n ")
  xx <- data[,x]
  if (!is.null(group)) yy <- data[, group]


  ## Resum univariat
  uni <- binom.confint(table(xx), sum(table(xx)), methods = "exact")
  res_uni <- data.frame( ALL =  paste0(uni$x, " (", round(uni$mean*100,nround), "%)", new_line,
                                       "[",round(uni$lower*100, nround),"; ", round(uni$upper*100, nround), "]" ),row.names =  levels(xx) )
  res_uni <- cbind(variable = c(x,rep("",nrow(res_uni) - 1)),levels = levels(xx), res_uni)
  if (show.n) res_uni$n <-  c(sum(table(xx)),rep("",nrow(res_uni) - 1))

  if (!is.null(group)) {
    # Calculem resum estadístic n(%) IC

    ## PER COLUMNES (analisi habitual)
    if (!byrow) {
      res_bi <-  apply(table(xx, yy), 2, function(x)  {
        bb <- binom.confint(x,sum(x), methods = "exact")
        data.frame(paste0(bb$x, " (", round(bb$mean*100,nround), "%)", new_line,
                          "[",round(bb$lower*100, nround),"; ", round(bb$upper*100, nround), "]" ))
      })
      res_all <- do.call(cbind,res_bi)
      colnames(res_all) <- levels(yy)
      rownames(res_all) <- levels(xx)
      res_all <- cbind(variable = c(x,rep("",nrow(res_all) - 1)),levels = levels(xx), res_all)
      caption <- c("by col <br> n(%) <br> [Exact CI]")
      ## PER FILES
    } else{
      res_bi <-  apply(table(xx, yy), 1, function(x)  {
        bb <- binom.confint(x,sum(x), methods = "exact")
        data.frame(paste0(bb$x, " (", round(bb$mean*100,nround), "%)", new_line,
                          "[",round(bb$lower*100, nround),"; ", round(bb$upper*100, nround), "]" ))
      })
      res_all <- data.frame(t(do.call(cbind,res_bi)))
      colnames(res_all) <- levels(yy)
      rownames(res_all) <- levels(xx)
      res_all <- cbind(variable = c(x,rep("",nrow(res_all) - 1)),levels = levels(xx), res_all)
      caption <- c("by row <br> n(%) <br> [Exact CI]")
    }

    ## Afegim columna ALL als resultats
    if (show.all)    res_all$ALL  <- res_uni$ALL


    ## Es realitza test estadístic
    if (show.pval) {
      ## Decidim test que es realitza
      if (is.null(test))    test <- ifelse( any(table(xx, yy)) < 5 , "Fisher","Chi")
      ## Calculem test
      pval <- switch(test,
                     "Fisher" = fisher.test(table(xx,yy))$p.va,
                     "Chi" = chisq.test(xx,yy)$p.val)

      res_all$p.value <- c(ifelse(pval < 0.001, "0.001", round(pval,3) ), rep("", nrow(res_all) - 1))

    }

    if (show.n)     res_all$n <-  c(sum(table(xx,yy)),rep("",nrow(res_all) - 1))

  }




  ## RESULTATS
  ifelse(exists("res_all"),
         return(list(rows = x, columns = group,test = test, caption = caption, summary = res_all )),
         return(list(variable = x, caption = c("n(%) <br> [Exact CI]"), summary = res_uni)))
}
