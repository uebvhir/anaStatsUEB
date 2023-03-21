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
#' @keywords summary ci qualitative txt_descriptive exploratory
#' @export summary.quali
#' @import binom
#' @examples
#'  # set.seed(1)
#'  # data <- df <- data.frame(MUT = factor(c(rep("A", 12),rep("B",13))),
#'  #                           var = factor(sample(c("Yes", "no"), 25, replace = T)))
#'  # tab <- summary.quali(x = "var", data = df)
#'  # summary.quali(group = "MUT",x = "var", data = df, show.all = F)
#'  # tab <- summary.quali(group = "MUT",x = "var", data = df, byrow = T)
#'  # kable(tab$summary,escape = F, row.names = F,align = "c", txt_caption = tab$txt_caption)  %>%
#'  # kable_styling(latex_options = c("striped","hold_position", "repeat_header"), font_size = 14) %>%
#'  # row_spec(0,background = "#993489", color = "white")
#'  # mtc_bis %>% summary.quali( x = gear, group = vs)
#'  # summary.quali( mtc_bis, x = gear, group = vs)
#'  # mtc_bis %>% summary.quali( x = "gear", group = "vs")
#'  # summary.quali( mtc_bis, x = "gear", group = "vs")






summary.quali <- function(data,
                          x,
                          group = NULL,
                          include.NA = FALSE,
                          patt.NA = "NA",
                          format = "html",
                          nround = 1,
                          test = NULL,
                          show.pval = TRUE,
                          show.all = TRUE,
                          show.n = TRUE,
                          show.stat = FALSE,
                          byrow = FALSE,
                          sub.ht = TRUE,
                          var.tidy = TRUE) {

  if (var.tidy) {
    ## Les 3 seguents linies permeten pasar el nom de la variable com a text o estil tidyverse
    x <- gsub('\"', "", deparse(substitute(x)))
    try(group <- gsub('\"', "", deparse(substitute(group))), TRUE)
    if (group == "NULL") group <- NULL
  }

  if (include.NA) {
    lb <- Hmisc::label(data[,x])
    data[,x] <- as.character(data[,x])
    data[,x][which(is.na(data[,x]))] <- patt.NA
    data[,x] <- factor_ueb(data[,x])
    Hmisc::label(data[,x]) <- lb
  }

  ## Comprovacions variades
  if (!is.null(group)) {

    if (length(length(table(data[,group]))) > 10) warning("La variable group tiene mas de 10 niveles")
    if (class(data[,group])[length(class(data[,group]))] != "factor") stop("La variable group debe ser factor")
    ### només dades completes
    data <- na.omit(data[,c(x,group)])
  }
  if (class(data[,x])[length(class(data[,x]))] != "factor") stop("La variable x debe ser factor")



  if (any(table(data[,x]) == 0 ) ) {
    lb <- Hmisc::label(data[,x])
    if (is.factor(data[,x] )) data[,x] <- droplevels(data[,x])
    message("Some levels of ", x, " are removed since no observation in that/those levels")
    Hmisc::label(data[,x]) <- lb
  }



  ## Assignació paametres i variables
  new_line <- switch(format, "html" = " <br> ", "latex" = " \\\\ " , "R" = " \n ")
  xx <- data[,x]
  varname_x <- ifelse( Hmisc::label(data[,x]) != "", Hmisc::label(data[,x]), x)
  if (!is.null(group)) {
    varname_group <- ifelse( Hmisc::label(data[,group]) != "", Hmisc::label(data[,group]), group)
    yy <- data[, group]
  }
  if (sub.ht) sub <- "<sub>1</sub>"


  ## Resum univariat
  if (!byrow) {n <-  sum(table(xx))} else {n <- table(xx) }
  uni <- binom.confint(table(xx), n , methods = "exact")
  if (is.null(uni$x)) uni$x <- uni$x.Freq
  if (is.null(uni$mean)) uni$mean <- uni$mean.Freq
  res_uni <- data.frame( ALL =  paste0(uni$x, " (", round(uni$mean*100,nround), "%)", new_line,
                                       "[",round(uni$lower*100, nround),"; ", round(uni$upper*100, nround), "]" ),row.names =  levels(xx) )
  res_uni <- cbind(variable = c(paste0(varname_x, sub),rep("",nrow(res_uni) - 1)),levels = levels(xx), res_uni)
  if (show.n) res_uni$n <-  c(sum(table(xx)),rep("",nrow(res_uni) - 1))
  txt_caption <- paste0(" <font size='1'> 1: n(%) <br> [Exact CI] </font>")

  if (!is.null(group)) {
    # Calculem resum estadístic n(%) IC

    ## PER COLUMNES (analisi habitual)
    if (!byrow) {
      res_bi <-  apply(table(xx, yy), 2, function(x)  {
        bb <- binom.confint(x,sum(x), methods = "exact")
        if (is.null(bb$x)) bb$x <- bb$x.Freq
        if (is.null(bb$mean)) bb$mean <- bb$mean.Freq
        data.frame(paste0(bb$x, " (", round(bb$mean*100,nround), "%)", new_line,
                          "CI[",round(bb$lower*100, nround),"; ", round(bb$upper*100, nround), "]" ))
      })
      res_all <- do.call(cbind,res_bi)
      colnames(res_all) <- levels(yy)
      rownames(res_all) <- levels(xx)
      res_all <- cbind(variable = c(paste0(varname_x, sub),rep("",nrow(res_all) - 1)),levels = levels(xx), res_all)
      txt_descriptive <- " <font size='1'> <br> 1: by col <br> n(%) <br> [Exact CI] </font>"
      txt_caption <- paste0("Summary of results by groups of ",varname_group ,txt_descriptive)
      ## PER FILES
    } else{
      res_bi <-  apply(table(xx, yy), 1, function(x)  {
        bb <- binom.confint(x,sum(x), methods = "exact")
        if (is.null(bb$x)) bb$x <- bb$x.Freq
        if (is.null(bb$mean)) bb$mean <- bb$mean.Freq
        data.frame(paste0(bb$x, " (", round(bb$mean*100,nround), "%)", new_line,
                          "CI[",round(bb$lower*100, nround),"; ", round(bb$upper*100, nround), "]" ))
      })
      res_all <- data.frame(t(do.call(cbind,res_bi)))
      colnames(res_all) <- levels(yy)
      rownames(res_all) <- levels(xx)

      res_all <- cbind(variable = c(paste0(varname_x, sub),rep("",nrow(res_all) - 1)),levels = levels(xx), res_all)
      txt_descriptive <- " <font size='1'> <br> 1: by row <br> n(%) <br> [Exact CI] </font>"
      txt_caption <- paste0("Summary of results by groups of ",varname_group ,txt_descriptive)
    }

    ## Afegim columna ALL als resultats
    if (show.all)    res_all$ALL  <- res_uni$ALL


    ## Es realitza test estadístic
    if (show.pval ) {
      ## Decidim test que es realitza
      if (is.null(test))    test <- ifelse( any(chisq.test(table(xx, yy))$expected < 5) , "Fisher's exact","Chi-squared")
      ## Calculem test
      pval <- try(switch(test,
                         "Fisher's exact" = fisher.test(table(xx,yy),simulate.p.value = T )$p.va,
                         "Chi-squared" = chisq.test(xx,yy)$p.val), TRUE)
      ## arreglem p.val final
      pval <- ifelse(grepl("Error", pval), ".",pval)
      pval_round <- ifelse(grepl("Error", try(round(pval,3), TRUE)), "-", round(pval,3))
      pval_round <- switch(test,
                           "Fisher's exact" = paste0(pval_round,  "<sup>3</sup>"),
                           "Chi-squared" = paste0(pval_round,  "<sup>4</sup>"))
      pval_round[grep("-",pval_round, fixed = T)] <- "-"

      res_all$p.value <- c(ifelse(pval != "." & pval < 0.001, "<0.001", pval_round ), rep("", nrow(res_all) - 1))
      txt_pval <- paste0("<font size='1'> <br> p.value: ",switch(test, "Fisher's exact" = "<sup>3</sup>","Chi-squared" = "<sup>4</sup>"), test, "</font>")
      txt_caption <-  paste(txt_caption,txt_descriptive,txt_descriptive,txt_pval )



      if (show.stat ) {

        ## Calculem test
        stat <- try(switch(test,
                           "Fisher's exact" = "-",
                           "Chi-squared" = chisq.test(xx,yy)$stat), TRUE)
        ## arreglem stat final
        stat <- ifelse(grepl("Error", stat), ".",stat)
        stat_round <- ifelse(grepl("Error", try(round(pval,3), TRUE)), "-", round(stat,3))

        stat_round[grep("-",stat_round, fixed = T)] <- "-"

        res_all$stat <- c(stat_round, rep("", nrow(res_all) - 1))



      }else{
        stat <- NULL

      }



    }else{
      pval <- NULL
      txt_pval <- NULL
    }

    if (show.n)     res_all$n <-  c(sum(table(xx,yy)),rep("",nrow(res_all) - 1))

  }




  ## RESULTATS
  ifelse(!is.null(group),
         return(list(rows = x,
                     columns = group,
                     txt_test = txt_pval,
                     pval = pval,
                     txt_caption = txt_caption,
                     methods = txt_descriptive ,
                     summary = res_all )),
         return(list(variable = x, methods = txt_caption, txt_caption = txt_caption, summary = res_uni)))
}








