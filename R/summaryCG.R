#' summaryCG Function
#'
#' Summary of restab$descr
#' @param res an object of class 'compareGroups'
#' @param restab an object of class 'createTable' (of param 'res').
#' @param dat a data frame containing the variables in the model.
#' @param y a vector variable that distinguishes the groups. It must be either a numeric, character, factor or NULL. Default value is NULL which means that descriptives for whole sample are calculated and no test is performed
#' @param met.adj correction method. Can be abbreviated. Default value is 'fdr'
#' @param xtab A logical value indicating whether the output is a xtable. Default value is FALSE.
#' @param col A logical value indicating the xtable color. Default value is TRUE.
#' @param title Character vector containing the table's caption or title. Default value is NULL.
#' @param sz.xtab A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is small.
#' @param xtab.type A character string. Possible values are latex, html, markdown, pandoc, and rst; this will be automatically determined if the function is called within knitr; it can also be set in the global option knitr.table.format. If format is a function, it must return a character string.
#' @param lbl Character vector of length 1 containing the LaTeX label. Default value is NULL.
#' @details The adjustment methods include the Bonferroni correction ('bonferroni') in which the
#' p-values are multiplied by the number of comparisons. Less conservative corrections
#'  are also included by Holm (1979) ('holm'), Hochberg (1988) ('hochberg'), Hommel
#'  (1988) ('hommel'), Benjamini & Hochberg (1995) ('BH' or its alias 'fdr'),
#'  and Benjamini & Yekutieli (2001) ('BY'), respectively.
#' @export summaryCG
#' @seealso \code{\link{p.adjust}}
#' @import compareGroups xtable
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' res <- compareGroups(am ~., dat = mtc_bis, method = NA)
#' restab <- createTable(res)
#' summaryCG(res, restab,  dat = mtc_bis, y = 'am', xtab = FALSE)
#' @return summary table.
#' @keywords comparegroups summary tests


summaryCG <- function(res,
                      restab,
                      dat,
                      y,
                      xtab = FALSE,
                      col = TRUE,
                      title = NULL,
                      lbl = NULL,
                      met.adj = "fdr",
                      sz.xtab = 8,
                      xtab.type = "latex",
                      sort.pval = FALSE) {
  dat[,y] <- factor(dat[,y])

  if (sum(Hmisc::label(dat) == "") != 0) {
    varnames <- labnames <- rownames(restab$avail)[rownames(restab$avail) %in% names(dat)]
  } else {
    varnames <- rownames(restab$avail)[rownames(restab$avail) %in% Hmisc::label(dat)]
    labnames <- rownames(restab$avail)
  }

  restab$avail[restab$avail[, "method"] == "continuous-normal", "method"] <- "quantitative-normal"
  restab$avail[restab$avail[, "method"] == "continuous-non-normal", "method"] <- "quantitative-non-normal"

  test <- NULL
  for (i in 1:dim(restab$avail)[1]) {
    m_var <- restab$avail[, "method"][i]
    switch(m_var, `quantitative-normal` = {
      test[i] <- ifelse(length(levels(dat[, y])) == 2, "Student's t-Test", "ANOVA")
    }, `quantitative-non-normal` = {
      test[i] <- ifelse(length(levels(dat[, y])) == 2, "U Mann-Withney test", "Kruskall-Wallis")
    }, categorical = {
      test[i] <- ifelse(sum(table(dat[, y], dat[, varnames[i]]) < 5) == 0,
                        "Chi-squared test",
                        "Fisher's exact test")
    })
  }

  # pval <- NA
  # for (i in 1:length(rownames(restab$avail))) {
  #   pval[i] <- na.omit(as.numeric(as.character(summary(res)[[labnames[i]]][, "p.overall"])))[1]
  # }
  pval <- getResults(restab, "p.overall")

  idx_order <- order(pval)
  pval.adj <- p.adjust(pval, method = met.adj)
  if (xtab & col) {
    pval <- ifelse(pval < 0.05,
                   paste0("\\colorbox{thistle}{", round(pval, 3), "}"),
                   round(pval, 3))
  }
  if (xtab & col) {
    pval.adj <- ifelse(pval.adj < 0.05,
                       paste0("\\colorbox{thistle}{", round(pval.adj, 3), "}"),
                       round(pval.adj, 3))
  }

  resum <- cbind(variable = rownames(restab$avail),
                 restab$avail[, !colnames(restab$avail) %in% c("select","Fact OR/HR")],
                 test,
                 p.value = pval,
                 adj.p.value = pval.adj)

  colnames(resum)[colnames(resum) == "[ALL]"] <- "N"
  colnames(resum)[colnames(resum) == "method"] <- "type"

  resum[, "type"][(resum[, "type"] == "quantitative-normal") |
                    (resum[, "type"] == "quantitative-non-normal")] <- "quantitative"

  if (sort.pval) resum <- resum[idx_order, ]

  if (xtab) {
    # print(xtable(resum, caption = title, label = lbl),
    #       size = sz.xtab,
    #       sanitize.text.function = function(x) x,
    #       include.rownames = FALSE, tabular.environment = "longtable", floating = FALSE)

    kable(resum, format = xtab.type, booktabs = T,
          caption = title, longtable = TRUE, escape = F)%>%
      kable_styling(latex_options = c("striped","hold_position", "repeat_header"), sz.xtab = size, full_width = F, position = "left")
  } else {
    return(resum)
  }
}
