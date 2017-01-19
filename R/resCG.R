#' resCG Function
#'
#' Summary of restab$descr
#' @param restab an object of class 'createTable'.
#' @param dat a data frame containing the variables in the model.
#' @param y a vector variable that distinguishes the groups. It must be either a numeric, character, factor or NULL. Default value is NULL which means that descriptives for whole sample are calculated and no test is performed
#' @param xtab A logical value indicating whether the output is a xtable
#' @param col A logical value indicating the xtable color.
#' @param title Character vector containing the table's caption or title. Default value is NULL.
#' @param lbl Character vector of length 1 containing the LaTeX label. Default value is NULL.
#' @export resCG
#' @import compareGroups xtable
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' res <- compareGroups(am ~., data = mtc_bis, method = NA)
#' restab <- createTable(res)
#' resCG(restab, mtc_bis, y = "am", xtab = FALSE)
#' @return summary table.
#' @keywords comparegroups summary tests


resCG <- function(restab, dat, y, xtab = FALSE, col = TRUE, title = NULL, lbl = NULL){

  restab$avail[restab$avail[,"method"] == "continuous-normal","method"] <- "quantitative-normal"
  restab$avail[restab$avail[,"method"] == "continuous-non-normal","method"] <- "quantitative-non-normal"

  test <- NULL
  for (i in 1:dim(restab$avail)[1]) {
    m_var = restab$avail[,"method"][i]
    switch(m_var,
           "quantitative-normal" = {
             test[i] <- "Student's t-Test"
           },
           "quantitative-non-normal" = {
             test[i] <- "U Mann-Withney test"
           },
           "categorical" = {
             test[i] <- ifelse(sum(table(dat[,y], dat[,rownames(restab$avail)[i]]) < 5) == 0 , "Chi-squared test", "Fisher's exact test" )
           }
    )
  }

  pval.ch <- restab$descr[,"p.overall"][complete.cases(restab$descr[,"p.overall"])]
  pval <- as.character(as.numeric(gsub("<","",pval.ch)))
  if (xtab & col) { pval <- ifelse(pval < 0.05, paste0("\\colorbox{thistle}{", pval, "}"), pval)}

  resum <- cbind(variable = rownames(restab$avail), restab$avail[ , !colnames(restab$avail) %in% c("select","Fact OR/HR")],
                 test,p.value = pval)

  colnames(resum)[colnames(resum) == "[ALL]"] <- "N"
  colnames(resum)[colnames(resum) == "method"] <- "type"


  if (xtab) {
    print(xtable(resum, caption = title, label = lbl), size = "small"  ,
          sanitize.text.function = function(x) x,
          include.rownames = FALSE)
  }else{
    return(resum)
  }

}
