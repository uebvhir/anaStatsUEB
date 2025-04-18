#' quickCor Function
#'
#' La función quickCor
#' @param x,y   numeric vectors of data values. x and y must have the same length.
#' @param dat an optional matrix or data frame (or similar: see model.frame) containing the variables in the formula formula.
#' @param nround integer indicating the number of decimal places (round).
#' @param xtab A logical value indicating whether the output is a xtable
#' @param method "pearson", "spearman" or "both" Default value is "pearson".
#' @param corplot A logical value indicating whether the output is a plot. Default value is TRUE.
#' @param pos a character string indicating the legend location. Options: "bottomright",  "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is "small".
#' @param cex.txt character expansion factor. NULL and NA are equivalent to 0.8. This is an absolute measure, not scaled by par("cex") or by setting par("mfrow") or par("mfcol"). Can be a vector.
#' @param sz.xtab A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is NULL.
#' @param cex.main settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.
#' @param xtab.type Type of table to produce. Possible values for type are "latex" or "html". Default value is "latex".
#' @param main an overall title for the plot.
#' @param sub a sub title for the plot.
#' @param lm.fit A logical value indicating if show a linear regression line. Default value is TRUE.
#' @param pos.text on which MARgin line, starting at 0 counting outwards.
#' @param cor_cut integer indicating the number cut relevant correlation
#' @export quickCor
#' @usage #' @usage \\method{names}{mtc_bis}(x) <- value
#' @import xtable mada
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' # quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' # nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE, sub = "subtitle")
#' # # Spearman correlation
#' # quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' # nround = 3, xtab = FALSE, pearson = FALSE, corplot = TRUE, sub = "subtitle")
#' # # No es mostra recta de regressio
#' # quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' # nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE, sub = "subtitle", lm.fit =FALSE)
#' # # canviem la posicio de la llegenda
#' # quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' # nround = 3, xtab = TRUE, pearson = TRUE, corplot = TRUE, sub = "subtitle",
#' # pos = "bottomright")
#' @return results:
#' @return coeff:
#' @return plot
#' @keywords quickCor pearson sperman plotcor correlation



quickCor <- function(dat, x, y,
                     nround = 3,
                     main = NULL,
                     xtab = TRUE,
                     pos = "bottomleft",
                     sz.xtab = NULL,
                     method = "pearson", #"pearson", "spearman" or "both"
                     corplot = TRUE,
                     cex.txt = 0.8,
                     cex.main = 0.8,
                     xtab.type = "html",
                     sub = NULL,
                     lm.fit = TRUE,
                     show.res = TRUE,
                     pos.text = -1.8,
                     show.pval = TRUE,
                     prep.tab = FALSE,
                     cor_cut = 0.7) {
  x <- names(dat %>% dplyr::select({{x}}))
  y <- names(dat %>% dplyr::select({{y}}))


  if (!is.numeric(dat[, x])) stop("La variable x debe ser numérica")
  if (!is.numeric(dat[, y])) stop("La variable y debe ser numérica")
  if(length(unique(na.omit(dat[, x]))) <2 | length(unique(na.omit(dat[, y]))) <2 ) stop("No es posible calcular la correlación. Una de las variables es constante.")
  namex <- ifelse(Hmisc::label(dat[,x]) == "", x,  Hmisc::label(dat[,x]))
  namey <- ifelse(Hmisc::label(dat[,y]) == "", y,  Hmisc::label(dat[,y]))


  n <- nrow(na.omit(dat[ , c(x, y)]))

  if(method == "pearson" | method == "both"){
    pe <- cor.test(dat[, x], dat[, y], method = "pearson")
    Pearson <- c(round(pe$estimate, nround),
                 paste0("(", round(pe$conf.int[1], nround), ", ", round(pe$conf.int[2], nround), ")"),
                 round(pe$p.value, nround),n )
    if(method == "pearson") result <- t(data.frame(Pearson))
  }
  if(method == "spearman" | method == "both"){
    sp <- cor.test(dat[, x], dat[, y], method = "spearman")
    ic.sp <- CIrho(sp$estimate, dim(na.omit(dat[ , c(x, y)]))[1], level = 0.95 )
    Spearman <- c(round(sp$estimate, nround),
                  paste0("(", round(ic.sp[2], nround), ", ", round(ic.sp[3], nround), ")"),
                  round(sp$p.value, nround),n )
    if(method == "spearman") result <- t(data.frame(Spearman))
  }
  if(method == "both"){
    result <- t(data.frame(Pearson, Spearman))
  }


  colnames(result) <- c("rho", "IC", "p-value", "n")
  result[,"p-value"][which(as.numeric(as.character(result[,"p-value"])) < 0.001)] <- "<0.001"

  if (!show.pval) result <- result[, !colnames(result) %in% c("p-value")]

  fit <- lm(dat[, y] ~ dat[, x])

  if (corplot) {
    if (is.null(main)) main <- paste(namex, "with", namey)
    plot(dat[, x], dat[, y],
         xlab = namex, ylab = namey,
         col = "purple",
         pch = 19,
         main =  main, cex.main = cex.main )
    mtext(sub, 3, line = .8)

    txt.plot <- ifelse(method == "pearson",
                       paste("Pearson Correlation = ", result["Pearson", "rho"],
                             "\n 95%CI", result["Pearson", "IC"], ifelse(show.pval,paste0("p-value = ",
                                                                                          result["Pearson", "p-value"]),"")),
                       paste("Spearman Correlation = ",
                             result["Spearman", "rho"],
                             "\n 95%CI", result["Spearman", "IC"], ifelse(show.pval,paste0("p-value = ",
                                                                                           result["Spearman", "p-value"]),"")))

    if (lm.fit) {
      abline(fit, col = "red", lwd = 3, lty = 3)
      legend(pos, c("Observations", "Linear fit"), cex = 0.8, # horiz = TRUE,
             lty = c(-1, 3), pch = c(19, NA),
             lwd = c(1, 2), col = c("purple", "red"),bg = "transparent")
    }else{
      legend(pos, "Observations", cex = 0.8,
             lty = -1, pch = 19,
             lwd = 1, col = "purple",bg = "transparent")
    }
    mtext(txt.plot, cex = cex.txt, line = pos.text )

  }


  result_list <- list(coeff = summary(fit), methods = "Correlation coefficient", result = result)
  if (prep.tab) {
    qc_res <- data.frame(result)
    result_list$df_prep_tab <- data.frame(t(c(variable = namex,
                                              levels = paste(rownames(qc_res), collapse = " <br>"),
                                              summary = paste(qc_res$rho, qc_res$IC, collapse = " <br> " ),
                                              p.value = paste(qc_res$p.value, collapse = " <br> " ),
                                              n = unique(qc_res$n))))
  }

  safe_sp_estimate <- tryCatch(sp$estimate, error = function(e) NULL)

  if (!is.null(safe_sp_estimate) && (abs(pe$estimate) > cor_cut | abs(safe_sp_estimate) > cor_cut)) {
    result_list$select <- namex
  }


  if (xtab) {

    print(kable_ueb(result, caption = paste("Correlation", x, "whit", y,".", sub)))
    # }else{
  }
  if (show.res) return( result_list )

}


## corrplot color by group
# ggplot(cansue_cantum_wide_clin, aes(x=miR_106b.CANsue, y=miR_106b.CANtum, color = TN_HER2)) +
#   geom_point() + geom_rug()


## corrplot color by group
# ggplot(cansue_cantum_wide_clin, aes(x=miR_106b.CANsue, y=miR_106b.CANtum, color = TN_HER2)) +
#   geom_point() + geom_rug()
