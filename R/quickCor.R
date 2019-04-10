#' quickCor Function
#'
#' La función quickCor
#' @param x,y   numeric vectors of data values. x and y must have the same length.
#' @param dat an optional matrix or data frame (or similar: see model.frame) containing the variables in the formula formula.
#' @param nround integer indicating the number of decimal places (round).
#' @param xtab A logical value indicating whether the output is a xtable
#' @param pearson A logical value indicating whether the text output is Pearson. Default value is TRUE.
#' @param corplot A logical value indicating whether the output is a plot. Default value is TRUE.
#' @param pos a character string indicating the legend location. Options: "bottomright",  "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".
#' @param A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is "small".
#' @param cex.txt character expansion factor. NULL and NA are equivalent to 0.8. This is an absolute measure, not scaled by par("cex") or by setting par("mfrow") or par("mfcol"). Can be a vector.
#' @param sz.xtab A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is NULL.
#' @param cex.main settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.
#' @param xtab.type Type of table to produce. Possible values for type are "latex" or "html". Default value is "latex".
#' @param sub a sub title for the plot.
#' @param lm.fit A logical value indicating if show a linear regression line. Default value is TRUE.
#' @export quickCor
#' @usage #' @usage \\method{names}{mtc_bis}(x) <- value
#' @import xtable mada
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE, sub = "subtitle")
#' # Spearman correlation
#' quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' nround = 3, xtab = FALSE, pearson = FALSE, corplot = TRUE, sub = "subtitle")
#' # No es mostra recta de regressio
#' quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE, sub = "subtitle", lm.fit =FALSE)
#' # canviem la posicio de la llegenda
#' quickCor(x = "mpg", y = "hp", dat = mtc_bis,
#' nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE, sub = "subtitle",
#' pos = "bottomright")
#' @return results:
#' @return coeff:
#' @return plot
#' @keywords quickCor pearson sperman plotcor correlation


quickCor <- function(x, y, dat,
                     nround = 3,
                     xtab = TRUE,
                     pos = "bottomleft",
                     sz.xtab = NULL,
                     pearson = TRUE,
                     corplot = TRUE,
                     cex.txt = 0.8,
                     cex.main = 0.8,
                     xtab.type = "latex",
                     sub = NULL,
                     lm.fit = TRUE) {

  if (!is.numeric(dat[, x])) stop("La variable x debe ser numérica")
  if (!is.numeric(dat[, y])) stop("La variable y debe ser numérica")
  pe <- cor.test(dat[, x], dat[, y], method = "pearson")
  sp <- cor.test(dat[, x], dat[, y], method = "spearman")
  ic.sp <- CIrho(sp$estimate, dim(na.omit(dat[ , c(x, y)]))[1], level = 0.95 )
  Pearson <- c(round(pe$estimate, nround),
               paste0("(", round(pe$conf.int[1], nround), ", ", round(pe$conf.int[2], nround), ")"),
               round(pe$p.value, nround) )

  Spearman <- c(round(sp$estimate, nround),
                paste0("(", round(ic.sp[2], nround), ", ", round(ic.sp[3], nround), ")"),
                round(sp$p.value, nround) )

  result <- t(data.frame(Pearson, Spearman))
  colnames(result) <- c("rho", "IC", "p-value")

  fit <- lm(dat[, y] ~ dat[, x])

  if (corplot) {
    plot(dat[, x], dat[, y],
         xlab = x, ylab = y,
         col = "purple",
         pch = 19,
         main =  paste(x, "with", y), cex.main = cex.main )
    mtext(sub, 3, line = .8)

    txt.plot <- ifelse(pearson,
                       paste("Pearson Correlation = ", result["Pearson", "rho"],
                             "\n 95%CI", result["Pearson", "IC"], "p-value = ",
                             result["Pearson", "p-value"]),
                       paste("Spearman Correlation = ",
                             result["Spearman", "rho"],
                             "\n 95%CI", result["Spearman", "IC"], "p-value = ",
                             result["Spearman", "p-value"]))

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
    mtext(txt.plot, cex = cex.txt, line = -1.8 )

  }
  if (xtab) {
    print(xtable(result,
                 caption = paste("Correlation", x, "whit", y,".", sub)),
          type = xtab.type,
          size = sz.xtab)
  }else{
    return( list(coeff = summary(fit), result = result) )
  }

}

