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
#' @export quickCor
#' @import xtable mada
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' data(mtcars)
#' quickCor(x = "mpg", y = "hp", dat = mtcars,
#' nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE)
#' @return results:
#' @return coeff:
#' @return plot
#' @keywords quickCor pearson sperman plotcor correlation


quickCor <- function(x, y, dat,
                     nround = 3, xtab = TRUE, pos = "bottomleft", sz.xtab = "small",
                     pearson = TRUE, corplot = TRUE)
{
  pe <- cor.test(dat[,x],dat[,y], method = "pearson")
  sp <- cor.test(dat[,x],dat[,y], method = "spearman")
  ic.sp <- CIrho(sp$estimate, dim(na.omit(dat[ ,c(x,y)]))[1], level = 0.95 )
  Pearson <- c(round(pe$estimate,nround),
               paste0("(", round(pe$conf.int[1],nround), ", ", round(pe$conf.int[2],nround), ")"),
               round(pe$p.value,nround) )

  Spearman <- c(round(sp$estimate,nround),
                paste0("(", round(ic.sp[2],nround), ", ", round(ic.sp[3],nround), ")"),
                round(sp$p.value,nround) )

  result <- t(data.frame(Pearson, Spearman))
  colnames(result) <- c("rho", "IC","p-value")

  fit <- lm(dat[,y] ~ dat[,x])

  if (corplot) {
    plot(dat[,x], dat[,y],
         xlab = x, ylab = y,
         col = "purple", pch = 19,
         main =  paste("Correlation",x, "whit",y)  )
    abline(fit, col = "red", lwd = 3, lty = 3)
    txt.plot <- ifelse(pearson,
                       paste("Pearson Correlation = ", result["Pearson","rho"],
                             "\n 95%CI",result["Pearson","IC"],"p-value = ",
                             result["Pearson","p-value"]),
                       paste("Spearman Correlation = ", result["Spearman","rho"],
                             "\n 95%CI",result["Spearman","IC"],"p-value = ",
                             result["Spearman","p-value"]))
    legend(pos, c("Observations","Linear fit"), cex = 0.8, # horiz = TRUE,
           lty = c(-1,3), pch = c(19, NA), lwd = c(1,2),col = c("purple","red"))
    mtext(txt.plot, cex = 0.8, line = -1.8 )

  }
  if (xtab) {
    print(xtable(result, caption = paste("Correlation",x, "whit",y)),size = sz.xtab)
  }else{
    return( list(coeff = summary(fit), result = result) )
  }

}
