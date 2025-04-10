% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quickCor.R
\name{quickCor}
\alias{quickCor}
\title{quickCor Function}
\usage{
#' @usage \\method{names}{mtc_bis}(x) <- value
}
\arguments{
\item{dat}{an optional matrix or data frame (or similar: see model.frame) containing the variables in the formula formula.}

\item{x, y}{numeric vectors of data values. x and y must have the same length.}

\item{nround}{integer indicating the number of decimal places (round).}

\item{main}{an overall title for the plot.}

\item{xtab}{A logical value indicating whether the output is a xtable}

\item{pos}{a character string indicating the legend location. Options: "bottomright",  "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center".}

\item{sz.xtab}{A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is NULL.}

\item{method}{"pearson", "spearman" or "both" Default value is "pearson".}

\item{corplot}{A logical value indicating whether the output is a plot. Default value is TRUE.}

\item{cex.txt}{character expansion factor. NULL and NA are equivalent to 0.8. This is an absolute measure, not scaled by par("cex") or by setting par("mfrow") or par("mfcol"). Can be a vector.}

\item{cex.main}{settings for main- and sub-title and axis annotation, see \code{\link{title}} and \code{\link{par}}.}

\item{xtab.type}{Type of table to produce. Possible values for type are "latex" or "html". Default value is "latex".}

\item{sub}{a sub title for the plot.}

\item{lm.fit}{A logical value indicating if show a linear regression line. Default value is TRUE.}

\item{pos.text}{on which MARgin line, starting at 0 counting outwards.}

\item{cor_cut}{integer indicating the number cut relevant correlation}

\item{A}{character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is "small".}
}
\value{
results:

coeff:

plot
}
\description{
La función quickCor
}
\examples{
# quickCor(x = "mpg", y = "hp", dat = mtc_bis,
# nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE, sub = "subtitle")
# # Spearman correlation
# quickCor(x = "mpg", y = "hp", dat = mtc_bis,
# nround = 3, xtab = FALSE, pearson = FALSE, corplot = TRUE, sub = "subtitle")
# # No es mostra recta de regressio
# quickCor(x = "mpg", y = "hp", dat = mtc_bis,
# nround = 3, xtab = FALSE, pearson = TRUE, corplot = TRUE, sub = "subtitle", lm.fit =FALSE)
# # canviem la posicio de la llegenda
# quickCor(x = "mpg", y = "hp", dat = mtc_bis,
# nround = 3, xtab = TRUE, pearson = TRUE, corplot = TRUE, sub = "subtitle",
# pos = "bottomright")
}
\author{
Miriam Mota  \email{miriam.mota@vhir.org}
}
\keyword{correlation}
\keyword{pearson}
\keyword{plotcor}
\keyword{quickCor}
\keyword{sperman}
