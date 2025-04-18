% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/desc_numeric.R
\name{desc_numeric}
\alias{desc_numeric}
\title{A desc_numeric Function}
\usage{
desc_numeric(
  data,
  covariates,
  y,
  frml = NULL,
  method = "non-param",
  method.cor = "pearson",
  caption = NULL,
  font_size = 13,
  width_lev = "9em",
  col.background = "#993489",
  show.pval = TRUE,
  show.all = TRUE,
  show.n = TRUE,
  corplot = FALSE,
  nround = 1,
  cor_cut = 0.7,
  pval_cut = 0.05,
  ...
)
}
\arguments{
\item{data}{data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.}

\item{covariates}{a character string with names of variables.}

\item{y}{numeric variable. Outcome}

\item{frml}{Right side of ~ must have the terms in an additive way, and left side of ~ must contain the name of the grouping variable or can be left in blank (in this latter case descriptives for whole sample are calculated and no test is performed).}

\item{method}{character string indicating the method to test use; possible values are 'param' or 'non-param'. Default values is 'non-param'.}

\item{method.cor}{character string indicating the method to correlation use; possible values are 'pearson' or 'spearman' or "both. Default values is 'pearson'.}

\item{caption}{Character vector containing the table's caption or title.}

\item{font_size}{A numeric input for table font size}

\item{width_lev}{defines the maximum width of table columns. Default value is 9em}

\item{show.pval}{logical indicating whether p-value of overall groups significance ('p.overall' column) is displayed or not. Default value is TRUE.}

\item{cor_cut}{A numeric value to variable dplyr::select}

\item{pval_cut}{A numeric value to variable dplyr::select (p.value)}
}
\description{
DESCRIPCIO DE LA FUNCIO
}
\author{
Miriam Mota-Foix <mmota.foix@gmail.com>
}
\keyword{ci}
\keyword{descriptive}
\keyword{exploratory}
\keyword{outcome}
\keyword{quantitative}
\keyword{summary}
