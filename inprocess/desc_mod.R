#' A tabOR_lr Function
#'
#' Genera tabla con los coeficientes OR , intervalos de confianza y p-valores de un modelo de regresión logística
#' @param mod a fitted object of class inheriting from "glm".
#' @param xtab TRUE o FALSE, para obtener tabla en formato .tex
#' @param title if xtab = T, Character vector containing the table's caption or title.
#' @param xtab.type Type of table to produce. Possible values for type are "latex" or "html". Default value is "latex".
#' @param sz.latex A character vector that is inserted just before the tabular environment starts. This can be used to set the font size and a variety of other table settings. Initial backslashes are automatically prefixed, if not supplied by user. Default value is "small".
#' @param label Character vector of length 1 containing the LaTeX label. Default value is NULL.
#' @param show.intcp TRUE o FALSE, indica si se muestra o no el intercept del modelo. En ambos casos el modelo se ha calcula con intercept. Default value is "FALSE".
#' @param show.n TRUE o FALSE muestra el total de individuos usados para el ajuste del modelo. Default value is "TRUE".
#' @param show.aov.pval TRUE o FALSE muestra el p-valor del modelo global. Default value is "TRUE".
#' @keywords OR regresion logistica
#' @export tabOR_lr
#' @import xtable
#' @examples
#'
#'
#'

library(stringr)
library(dplyr)


df <- data.frame( x = rnorm(48,10,1),
 y = as.factor(c(rep("1",16), rep("0",32) ) ), match = c(rep(1:16,3) ) )

 library(survival)
 data(lung)
 dat <- lung
 dat$sex <- factor(dat$sex)
 m <- list()
 mod <-m$log <- glm(sex~ wt.loss, data = dat, family = binomial)
  m$lin <- lm(meal.cal~  wt.loss, data = dat)
 m$cox <- coxph(Surv(time, status) ~ sex, data = lung)
 m$clog <- survival::clogit(as.numeric(y) ~ x + strata(match), data = df)

 lapply(m, function(x) summary(x))

 summary(m$cox)
 require(papeR)
 (prettify(summary(m$lin), digits = 4))

 desc_mod(m$cox, xtab = FALSE,title='OR de los coeficientes', show.intcp = TRUE)

 (prettify(summary(mod), digits = 4))
 desc_mod(mod, xtab = FALSE,title='OR de los coeficientes')





desc_mod <- function(mod,
                     xtab = FALSE,
                     title = "title",
                     xtab.type = "latex",
                     sz.latex = "small",
                     font_size = 13,
                     label = NULL,
                     show.intcp = FALSE,
                     show.n = TRUE,
                     show.aov.pval = TRUE) {


  type_mod <-  switch(class(mod)[1],
                      glm= 'Odds Ratio',
                      clogit='Hazard Ratio',
                      lm = "Estimate",
                      coxph = "Hazard Ratio")
  pret_mod <- prettify(summary(mod))
  names(pret_mod)[names(pret_mod) == " "] <- "Variable"
  res <- pret_mod[, c("Variable", type_mod, "CI (lower)", "CI (upper)", grep("Pr", names(pret_mod), value = T) ) ]
  rownames(res) <- res$Variable

  if(!show.intcp){
    res <- res %>% filter(Variable != "(Intercept)" )
  }

  res$`P-value (Global)` <- na.omit(anova(mod,test = "Chisq")$Pr)[1]
  res$N <- nobs(mod)


  # colnames(tauORcoef) <- c("OR", "LowerIC", "UpperIC", "P-value", "P-value (Global)", "N")


  if (!show.n) {  res <- res[,!names(res) %in% ("N")]  }

  if (!show.aov.pval) {    res <- res[,!names(res) %in% ("P-value (Global)")]  }

  if (xtab) {
    kable_ueb(res, caption = title)
  } else {
    return(res)
  }
}
