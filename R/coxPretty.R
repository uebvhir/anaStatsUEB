#' A coxPretty Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param frml a formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the Surv function.
#' @param data data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.
#' @param summary.cox a character string; possible values are ht, r, no
#' @param mod_original logical value, return original cox model.
#' @param title Character vector containing the table's caption or title.
#' @param namevar Character vector containing the variable name.
#' @keywords cox coxph survival pretty
#' @export coxPretty
#' @import kableExtra survival
#' @examples
#' dat <- data.frame(tm = c(rnorm(50,12*4,19), rnorm(50,12*2,12)),
#' ev = factor(sample(c("Vivo", "Muerto"), 100, replace = TRUE)) ,
#' iq = factor(c(rep("A",50), rep("B",50))), age = rnorm(100,22,6))
#' my_surv <- Surv(time = dat$tm, event = as.numeric(dat$ev) - 1)
#' # coxPretty(my_surv ~ iq + age , data = dat)
#' # coxPretty(my_surv ~ iq + age , data = dat,summary.cox = "r", mod_original = FALSE)



coxPretty <- function(frml, data,
                      summary.cox = "ht",
                      # summary.cox = getOption(c("ht","r", "no"), "ht"),
                      mod_original= FALSE,
                      title = NULL,
                      namevar = NULL, ...) {

  results <- list()
  mod <- coxph(frml, data = data)
  if (mod_original) results$mod <- mod
  sum_mod <- papeR::prettify(summary(mod), digits = 4)[,c(" ", "Hazard Ratio", "CI (lower)", "CI (upper)","Pr(>|z|)" )]

  switch(summary.cox,
         "r" = results$sum_mod <- sum_mod,
         "ht" = (results$sum_mod_ht <- kable_ueb(sum_mod, booktabs = T,
                                                 caption = paste0( "Hazard Ratio Cox ", namevar ,""))),
         "no" = cat("")  )

  return(results)
}


