#' @title Tabla con formato para los resultados de un modelo de Cox
#'
#' @description
#' Ajusta (o recibe) un modelo de regresión de Cox (\code{coxph}) a partir de una fórmula
#' con un objeto \code{Surv} como respuesta, y devuelve los resultados (hazard ratios,
#' intervalo de confianza y p-valor) en una tabla con formato \code{kable} lista para
#' incluir en el informe.
#'
#' @param frml Objeto de fórmula, con la respuesta a la izquierda del operador \code{~} y
#'   los términos a la derecha. La respuesta debe ser un objeto de supervivencia devuelto
#'   por la función \code{Surv}.
#' @param data Data frame, lista o entorno (o un objeto que \code{as.data.frame} pueda
#'   convertir en data frame) que contiene las variables del modelo. Si no se encuentran
#'   en \code{data}, las variables se toman de \code{environment(formula)}.
#' @param summary.cox Cadena de caracteres; los valores posibles son \code{"ht"},
#'   \code{"r"} o \code{"no"}.
#' @param mod_original Valor lógico. Si es \code{TRUE}, devuelve también el modelo de Cox
#'   original.
#' @param title Vector de caracteres con el título o leyenda de la tabla.
#' @param namevar Vector de caracteres con el nombre de la variable.
#'
#' @return Una tabla con formato \code{kable} con los resultados del modelo de Cox (y, si
#'   \code{mod_original = TRUE}, también el modelo \code{coxph} original).
#'
#' @import kableExtra survival
#'
#' @examples
#' dat <- data.frame(tm = c(rnorm(50,12*4,19), rnorm(50,12*2,12)),
#' ev = factor(sample(c("Vivo", "Muerto"), 100, replace = TRUE)) ,
#' iq = factor(c(rep("A",50), rep("B",50))), age = rnorm(100,22,6))
#' my_surv <- survival::Surv(time = dat$tm, event = as.numeric(dat$ev) - 1)
#' # coxPretty(my_surv ~ iq + age , data = dat)
#' # coxPretty(my_surv ~ iq + age , data = dat,summary.cox = "r", mod_original = FALSE)
#'
#' @author
#' Miriam Mota-Foix \email{mmota.foix@gmail.com}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#' @keywords cox coxph survival pretty
#' @export
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


