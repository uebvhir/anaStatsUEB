#' A stepLR Function
#'
#' Construye el mejor modelo de regresion logistica basandose en el AIC y cuyo pvalor
#' al comparar los modelos mediante LRT sea inferior a 0.1,  partiendo de 'x' variables explicativas.
#' IMPORTANTE: para la construcción de todos los modelos se tendran en cuenta solo aquellos pacientes que tengan valores para todas las variables indicadas excepto para el final que tendra
#' en cuenta las variables finales.
#' @param VR Nombre de la variable respuesta. Debe ser una variable categorica dicotomica
#' @param varExpl vector con los nombre de las variables explicativas que se quieren tener en cuenta para construir el modelo.
#' @param data data.frame con los datos a trabajar
#' @param var2mod Nombre de las variables explicativas a incluir en el modelo (sean o no significativas). En el caso de no indicar ninguna, se partira del modelo nulo
#' @param trace TRUE si se quiere mostrar la construcción del modelo paso a paso
#' @param  thrPval valor a partir del cual se considerara que una variable es significativa (p.valor resultante de comparar dos modelos). Por defecto 0.1
#' @keywords step regresion logisitica stepwise forward glm aic
#' @export stepLR
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' # modfin <- stepLR(VR = 'vs',varExpl = c('hp', 'am', 'carb'), data = mtc_bis, trace = TRUE )
#' # summary(modfin[[1]])
#' # summary(modfin[[2]])
#' @return modfin: exporta dos modelos, el final teniendo en cuenta solo aquellos individuos que tienen todos los valores para todas las variables y un segundo modelo que tiene en cuenta todos aquellos individuos que tienen valores para las variables finales.
stepLR <- function(VR,
                   varExpl,
                   data,
                   var2mod = NA,
                   trace = TRUE,
                   thrPval = 0.1) {
    if (length(varExpl) <= 1)
        stop("Debe introducirse más de una varExpl")
    for (i in 1:length(varExpl)) {
        if (sum(is.na(var2mod)) >= 1) {
            frml <- as.formula(paste(VR, "~", "1"))
            mod <- glm(frml,
                       data = na.omit(data[, c(VR, varExpl)]),
                       family = binomial)
        } else {
            frml <- as.formula(paste(VR, "~", paste(var2mod, collapse = "+")))
            mod <- glm(frml,
                       data = na.omit(data[, c(VR, var2mod, varExpl)]),
                       family = binomial)
            if (trace) {
                cat(paste(VR, "~", paste(var2mod, collapse = " + ")), "\n")
                print(round(tabOR_lr(mod, xtab = F), 3))
            }
        }
        modvar <- lapply(varExpl[!grepl(paste0(var2mod, collapse = "|"), varExpl)],
                         function(var) { if (sum(is.na(var2mod)) >= 1) {
                           formula <- as.formula(paste(VR, " ~ ", var))
                         } else {
                           formula <- as.formula(paste(VR, " ~ ", paste(var2mod, collapse = "+"), "+", var))
                         }
                           if (sum(is.na(var2mod)) >= 1) {
                             res.logist <- glm(formula,
                                               data = na.omit(data[, c(VR, varExpl)]),
                                               family = binomial)
                           } else {

                             res.logist <- glm(formula,
                                               data = na.omit(data[, c(VR, varExpl, var2mod)]),
                                               family = binomial)

                           }

                           c(var, res.logist$aic, anova(mod, res.logist, test = "LRT")$Pr[2])
                         })
        df <- data.frame(matrix(unlist(modvar), nrow = length(modvar), byrow = T),
                         stringsAsFactors = FALSE,
                         row.names = 1)
        colnames(df) <- c("AIC", "p_value")
        df$AIC <- as.numeric(df$AIC)
        df$p_value <- as.numeric(df$p_value)
        df_sel <- df[df$p_value < thrPval, ]
        if (trace & dim(df_sel)[1] > 0)
          print(round(df_sel, 3))
        varSelStep <- rownames(df)[(df$AIC == min(df$AIC, na.rm = T)) & (df$p_value < thrPval)]
        var2mod <- c(var2mod, varSelStep)
        var2mod <- na.omit(var2mod)
        if (length(varSelStep) == 0) {
          modfin <- list()
          modfin[[1]] <- glm(as.formula(paste(VR, "~", paste(var2mod, collapse = "+"))),
                             data = na.omit(data[, c(VR, varExpl, var2mod)]),
                             family = binomial)
          modfin[[2]] <- glm(as.formula(paste(VR, "~", paste(var2mod, collapse = "+"))),
                             data = na.omit(data[, c(VR, var2mod)]),
                             family = binomial)
          return(modfin)
          if (trace) {
            print(summary(modfin[[1]]))
            print(summary(modfin[[2]]))
          }
          break
        }
        if (trace)
          cat("Variable candidata a entrar", varSelStep, "\n")
    }
}
