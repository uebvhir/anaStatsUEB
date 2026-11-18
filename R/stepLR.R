#' @title Regresión logística por pasos hacia adelante (AIC y test LRT)
#'
#' @description
#' Construye un modelo de regresión logística mediante selección por pasos hacia
#' adelante a partir de un conjunto de variables explicativas. En cada paso se
#' evalúa añadir cada variable candidata y se elige la de menor AIC, que solo
#' entra en el modelo si el p-valor del test de razón de verosimilitudes (LRT)
#' frente al modelo anterior es inferior a \code{thrPval}.
#'
#' IMPORTANTE: durante la selección solo se usan los individuos con valores en
#' todas las variables indicadas (\code{VR}, \code{varExpl} y \code{var2mod}). El
#' segundo modelo final se reajusta con todos los individuos que tienen valores en
#' las variables finales.
#'
#' @param VR Nombre de la variable respuesta (texto). Debe ser dicotómica: un
#'   factor de dos niveles o una variable numérica 0/1.
#' @param varExpl Vector con los nombres de las variables explicativas candidatas.
#'   Debe haber al menos dos.
#' @param data Data frame con los datos.
#' @param var2mod Vector con los nombres de las variables que se quieren incluir en
#'   el modelo desde el principio, sean o no significativas. Por defecto,
#'   \code{NA}: se parte del modelo nulo.
#' @param trace Lógico. Si es \code{TRUE} (por defecto), muestra la construcción
#'   del modelo paso a paso; véase \sQuote{Details}.
#' @param thrPval Umbral del p-valor (LRT entre dos modelos): una variable entra si
#'   su p-valor es inferior a este valor (por defecto, 0.1).
#'
#' @return Una lista de dos modelos \code{glm} (binomial):
#' \describe{
#'   \item{\code{[[1]]}}{Modelo final ajustado con los individuos con valores en
#'     \code{VR}, en todas las \code{varExpl} y en las variables seleccionadas.}
#'   \item{\code{[[2]]}}{Mismo modelo, ajustado con los individuos con valores en
#'     \code{VR} y en las variables seleccionadas.}
#' }
#'
#' @details
#' \itemize{
#'   \item En cada paso se compara la variable de menor AIC entre todas las
#'     candidatas; si su p-valor no es inferior a \code{thrPval}, la selección se
#'     detiene, aunque haya otra candidata significativa.
#'   \item Las variables de \code{var2mod} se excluyen de las candidatas
#'     mediante una expresión regular, por lo que un nombre que contenga otro
#'     (por ejemplo, \code{hp} y \code{hp2}) también queda excluido.
#'   \item Nota: en la versión actual, \code{trace = TRUE} produce un error al
#'     imprimir el modelo intermedio (\code{non-numeric-alike variable(s) in data
#'     frame}), porque usa la función obsoleta \code{tabOR_lr()}. Hasta que se
#'     corrija, usar \code{trace = FALSE}.
#'   \item Nota: cuando solo queda una variable candidata no se evalúa
#'     correctamente (aviso de coerción y \code{min()} de \code{Inf}), por lo que
#'     la última candidata nunca entra en el modelo. Si ninguna variable entra en
#'     el primer paso se produce un error, y también si \code{var2mod} contiene
#'     todas las \code{varExpl}.
#'   \item Con \code{trace = TRUE}, los resúmenes finales de los modelos no se
#'     imprimen (el código correspondiente no se ejecuta).
#' }
#'
#' @seealso \code{\link[stats]{glm}}, \code{\link[stats]{step}},
#'   \code{\link[stats]{anova}}, \code{\link{desc_mod}}
#'
#' @examples
#' modfin <- stepLR(VR = "vs", varExpl = c("hp", "am", "carb"),
#'                  data = mtc_bis, trace = FALSE)
#' summary(modfin[[1]])
#' summary(modfin[[2]])
#'
#' @keywords step regresion logistica stepwise forward glm aic
#'
#' @importFrom stats glm anova as.formula na.omit binomial
#'
#' @author
#' Miriam Mota \email{mmota.foix@@gmail.com}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
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
