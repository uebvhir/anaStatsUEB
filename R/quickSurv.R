#' @title Supervivencia de Kaplan-Meier y regresión de Cox
#'
#' @description
#' Estima la supervivencia para una covariable. Si es un factor, dibuja la curva de
#' Kaplan-Meier (con tabla de riesgo) y calcula el log-rank; si es numérica, ajusta
#' un modelo de Cox. También puede mostrar el Cox para factores con un p-valor
#' significativo.
#'
#' @param time Variable numérica de \code{data} con el tiempo de seguimiento (sin
#'   comillas).
#' @param event Variable de \code{data} con el evento. Debe ser un factor con dos
#'   niveles (sin comillas).
#' @param tag.event Texto con el nivel de \code{event} que indica que se ha
#'   producido el evento; el otro nivel se toma como referencia.
#' @param covariate Variable de \code{data} a evaluar (sin comillas): un factor para
#'   Kaplan-Meier o una variable numérica para Cox.
#' @param data Data frame con las variables.
#' @param do.plot Lógico. Si es \code{TRUE} (por defecto), dibuja las curvas de
#'   Kaplan-Meier. Solo se aplica a covariables factor.
#' @param pval.plot Lógico. Si es \code{TRUE} (por defecto), muestra el p-valor del
#'   log-rank en el gráfico.
#' @param title.plot Título del gráfico. Si es \code{NULL} (por defecto), se usa
#'   \code{"Kaplan-Meier estimate (<covariable>)"}; véase \sQuote{Details}.
#' @param summary.km Resumen de la curva de Kaplan-Meier: \code{"ht"} (tabla HTML),
#'   \code{"r"} o \code{"no"}. Hay que indicarlo siempre; véase \sQuote{Details}.
#' @param do.cox Si es \code{"no"}, no se calcula el Cox para factores. Cualquier
#'   otro valor, incluido \code{FALSE}, no tiene efecto (por defecto,
#'   \code{TRUE}). Para covariables numéricas, el Cox se calcula siempre.
#' @param summary.cox Resumen del modelo de Cox: \code{"ht"} (tabla HTML),
#'   \code{"r"} (data frame) o \code{"no"}. Hay que indicarlo siempre.
#' @param title.cox Título del Cox. Sin efecto en la versión actual (por defecto,
#'   \code{NULL}).
#' @param pval.cut Valor del p-valor del log-rank por debajo del cual se calcula
#'   el Cox para un factor (por defecto, 0.05).
#' @param ... Otros argumentos que se pasan a \code{survminer::ggsurvplot()},
#'   por ejemplo \code{surv.median.line = "hv"} o \code{cumevents = TRUE}.
#'
#' @return Una lista con los elementos:
#' \describe{
#'   \item{\code{pval.cut}}{Valor de \code{pval.cut} usado.}
#'   \item{\code{my_surv}}{Objeto \code{Surv} construido con \code{time} y
#'     \code{event}.}
#'   \item{\code{pplot}}{Solo para factores y \code{do.plot = TRUE}: objeto
#'     \code{ggsurvplot} con las curvas.}
#'   \item{\code{sum_km_ht}}{Solo para factores y \code{summary.km = "ht"}: tabla
#'     HTML con registros, eventos, mediana e IC al 95 \%.}
#'   \item{\code{cox}}{Si se calcula el Cox: lista de \code{\link{coxPretty}}, con
#'     \code{sum_mod_ht} (si \code{"ht"}) o \code{sum_mod} (si \code{"r"}).}
#' }
#'
#' @details
#' \itemize{
#'   \item \code{time}, \code{event} y \code{covariate} se evalúan dentro de
#'     \code{data}. Se detiene con un error si \code{event} no tiene dos niveles o
#'     si \code{time} no es numérico.
#'   \item Las etiquetas de \code{Hmisc::label} se usan, si existen, como nombre de
#'     la covariable.
#'   \item Nota: en la versión actual, \code{summary.km} y \code{summary.cox}
#'     deben indicarse siempre: los valores por defecto (\code{getOption(...)})
#'     producen un error. \code{summary.km = "r"} no devuelve la tabla. Si se
#'     indica \code{title.plot}, la función falla
#'     (\code{argument "no" is missing}). \code{title.cox} no tiene efecto.
#'   \item Nota: las funciones de \pkg{survminer} (\code{surv_fit},
#'     \code{surv_pvalue}, \code{ggsurvplot}) no están declaradas en el paquete;
#'     con factores, \pkg{survminer} debe estar cargado
#'     (\code{library(survminer)}).
#'   \item \code{event} debe ser un factor y \code{tag.event} uno de sus niveles.
#'     Las covariables de tipo texto no producen resultados (sin aviso).
#'   \item El eje X del gráfico se rotula \code{"Years"}.
#' }
#'
#' @seealso \code{\link{coxPretty}}, \code{\link{kable_ueb}},
#'   \code{\link[survival]{Surv}}, \code{\link[survival]{coxph}}
#'
#' @examples
#' set.seed(81)
#' dat <- data.frame(
#'   tm = abs(c(rnorm(50, 12 * 4, 19), rnorm(50, 12 * 2, 12))),
#'   ev = factor(sample(c("Vivo", "Muerto"), 100, replace = TRUE)),
#'   iq = factor(c(rep("A", 50), rep("B", 50))),
#'   age = rnorm(100, 22, 6)
#' )
#' # Covariable numérica: regresión de Cox
#' res <- quickSurv(time = tm, event = ev, covariate = age, tag.event = "Muerto",
#'                  data = dat, summary.km = "no", summary.cox = "r")
#' res$cox$sum_mod
#'
#' # Covariable factor: Kaplan-Meier y Cox
#' if (requireNamespace("survminer", quietly = TRUE)) {
#'   library(survminer)
#'   res2 <- quickSurv(time = tm, event = ev, covariate = iq, tag.event = "Muerto",
#'                     data = dat, summary.km = "r", summary.cox = "r",
#'                     pval.cut = 1, surv.median.line = "hv")
#'   res2$cox$sum_mod
#' }
#'
#' @keywords survival kaplan-meier cox quickSurv
#'
#' @importFrom stats na.omit relevel as.formula
#' @import survival
#'
#' @author
#' Miriam Mota \email{miriam.mota@@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
quickSurv <- function(time, event, tag.event,
                      covariate ,
                      data,
                      do.plot = TRUE,
                      pval.plot = TRUE,
                      title.plot = NULL,
                      summary.km = getOption(c("ht","r","no"), "ht"),
                      do.cox = TRUE,
                      summary.cox = getOption(c("ht","r","no"), "ht"),
                      title.cox = NULL,
                      pval.cut = 0.05, ...) {



  arguments <- as.list(match.call())
  event = eval(arguments$event, data)
  time = eval(arguments$time, data)
  covariate = eval(arguments$covariate, data)
  results = list()
  results$pval.cut <- pval.cut
  pval.cut.logical <- FALSE
  # fits <- NULL

  if (length(unique(na.omit(event))) != 2) stop("La variable event debe tener dos niveles")
  if (!is.numeric(time)) stop("La variable time debe ser numérica")
  if (!grepl("ht|r|no", summary.km)) warning("La variable summary.km debe contener alguno de los siguientes valores: ht, r o no")
  if (!grepl("ht|r|no", summary.cox)) warning("La variable summary.km debe contener alguno de los siguientes valores: ht, r o no")


  ## Estimem supervivencia
  namevar <- ifelse(Hmisc::label(covariate) != "", Hmisc::label(covariate), deparse(arguments$covariate))
  tag.healthy <- levels(event)[!levels(event) %in% tag.event]
  event <- relevel(event, ref = tag.healthy)
  my_surv <- results$my_surv <-  Surv(time = time, event = as.numeric(event) - 1)

  ##
  ## Per a variables qualitatives
  if (is.factor(covariate)) {
    fits <- surv_fit(my_surv ~ covariate, data = data )
    pval_fits <- surv_pvalue(fits, data = data)$pval
    pval.cut.logical <- pval_fits < pval.cut

    if (do.plot) {
      # Dibuixem Kaplan-Meier
      title.plot <- ifelse(is.null(title.plot),paste0("Kaplan-Meier estimate (",namevar, ")",title.plot))
      results$pplot <- ggsurvplot(fits, data = data,
                                  risk.table = TRUE,
                                  xlab = "Years",
                                  risk.table.col = "strata",
                                  pval = pval.plot,
                                  legend.labs = levels(covariate),
                                  title = title.plot, ...)
      print(results$pplot)

      # Resum corba KM
      sum_km <- summary(fits)$table[,c("records", "events", "median", "0.95LCL", "0.95UCL")]
      switch(summary.km,
             "r" = results$sum_km,
             "ht" = (results$sum_km_ht <- kable_ueb(sum_km)),
             "no" = cat("")  )

    }
  }
  # Cox Model
  # Si la variable es quantitativa o volem realitzar cox
  if (is.numeric(covariate) | (do.cox != "no" & pval.cut.logical) ) {

    results$cox <- coxPretty(as.formula(paste0("my_surv ~",arguments$covariate)), data = data, summary.cox = summary.cox,
                             title = title.cox , namevar = namevar)
  }
  return(results)
}

