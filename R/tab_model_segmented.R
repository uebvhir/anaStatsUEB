#' @title Tabla sjPlot limpia para un modelo segmentado
#'
#' @description
#' Prepara una tabla con \code{sjPlot::tab_model()} para un modelo ajustado con
#' \code{segmented::segmented()}. Los coeficientes de los puntos de corte
#' (\code{psi}) que \code{tab_model()} no muestra correctamente se sustituyen por
#' sus estimaciones reales, tomadas del resumen del modelo.
#'
#' @param mod_segm Objeto de clase \code{segmented}, creado con
#'   \code{segmented::segmented()} a partir de un modelo lineal.
#' @param show.df Lógico. Si es \code{TRUE}, añade a la tabla la columna con los
#'   grados de libertad (\code{df}). Por defecto, \code{FALSE}.
#'
#' @return Un objeto \code{sjTable}, la tabla HTML creada con
#' \code{sjPlot::tab_model()}, con los coeficientes del modelo y las estimaciones
#' correctas de los puntos de corte \code{psi}.
#'
#' @details
#' \itemize{
#'   \item En las filas de \code{psi}, la columna \code{Estimates} contiene la
#'     estimación del punto de corte (\code{summary(mod_segm)$psi[, "Est."]}); el
#'     intervalo de confianza y el p-valor quedan en blanco.
#'   \item Con \code{sjPlot::tab_model()} directamente, la fila de \code{psi}
#'     muestra una estimación de 0 con un intervalo y un p-valor sin sentido.
#'   \item La función está pensada para modelos \code{segmented} basados en
#'     \code{lm}. Con un \code{segmented} basado en \code{glm} se produce un error
#'     (clase \code{summary.segmented} no admitida).
#' }
#'
#' @seealso \code{\link[segmented]{segmented}}, \code{\link[sjPlot]{tab_model}},
#'   \code{\link{tab_model_pglobal}}
#'
#' @examples
#' if (requireNamespace("segmented", quietly = TRUE)) {
#'   set.seed(12)
#'   x <- 1:100
#'   y <- 2 + 1.5 * pmin(x, 50) + 0.2 * pmax(x - 50, 0) + rnorm(100, sd = 3)
#'   datos <- data.frame(x = x, y = y)
#'   mod_lm <- lm(y ~ x, data = datos)
#'   modelo_segmentado <- segmented::segmented(mod_lm, seg.Z = ~x, npsi = 1)
#'   tab_model_segmented(modelo_segmentado)
#'   tab_model_segmented(modelo_segmentado, show.df = TRUE)
#' }
#'
#' @keywords segmented regression breakpoint psi table sjPlot
#'
#' @importFrom sjPlot tab_model
#'
#' @author
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
tab_model_segmented <- function(mod_segm,show.df = F) {

  # Obtener el resumen del modelo segmentado
  sum_seg <- summary(mod_segm)

  # Eliminar los valores incorrectos de los coeficientes asociados a psi
  sum_seg$coefficients[grepl("^psi", rownames(sum_seg$coefficients)), ] <- NA

  # Reemplazar las estimaciones NA por los valores correctos desde sum_seg$psi
  sum_seg$coefficients[grepl("^psi", rownames(sum_seg$coefficients)), "Estimate"] <- sum_seg$psi[, "Est."]

  # Crear tabla sjPlot con los coeficientes corregidos
  return(sjPlot::tab_model(sum_seg,show.df = show.df))
}
