#' Crear una tabla sjPlot::tab_model limpia para un modelo segmentado
#'
#' Esta función toma un modelo segmentado (`segmented::segmented`) y prepara una tabla con los coeficientes,
#' reemplazando los valores `Estimate` de los puntos de corte (`psi`) por los valores estimados reales,
#' ya que sjPlot::tab_model no los muestra correctamente por defecto.
#'
#' @param mod_segm Un objeto de clase `segmented`, creado con la función `segmented()`.
#'
#' @return Una tabla HTML creada con `sjPlot::tab_model()` que muestra los coeficientes del modelo,
#' incluyendo los puntos de corte `psi` con sus estimaciones correctas.
#'
#' @examples
#' #modelo_segmentado <- segmented(lm(y ~ x, data = datos), seg.Z = ~x, npsi = 1)
#' #tab_model_segmented(modelo_segmentado)
#'
#' @import sjPlot
#' @export
#'
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
