#' Prueba estadística para datos categóricos apareados y no apareados
#'
#' Esta función aplica una prueba estadística para comparar dos variables categóricas.
#' Si los datos no están apareados, elige entre la prueba de **Chi-cuadrado** o **Fisher** según los valores esperados.
#' Si los datos están apareados, usa la **prueba de McNemar** en tablas 2x2.
#'
#' @param data Un data.frame que contiene las variables categóricas a comparar.
#' @param factor1 Nombre de la primera variable categórica (como string).
#' @param factor2 Nombre de la segunda variable categórica (como string).
#' @param paired Lógico. Si `TRUE`, se asume que los datos están apareados y se usa la prueba de McNemar.
#' Si `FALSE`, se usa Chi-cuadrado o Fisher según corresponda. Por defecto, `FALSE`.
#'
#' @details
#' - Si `paired = FALSE`, se crea una tabla de contingencia y se aplica la prueba de **Chi-cuadrado**.
#'   Si alguna celda esperada es menor que 5, se usa **Fisher** en su lugar.
#' - Si `paired = TRUE`, se verifica que la tabla sea **2x2** y se usa **McNemar** para evaluar cambios en datos apareados.
#'   Si la tabla no es 2x2, se genera un error.
#'
#' @return Una lista con dos elementos:
#' - `pvalor`: el valor p del test realizado.
#' - `test`: el nombre de la prueba aplicada ("Chi-squared test", "Fisher's exact test" o "McNemar's test").
#'
#' @import stats
#' @export
#'
#' @examples
#' # Ejemplo con datos no apareados (Chi-cuadrado o Fisher)
#' data <- data.frame(
#'   Grupo = factor(c("A", "A", "B", "B", "A", "B", "A", "B")),
#'   Resultado = factor(c("Sí", "No", "Sí", "No", "No", "Sí", "Sí", "No"))
#' )
#' test_categoricas(data, "Grupo", "Resultado", paired = FALSE)
#'
#' # Ejemplo con datos apareados (McNemar)
#' data_pareada <- data.frame(
#'   Antes = factor(c("Sí", "No", "Sí", "No", "Sí", "No", "Sí", "No")),
#'   Despues = factor(c("No", "Sí", "Sí", "No", "No", "Sí", "Sí", "No"))
#' )
#' test_categoricas(data_pareada, "Antes", "Despues", paired = TRUE)



test_categoricas <- function(data, factor1, factor2, paired = FALSE) {
  # Verificar que las columnas sean factores
  if (!is.factor(data[[factor1]]) | !is.factor(data[[factor2]])) {
    stop("Ambas variables deben ser de tipo factor.")
  }

  # Crear la tabla de contingencia
  tabla <- table(data[[factor1]], data[[factor2]])

  if (paired) {
    # Verificar si la tabla es 2x2 (necesario para McNemar)
    if (all(dim(tabla) == c(2, 2))) {
      test <- mcnemar.test(tabla, correct = FALSE)  # Sin corrección de continuidad
      nombre_test <- "McNemar's test"
    } else {
      stop("Para datos apareados, ambas variables deben tener exactamente 2 niveles.")
    }
  } else {
    # Aplicar Chi-cuadrado o Fisher
    test_chi <- chisq.test(tabla, simulate.p.value = FALSE)
    esperados <- test_chi$expected

    if (any(esperados < 5)) {
      test <- fisher.test(tabla, simulate.p.value = TRUE)
      nombre_test <- "Fisher's exact test"
    } else {
      test <- test_chi
      nombre_test <- "Chi-squared test"
    }
  }

  # Devolver el resultado
  return(list(pvalor = test$p.value, test = nombre_test))
}





