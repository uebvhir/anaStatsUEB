#' @title Prueba estadística para datos numéricos según un factor categórico
#' @description Realiza pruebas estadísticas para comparar una variable numérica entre niveles de un factor categórico.
#' Soporta datos apareados y selecciona automáticamente entre pruebas paramétricas y no paramétricas según el número de niveles del factor.
#'
#' @param data Un `data.frame` que contenga la variable numérica y la variable categórica.
#' @param factor_col Nombre de la columna categórica (debe ser un factor).
#' @param numerica_col Nombre de la columna numérica (debe ser numérica).
#' @param parametrico Lógico. Si `FALSE`, se usan pruebas paramétricas (t-test, ANOVA); si `FALSE`, se usan pruebas no paramétricas (Mann-Whitney, Kruskal-Wallis). Por defecto es `TRUE`.
#' @param paired Lógico. Si `TRUE`, se realizan pruebas apareadas (t-test pareado o Wilcoxon pareado). Solo válido si el factor tiene exactamente 2 niveles. Por defecto es `FALSE`.
#'
#' @return Una lista con los siguientes elementos:
#' \itemize{
#'   \item `pvalor`: P-valor de la prueba.
#'   \item `estadistica`: Estadístico de la prueba (si aplica).
#'   \item `test`: Nombre de la prueba utilizada.
#' }
#'
#' @details
#' - Si el factor tiene **2 niveles**:
#'   - **Datos NO apareados**: Se usa `t.test` (paramétrico) o `wilcox.test` (no paramétrico).
#'   - **Datos apareados**: Se usa `t.test` con `paired = TRUE` o `wilcox.test` con `paired = TRUE`.
#'
#' - Si el factor tiene **3 o más niveles**:
#'   - Se usa `aov` (ANOVA) si es paramétrico o `kruskal.test` si es no paramétrico.
#'
#' @examples
#' # Ejemplo con datos NO apareados (2 grupos, t-test)
#' data <- data.frame(
#'   Grupo = factor(rep(c("A", "B"), each = 10)),
#'   Medida = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
#' )
#' test_numericas(data, "Grupo", "Medida", parametrico = TRUE, paired = FALSE)
#'
#' # Ejemplo con datos apareados (t-test pareado)
#' data_pareada <- data.frame(
#'   Sujeto = factor(rep(1:10, times = 2)),
#'   Momento = factor(rep(c("Antes", "Despues"), each = 10)),
#'   Medida = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
#' )
#' test_numericas(data_pareada, "Momento", "Medida", parametrico = TRUE, paired = TRUE)
#'
#' @export



test_numericas <- function(data, factor_col, numerica_col, parametrico = FALSE, paired = FALSE) {
  # Extraer las columnas
  factor <- data[[factor_col]]
  numerica <- data[[numerica_col]]

  # Validaciones
  if (!is.factor(factor)) stop("La variable categórica debe ser un factor.")
  if (!is.numeric(numerica)) stop("La variable numérica debe ser numérica.")

  # Contar niveles del factor
  niveles <- length(unique(factor))

  # Caso: Datos apareados (solo 2 niveles permitidos)
  if (paired) {
    if (niveles != 2) stop("Los datos apareados deben tener exactamente 2 niveles.")

    # Ordenar para asegurar correspondencia
    data_ordenada <- data[order(factor), ]
    factor_ordenado <- data_ordenada[[factor_col]]
    numerica_ordenada <- data_ordenada[[numerica_col]]

    # Separar en dos grupos
    grupo1 <- numerica_ordenada[factor_ordenado == levels(factor_ordenado)[1]]
    grupo2 <- numerica_ordenada[factor_ordenado == levels(factor_ordenado)[2]]

    # Aplicar test apareado
    if (parametrico) {
      test <- t.test(grupo1, grupo2, paired = TRUE)
      return(list(pvalor = test$p.value, estadistica = test$statistic, test = "t-test pareado"))
    } else {
      test <- wilcox.test(grupo1, grupo2, paired = TRUE)
      return(list(pvalor = test$p.value, estadistica = test$statistic, test = "Wilcoxon pareado"))
    }
  }

  # Caso: Datos NO apareados con 2 niveles
  if (niveles == 2) {
    if (parametrico) {
      test <- t.test(numerica ~ factor)
      return(list(pvalor = test$p.value, estadistica = test$statistic, test = "t-test de Student"))
    } else {
      test <- wilcox.test(numerica ~ factor)
      return(list(pvalor = test$p.value, estadistica = test$statistic, test = "U de Mann-Whitney"))
    }
  }

  # Caso: Datos NO apareados con 3+ niveles
  if (niveles >= 3) {
    if (parametrico) {
      test <- aov(numerica ~ factor)
      pvalor <- summary(test)[[1]]$`Pr(>F)`[1]
      return(list(pvalor = pvalor, test = "ANOVA"))
    } else {
      test <- kruskal.test(numerica ~ factor)
      return(list(pvalor = test$p.value, test = "Kruskal-Wallis"))
    }
  }
}



