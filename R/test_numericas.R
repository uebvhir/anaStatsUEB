#' @title Prueba estadística para datos numéricos según un factor categórico
#'
#' @description
#' Compara una variable numérica entre los niveles de un factor categórico y
#' devuelve el p-valor y el nombre del test aplicado. Admite datos apareados y
#' selecciona el test según el número de niveles del factor y el argumento
#' \code{parametrico}: t-test o ANOVA (paramétricos) y Wilcoxon/Mann-Whitney o
#' Kruskal-Wallis (no paramétricos).
#'
#' @param data Data frame que contiene la variable numérica y la variable
#'   categórica.
#' @param factor_col Cadena de texto con el nombre de la columna categórica en
#'   \code{data}. Debe ser de tipo factor.
#' @param numerica_col Cadena de texto con el nombre de la columna numérica en
#'   \code{data}. Debe ser numérica.
#' @param parametrico Valor lógico. Si es \code{TRUE}, se usan pruebas
#'   paramétricas (t-test, ANOVA); si es \code{FALSE}, pruebas no paramétricas
#'   (Mann-Whitney, Wilcoxon, Kruskal-Wallis). Por defecto \code{FALSE}.
#' @param paired Valor lógico. Si es \code{TRUE}, se realizan pruebas apareadas
#'   (t-test pareado o Wilcoxon pareado). Solo es válido si el factor tiene
#'   exactamente 2 niveles. Por defecto \code{FALSE}.
#'
#' @return Una lista con los elementos:
#' \describe{
#'   \item{pvalor}{P-valor de la prueba.}
#'   \item{estadistica}{Estadístico de la prueba (con nombre: \code{t}, \code{W}
#'     o \code{V}). Solo está presente en los tests de 2 grupos.}
#'   \item{test}{Nombre del test aplicado: \code{"t-test de Student"},
#'     \code{"U de Mann-Whitney"}, \code{"t-test pareado"},
#'     \code{"Wilcoxon pareado"}, \code{"ANOVA"} o \code{"Kruskal-Wallis"}.}
#' }
#'
#' @details
#' \itemize{
#'   \item Factor con \strong{2 niveles}, datos no apareados:
#'     \code{\link[stats]{t.test}} (paramétrico) o \code{\link[stats]{wilcox.test}}
#'     (no paramétrico).
#'   \item Factor con \strong{2 niveles}, datos apareados: \code{t.test} o
#'     \code{wilcox.test} con \code{paired = TRUE}.
#'   \item Factor con \strong{3 o más niveles}: \code{\link[stats]{aov}}
#'     (paramétrico, se devuelve el p-valor del factor) o
#'     \code{\link[stats]{kruskal.test}} (no paramétrico). En este caso no se
#'     devuelve \code{estadistica}.
#' }
#' Notas:
#' \itemize{
#'   \item El test llamado \code{"t-test de Student"} es en realidad el t-test de
#'     Welch (varianzas no asumidas iguales), que es el valor por defecto de
#'     \code{t.test}.
#'   \item En datos apareados el emparejamiento se hace por la posición de las
#'     filas dentro de cada nivel (no por un identificador de sujeto): las
#'     observaciones de ambos niveles deben estar en el mismo orden.
#'   \item El número de niveles se calcula con \code{unique(factor)}, por lo que
#'     un \code{NA} en el factor cuenta como un nivel adicional (con 2 grupos y
#'     algún \code{NA} se aplica ANOVA o Kruskal-Wallis en lugar de un test de
#'     2 grupos; con \code{paired = TRUE} da error). Elimine antes los \code{NA}
#'     del factor.
#'   \item Si el factor tiene un único nivel, la función devuelve \code{NULL}
#'     sin aviso.
#'   \item Con empates, \code{wilcox.test} avisa de que no puede calcular el
#'     p-valor exacto.
#' }
#'
#' @examples
#' set.seed(1)
#' # Datos no apareados (2 grupos, t-test)
#' datos <- data.frame(
#'   Grupo = factor(rep(c("A", "B"), each = 10)),
#'   Medida = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
#' )
#' test_numericas(datos, "Grupo", "Medida", parametrico = TRUE)
#'
#' # Datos apareados (t-test pareado)
#' datos_par <- data.frame(
#'   Sujeto = factor(rep(1:10, times = 2)),
#'   Momento = factor(rep(c("Antes", "Despues"), each = 10)),
#'   Medida = c(rnorm(10, mean = 5), rnorm(10, mean = 6))
#' )
#' test_numericas(datos_par, "Momento", "Medida", parametrico = TRUE, paired = TRUE)
#'
#' @seealso \code{\link[stats]{t.test}}, \code{\link[stats]{wilcox.test}},
#'   \code{\link[stats]{aov}}, \code{\link[stats]{kruskal.test}},
#'   \code{\link{test_categoricas}}
#'
#' @keywords htest
#'
#' @importFrom stats t.test wilcox.test aov kruskal.test
#'
#' @author
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
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



