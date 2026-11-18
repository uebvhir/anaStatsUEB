#' @title Prueba estadística para variables categóricas, apareadas o no
#'
#' @description
#' Compara dos variables categóricas (factores) y devuelve el p-valor y el nombre
#' del test aplicado. En datos independientes se usa el test de Chi-cuadrado o,
#' si alguna frecuencia esperada es menor que 5, el test exacto de Fisher. En
#' datos apareados se usa el test de McNemar.
#'
#' @param data Data frame que contiene las variables a comparar.
#' @param factor1 Cadena de texto con el nombre de la primera variable en
#'   \code{data}. Debe ser de tipo factor.
#' @param factor2 Cadena de texto con el nombre de la segunda variable en
#'   \code{data}. Debe ser de tipo factor.
#' @param paired Valor lógico. Si es \code{TRUE}, los datos se consideran
#'   apareados y se aplica el test de McNemar (ambas variables deben tener
#'   exactamente 2 niveles). Por defecto \code{FALSE}.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{pvalor}{P-valor numérico del test aplicado.}
#'   \item{test}{Nombre del test aplicado: \code{"Chi-squared test"},
#'     \code{"Fisher's exact test"} o \code{"McNemar's test"}.}
#' }
#'
#' @details
#' Se construye la tabla de contingencia con \code{table()} y se aplica:
#' \itemize{
#'   \item \strong{Datos apareados} (\code{paired = TRUE}):
#'     \code{\link[stats]{mcnemar.test}} con \code{correct = FALSE}.
#'   \item \strong{Datos independientes} (\code{paired = FALSE}):
#'     \code{\link[stats]{chisq.test}} si todas las frecuencias esperadas son
#'     \eqn{\ge 5}; en caso contrario \code{\link[stats]{fisher.test}}.
#' }
#' Notas:
#' \itemize{
#'   \item Si las variables no son factores se devuelve el error
#'     \code{"Ambas variables deben ser de tipo factor."}. Con
#'     \code{paired = TRUE} y tablas distintas de 2x2 se devuelve un error.
#'   \item En tablas 2x2 el test de Chi-cuadrado se aplica con la corrección de
#'     continuidad de Yates (valor por defecto de \code{chisq.test}).
#'   \item En tablas mayores que 2x2 con frecuencias esperadas menores que 5, el
#'     test de Fisher se calcula con \code{simulate.p.value = TRUE} (p-valor de
#'     Monte Carlo): el resultado no es exacto y varía entre ejecuciones; use
#'     \code{set.seed()} para reproducirlo.
#'   \item Las filas con \code{NA} en alguna de las variables se descartan
#'     silenciosamente (\code{table()}).
#'   \item Si una variable tiene un solo nivel, \code{fisher.test} da error.
#'   \item Si no hay parejas discordantes, McNemar devuelve \code{NaN}.
#'   \item Al aplicarse \code{chisq.test} antes de decidir el test, puede
#'     aparecer el aviso "Chi-squared approximation may be incorrect".
#' }
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   Grupo = factor(rep(c("A", "B"), each = 10)),
#'   Resultado = factor(sample(c("Si", "No"), 20, replace = TRUE))
#' )
#' test_categoricas(df, "Grupo", "Resultado")
#'
#' # Datos apareados
#' df2 <- data.frame(
#'   Antes = factor(c("Si", "No", "Si", "No", "Si", "No")),
#'   Despues = factor(c("Si", "Si", "No", "No", "Si", "Si"))
#' )
#' test_categoricas(df2, "Antes", "Despues", paired = TRUE)
#'
#' @seealso \code{\link[stats]{chisq.test}}, \code{\link[stats]{fisher.test}},
#'   \code{\link[stats]{mcnemar.test}}, \code{\link{summary.quali}}
#'
#' @keywords htest categorical
#'
#' @importFrom stats chisq.test fisher.test mcnemar.test
#'
#' @author
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
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





