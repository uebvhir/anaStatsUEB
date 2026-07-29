#' Filtrado automático de niveles escasos en variables categóricas
#'
#' @description
#' Esta función identifica y elimina automáticamente los niveles poco frecuentes
#' de variables categóricas en relación con una variable de resultado.
#'
#' Para cada variable incluida en \code{vars}, se construye una tabla de
#' contingencia frente a la variable de resultado (\code{outcome}) y se
#' identifican aquellos niveles que presentan al menos una celda con un número
#' de observaciones inferior al valor especificado en \code{min_cell}.
#'
#' Los niveles identificados como escasos se recodifican como valores perdidos
#' (\code{NA}) y posteriormente se evalúa si la variable conserva suficientes
#' categorías para continuar en el análisis. Si tras la eliminación de dichos
#' niveles queda menos de dos categorías observadas, la variable se excluye
#' automáticamente.
#'
#' La función preserva las etiquetas (\code{label}) y el orden original de los
#' niveles de las variables factor.
#'
#' @details
#' El flujo interno de la función es el siguiente:
#' \enumerate{
#'   \item \strong{Almacenamiento de metadatos:} Se guardan las etiquetas
#'   (\code{Hmisc::label}) y los niveles originales de las variables para poder
#'   restaurarlos posteriormente.
#'
#'   \item \strong{Construcción de tablas de contingencia:} Para cada variable
#'   categórica se calcula una tabla de contingencia frente a la variable de
#'   resultado especificada en \code{outcome}.
#'
#'   \item \strong{Detección de niveles escasos:} Se identifican los niveles
#'   cuya frecuencia es inferior a \code{min_cell} en al menos una de las
#'   categorías del resultado.
#'
#'   \item \strong{Recodificación de niveles:} Los niveles identificados se
#'   convierten en \code{NA}, preservando el orden original de los niveles
#'   restantes.
#'
#'   \item \strong{Validación de variables:} Si tras eliminar los niveles
#'   escasos solo queda una categoría observada, la variable se elimina de la
#'   lista de variables válidas.
#'
#'   \item \strong{Restauración de metadatos:} Se restauran las etiquetas
#'   originales de todas las variables conservadas.
#' }
#'
#' \strong{Puntos fuertes}
#'
#' \itemize{
#'   \item Reduce problemas de separación completa o cuasi-separación en modelos
#'   de regresión logística.
#'   \item Disminuye la aparición de celdas escasamente pobladas en tablas de
#'   contingencia.
#'   \item Conserva el nivel de referencia y el orden original de los factores.
#'   \item Preserva las etiquetas (\code{label}) utilizadas en otras funciones
#'   descriptivas y de modelización.
#'   \item Facilita la preparación automática de datos para análisis
#'   multivariables.
#' }
#'
#' \strong{Limitaciones}
#'
#' \itemize{
#'   \item Solo actúa sobre variables de tipo factor. Las variables numéricas
#'   se ignoran automáticamente.
#'   \item La detección de niveles escasos se basa exclusivamente en las
#'   frecuencias observadas respecto a la variable de resultado y no considera
#'   criterios clínicos o de relevancia conceptual.
#'   \item Los niveles eliminados se transforman en \code{NA}, por lo que pueden
#'   incrementar el número de valores perdidos en análisis posteriores.
#'   \item En variables con muchos niveles poco frecuentes pueden eliminarse
#'   numerosas categorías simultáneamente.
#'   \item No realiza agrupación automática de niveles; únicamente los elimina.
#' }
#'
#' @param data data.frame que contiene las variables a analizar.
#' @param vars vector de caracteres con los nombres de las variables
#'   categóricas a evaluar.
#' @param outcome nombre de la variable de resultado utilizada para construir
#'   las tablas de contingencia.
#' @param min_cell frecuencia mínima permitida en cada celda de la tabla de
#'   contingencia. Los niveles que presenten alguna celda con una frecuencia
#'   inferior a este valor serán excluidos. Por defecto \code{2}.
#'
#' @return
#' Una lista con dos elementos:
#' \describe{
#'   \item{data}{Data frame modificado tras eliminar los niveles escasos.}
#'   \item{vars_validas}{Vector con las variables que conservan al menos dos
#'   categorías observadas después del filtrado.}
#' }
#'
#' @examples
#' \dontrun{
#' data(iris)
#' iris$Species <- factor(iris$Species)
#' iris$outcome <- rbinom(nrow(iris), 1, 0.5)
#'
#' res <- filter_sparse_levels(
#'   data    = iris,
#'   vars    = "Species",
#'   outcome = "outcome",
#'   min_cell = 5)
#'
#' res$data
#' res$vars_validas
#' }
#'
#' @author Marcos Esteve \email{marcos.esteve@vhir.org}
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR)
#' 
#' Modificaciones y mantenimiento:
#' Biomedical Data Intelligence Unit (BIDU) 
#' Vall d'Hebron Research Institute (VHIR) | 
#' Vall d'Hebron Barcelona Hospital Campus.
#' 
#' @rdname filter_sparse_levels
#'
#' @importFrom Hmisc label
#' @export
filter_sparse_levels <- function(
    data,
    vars,
    outcome,
    min_cell = 2) {

### --- 0. Validaciones básicas ---
  if (!all(vars %in% names(data))) {
    stop("Alguna de las variables indicadas en 'vars' no existe en 'data'.")
  }
  if (!outcome %in% names(data)) {
    stop("La variable 'outcome' no existe en 'data'.")
  }

### --- 1. Etiquetas y niveles originales ---
  original_levels <- lapply(
    data[, vars, drop = FALSE],
    function(x) if (is.factor(x)) levels(x) else NULL)

  var_labels <- lapply(data[, unique(c(vars, outcome)), drop = FALSE], Hmisc::label)

  vars_validas <- vars

### --- 2. 'Loop' niveles con alguna celda < 'min_cell' ---
  for (var in vars) {

    if (!is.factor(data[[var]])) next

    tab <- table(data[[var]], data[[outcome]], useNA = "no")

    ### Niveles con alguna celda < 'min_cell'
    bad_levels <- rownames(tab)[apply(tab, 1, function(x) any(x < min_cell))]

    if (length(bad_levels) > 0) {

      for (lvl in bad_levels) {

        min_freq <- min(tab[lvl, ])

        message_ueb(
          paste0(
            "Level '", lvl, "' of variable '", var,
            "' has been excluded because at least one cell of the contingency table with '",
            get_lab_nam(data, outcome),
            "' contains fewer than ", min_cell,
            " observations (minimum cell frequency = ", min_freq, ")."),
          col = "#B22222")
      }

      ### Mantener orden original y de referencia de las variables factor
      data[[var]] <- as.character(data[[var]])
      data[[var]][data[[var]] %in% bad_levels] <- NA
      data[[var]] <- factor(data[[var]], levels = setdiff(original_levels[[var]], bad_levels))
    }

    ### Comprobar cuántos niveles observados quedan
    n_levels <- length(unique(na.omit(data[[var]])))

    if (n_levels < 2) {

      vars_validas <- setdiff(vars_validas, var)

      msg_detail <- if (length(bad_levels) > 0) {
        paste0(
          "after removing sparse levels (", paste(bad_levels, collapse = ", "),
          "), only one category remained")
      } else {
        "fewer than two observed categories remained"
      }

      message_ueb(
        paste0(
          "Variable '", var,
          "' has been excluded from the analysis because ", msg_detail, "."),
        col = "#B22222")
    }
  }

### --- 3. Restauración de etiquetas ---
  for (v in names(var_labels)) {
    if (v %in% names(data)) {
      Hmisc::label(data[[v]]) <- var_labels[[v]]
    }
  }

### --- 4. 'return()' ---
  list(
    data = data,
    vars_validas = vars_validas)
}