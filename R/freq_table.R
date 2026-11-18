#' @title Tabla de frecuencias relativas de variables categóricas
#'
#' @description
#' Genera una tabla de frecuencias absolutas y relativas (porcentajes) de una o
#' varias variables categóricas según los niveles de una variable factor
#' principal. Los porcentajes pueden calcularse sobre el total, por filas o por
#' columnas.
#'
#' @param data Data frame que contiene las variables a analizar. Debe ser un
#'   \code{data.frame} (no un tibble).
#' @param var.cat Vector de caracteres con el nombre de las variables categóricas
#'   a resumir. Cada una se convierte internamente en factor.
#' @param y Nombre de la variable factor principal, que define las columnas de la
#'   tabla. Se convierte internamente en factor.
#' @param margin Índice de la tabla de contingencia sobre el que se calculan los
#'   porcentajes: \code{NULL} (por defecto) para el total, \code{1} para cada fila
#'   y \code{2} para cada columna. Se pasa a \code{\link[base]{prop.table}}.
#' @param nround Número entero de decimales con los que se redondean los
#'   porcentajes. Por defecto, 2.
#' @param restab Lógico. Si es \code{TRUE} (por defecto), se devuelve la tabla con
#'   formato mediante \code{\link{kable_ueb}}. Si es \code{FALSE}, se devuelve la
#'   matriz de texto.
#' @param title Título de la tabla (solo si \code{restab = TRUE}). Por defecto,
#'   \code{"titles for the table"}.
#' @param xtab.type Cadena de texto con el formato de la tabla (\code{"latex"},
#'   \code{"html"}, \code{"markdown"}, \code{"pandoc"} o \code{"rst"}). Por
#'   defecto, \code{"html"}. Se pasa como \code{format} a \code{kable_ueb()}.
#'
#' @return
#' Si \code{restab = TRUE}, una tabla con formato (objeto \code{kableExtra}). Si
#' \code{restab = FALSE}, una matriz de texto con las columnas \code{Variable},
#' \code{Levels} y una columna por cada nivel de \code{y}, cuyas celdas tienen el
#' formato \dQuote{n (porcentaje\%)}. Cada variable va precedida de una fila vacía.
#'
#' @details
#' Para cada variable de \code{var.cat} se calcula la tabla de frecuencias
#' absolutas frente a \code{y} y se obtiene el porcentaje con
#' \code{prop.table()} según \code{margin}. La primera fila de cada bloque
#' contiene el nombre de la variable y las siguientes sus niveles.
#'
#' Nota: en la versión actual, con \code{restab = TRUE} la llamada a
#' \code{kable_ueb()} recibe \code{format} por duplicado y produce un error. Hasta
#' que se corrija, solo funciona \code{restab = FALSE}.
#'
#' @seealso \code{\link{kable_ueb}}, \code{\link{desc_group}},
#'   \code{\link[base]{table}}, \code{\link[base]{prop.table}}
#'
#' @examples
#' # Porcentajes sobre el total
#' freq_table(var.cat = c("cyl", "vs"), y = "am", data = mtcars, restab = FALSE)
#'
#' # Porcentajes por filas
#' freq_table(var.cat = "cyl", y = "am", data = mtcars, margin = 1, restab = FALSE)
#'
#' # Porcentajes por columnas, sin decimales
#' freq_table(var.cat = "cyl", y = "am", data = mtcars, margin = 2,
#'            nround = 0, restab = FALSE)
#'
#' @keywords freq tables compare qualitative
#'
#' @import stargazer
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
freq_table <- function(var.cat, y,
                       data, margin = NULL,
                       nround = 2,
                       restab = TRUE,
                       title = "titles for the table",
                       xtab.type = "html") {

  data[, y] <- factor(data[, y])
  res_freq_all <- NULL
  for (i in 1:length(var.cat)) {
    data[, var.cat[i]] <- factor(data[, var.cat[i]])
    (freq_abs <- table(data[, var.cat[i]], data[, y]))
    (freq_rel <- prop.table(freq_abs, margin = margin) * 100)
    res_freq <- matrix(paste0(freq_abs, " (", round(freq_rel, nround), "%)"),
                       ncol = ncol(freq_abs))
    res_freq <- cbind(c(var.cat[i], rep("", nrow(res_freq) - 1)),
                      rownames(freq_abs),
                      res_freq)
    colnames(res_freq) <- c("Variable", "Levels", colnames(freq_abs))
    res_freq_all <- rbind(res_freq_all, rep("", ncol(res_freq)), res_freq)
  }
  if (restab) {
    kable_ueb(res_freq_all, format = xtab.type, booktabs = T,
          caption = title)
  } else {
    return(res_freq_all)
  }
}
