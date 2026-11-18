#' @title Graficar múltiples curvas ROC de modelos
#'
#' @description
#' Genera un gráfico que muestra las curvas ROC (Receiver Operating
#' Characteristic) de varios modelos, comparándolos en un solo gráfico. Además,
#' muestra el valor del AUC (área bajo la curva) de cada modelo en la leyenda e
#' indica los niveles de control y caso de la variable de respuesta.
#'
#' @param modelos Lista con nombres de modelos de predicción (por ejemplo, modelos
#'   de regresión logística ajustados con \code{glm}; otros tipos, como árboles de
#'   decisión, están pendientes de probar). Cada elemento debe tener un nombre, que
#'   se utiliza en la leyenda del gráfico.
#' @param datos Data frame con los datos que se usan para hacer las predicciones.
#'   Debe incluir la columna de la variable de respuesta indicada en
#'   \code{outcome}.
#' @param outcome Nombre de la columna de \code{datos} que representa la variable
#'   dependiente (variable binaria de clase: control/caso).
#' @param colores Vector de caracteres (opcional) con los colores de las curvas
#'   ROC, uno por modelo. Si es \code{NULL} (por defecto), se generan colores con
#'   \code{\link{gg_color}}.
#' @param text.xlim Coordenada X del texto que indica los niveles de control y
#'   caso, en unidades del gráfico. Por defecto, 0.15.
#' @param text.ylim Coordenada Y del texto que indica los niveles de control y
#'   caso, en unidades del gráfico. Por defecto, 0.7.
#'
#' @return
#' \code{NULL}, de forma invisible. La función genera un gráfico de curvas ROC en
#' el dispositivo gráfico activo de R.
#'
#' @details
#' \enumerate{
#'   \item Si no se indican colores, se asigna un conjunto con
#'     \code{gg_color()}.
#'   \item Para cada modelo se predicen las probabilidades con
#'     \code{predict(modelo, newdata = datos, type = "response")} y se calcula la
#'     curva ROC con \code{pROC::roc()}.
#'   \item Las curvas se dibujan en el mismo espacio de coordenadas y se añaden
#'     sucesivamente al gráfico.
#'   \item La leyenda (abajo a la derecha) muestra el nombre de cada modelo y su
#'     AUC, redondeado a dos decimales.
#'   \item Se añade un texto con los niveles de control y de caso, tomados de la
#'     última curva ROC calculada.
#' }
#'
#' @seealso \code{\link[pROC]{roc}}, \code{\link[pROC]{plot.roc}},
#'   \code{\link{doROC}}, \code{\link{diag.test}}, \code{\link{gg_color}}
#'
#' @examples
#' dat <- mtcars
#' dat$am <- factor(dat$am, labels = c("Manual", "Auto"))
#'
#' m1 <- glm(am ~ mpg, data = dat, family = binomial)
#' m2 <- glm(am ~ mpg + wt, data = dat, family = binomial)
#'
#' plot_multi_roc(
#'   modelos = list(Simple = m1, Ampliado = m2),
#'   datos = dat,
#'   outcome = "am"
#' )
#'
#' @keywords roc auc model-comparison plot
#'
#' @importFrom pROC roc auc
#'
#' @author
#' Marcos Esteve \email{marcos.esteve@@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
plot_multi_roc <- function(modelos, datos, outcome, colores = NULL, text.xlim = 0.15, text.ylim = 0.7 ) {

  # Verificar si se proporcionan colores o asignarlos automáticamente
  if (is.null(colores)) {
    colores <- gg_color(length(modelos))  # Colores por defecto si no se proporcionan
  }

  # Inicializar el índice para la leyenda
  legend_text <- c()
  roc_modelo <- list()
  # Graficar la primera curva
  for (i in seq_along(modelos)) {
    nombre <- names(modelos)[i]
    modelo <- modelos[[i]]

    # Predecir probabilidades
    prob <- predict(modelo, newdata = datos, type = "response")

    # Calcular curva ROC
    roc_modelo[[i]] <- roc(datos[[outcome]], prob)

    # Graficar
    if (i == 1) {
      plot(roc_modelo[[i]], col = colores[i], lwd = 2, main = "Curvas ROC de múltiples modelos")
    } else {
      plot(roc_modelo[[i]], col = colores[i], lwd = 2, add = TRUE)
    }

    # Texto de leyenda
    legend_text <- c(legend_text, paste0(nombre, " (AUC = ", round(auc(roc_modelo[[i]]), 2), ")"))
  }

  # Añadir leyenda al gráfico
  legend("bottomright", legend = legend_text, col = colores, lwd = 2, bg = "white")
  # Añadir texto con los niveles de control y caso
  text(text.xlim, text.ylim, paste0("controls: ", roc_modelo[[i]]$levels[1], "\n cases: ", roc_modelo[[i]]$levels[2]), cex = 1)

}
