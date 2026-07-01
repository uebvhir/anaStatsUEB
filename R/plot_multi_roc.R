#' Graficar múltiples curvas ROC de modelos
#'
#' Esta función genera un gráfico que muestra las curvas ROC (Receiver Operating Characteristic)
#' para varios modelos, comparándolos en un solo gráfico. Además, muestra los valores AUC (Área Bajo la Curva)
#' para cada modelo en la leyenda y proporciona una indicación de los niveles de control y caso en el conjunto de datos.
#'
#' @param modelos Una lista de modelos de predicción (por ejemplo, modelos ajustados de regresión logística,
#' árboles de decisión (pendiente de hacer pruebas), etc.). Cada elemento de la lista debe tener un nombre que será utilizado en la leyenda del gráfico.
#' @param datos Un data frame que contiene los datos que se usarán para hacer las predicciones. El data frame debe incluir
#' una columna con las etiquetas de la variable de respuesta (outcome).
#' @param outcome El nombre de la columna en `datos` que representa la variable dependiente (la variable de clase binaria: caso/control).
#' @param colores Un vector de caracteres (opcional) con los colores a utilizar para graficar las curvas ROC de cada modelo. Si no se especifica,
#' se generarán colores predeterminados automáticamente.
#'
#' @details
#' 1. Si no se proporcionan colores, la función asigna un conjunto de colores predeterminados utilizando la función `gg_color()`.
#' 2. Para cada modelo, se predicen las probabilidades utilizando `predict()`, y luego se calcula la curva ROC con la función `roc()` de la librería `pROC`.
#' 3. Las curvas ROC se grafican en el mismo espacio de coordenadas y se añaden sucesivamente a la gráfica.
#' 4. La leyenda muestra los nombres de los modelos junto con sus valores AUC.
#' 5. Se incluye texto adicional que indica los niveles de "control" y "caso" en el gráfico.
#'
#' @return No retorna nada. La función genera un gráfico de las curvas ROC en el dispositivo gráfico de R.
#'
#' @examples
#' # Cargar librerías necesarias
#' # library(pROC)
#'
#' # Crear algunos modelos (por ejemplo, regresión logística y un árbol de decisión)
#' # modelo1 <- glm(outcome ~ ., data = datos_train, family = binomial)
#' # modelo2 <- rpart(outcome ~ ., data = datos_train, method = "class")
#'
#' # Graficar múltiples curvas ROC
#' # plot_multiple_roc(
#'  #  modelos = list(Modelo_Logistico = modelo1, Modelo_Arbol = modelo2),
#'  #  datos = datos_test,
#'  #  outcome = "outcome"
#'  # )
#'
#' @importFrom pROC roc auc
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
