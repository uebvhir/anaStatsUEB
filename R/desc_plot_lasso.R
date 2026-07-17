#' Representación gráfica de la trayectoria de regularización LASSO
#'
#' @description
#' Genera una representación gráfica de la trayectoria de coeficientes de un
#' modelo LASSO ajustado previamente mediante \code{desc_lasso()}.
#'
#' La función muestra la evolución de los coeficientes a medida que varía el
#' parámetro de penalización \eqn{\lambda}. Además, añade una línea vertical
#' indicando el valor de \eqn{\lambda} seleccionado por \code{desc_lasso()},
#' permitiendo identificar visualmente el punto de selección de variables.
#'
#' Cuando el paquete \pkg{plotmo} está disponible, se utiliza
#' \code{plotmo::plot_glmnet()} para mejorar la visualización y etiquetar las
#' trayectorias de los coeficientes. En caso contrario, se utiliza la función
#' gráfica estándar de \pkg{glmnet}.
#'
#' @param res objeto generado por \code{desc_lasso()}.
#' Debe contener como mínimo los elementos:
#' \itemize{
#'   \item \code{model}: objeto \code{glmnet}.
#'   \item \code{lambda}: valor de \eqn{\lambda} seleccionado.
#' }
#' @param nresponse respuesta a representar en modelos multinomiales.
#' Puede indicarse mediante índice numérico o mediante el nivel correspondiente
#' según la implementación de \code{plotmo::plot_glmnet()}.
#' Por defecto \code{1}.
#'
#' @return
#' No devuelve ningún objeto. La función genera una representación gráfica
#' de la trayectoria de regularización LASSO.
#'
#' @details
#' Esta función está diseñada como complemento de \code{desc_lasso()} y utiliza
#' directamente el modelo almacenado en \code{res$model}.
#'
#' La línea vertical discontinua de color púrpura indica el valor de
#' \eqn{\log(\lambda)} finalmente seleccionado durante el proceso de ajuste.
#'
#' En modelos multinomiales, el argumento \code{nresponse} permite seleccionar
#' qué categoría de respuesta debe visualizarse.
#'
#' \strong{Limitaciones}
#'
#' \itemize{
#'   \item Solo acepta objetos creados por \code{desc_lasso()}.
#'   \item En modelos multinomiales únicamente se representa una categoría de
#'   respuesta cada vez.
#'   \item La visualización depende de la disponibilidad del paquete
#'   \pkg{plotmo}. Si no está instalado se utilizará la representación estándar
#'   de \pkg{glmnet}, con menor capacidad de etiquetado.
#'   \item La función tiene finalidad descriptiva y no realiza ninguna selección
#'   adicional de variables ni recalcula el modelo.
#' }
#'
#' @examples
#' \dontrun{
#' # Ajustar modelo LASSO
#' res <- desc_lasso(
#'   var_out = "Species",
#'   var_comp = c(
#'     "Sepal.Length",
#'     "Sepal.Width",
#'     "Petal.Length",
#'     "Petal.Width"
#'   ),
#'   data = iris,
#'   show.plot = FALSE
#' )
#'
#' # Mostrar trayectoria de regularización
#' desc_plot_lasso(res)
#'
#' # Visualizar segunda categoría en modelo multinomial
#' desc_plot_lasso(res, nresponse = 2)
#' }
#'
#' @seealso
#' \code{\link{desc_lasso}}
#'
#' @keywords lasso glmnet regularization variable-selection prediction
#'
#' @author
#' Miquel Vazquez
#'
#' @export desc_plot_lasso
desc_plot_lasso <- function(res, nresponse = 1) {

  fit <- res$model
  lambda <- res$lambda

  if (!requireNamespace("plotmo", quietly = TRUE)) {

    plot(fit, xvar = "lambda", label = TRUE)

  } else {

    plotmo::plot_glmnet(
      fit,
      xvar = "lambda",
      label = TRUE,
      s = lambda,
      nresponse = nresponse
    )
  }

  abline(v = log(lambda), col = "#993489", lty = 2)
}
