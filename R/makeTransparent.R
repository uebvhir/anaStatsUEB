#' A makeTransparent Function
#'
#' Creación de colores transparentes
#' ROC(representación gráfica de la sensibilidad en frente a la especificidad).
#' @param color nombre del color en formato R
#' @param alpha nivel de translucido
#' @export makeTransparent
#' @examples
#' makeTransparent('mediumorchid4')
#' @keywords color transparent


makeTransparent <- function(...,
                            alpha = 0.5) {

    if (alpha < 0 | alpha > 1)
      stop("alpha must be between 0 and 1")

    alpha <- floor(255 * alpha)
    newColor <- col2rgb(col = unlist(list(...)), alpha = FALSE)

    .makeTransparent <- function(col, alpha) {
        rgb(red = col[1],
            green = col[2],
            blue = col[3],
            alpha = alpha,
            maxColorValue = 255)
    }

    newColor <- apply(newColor, 2, .makeTransparent, alpha = alpha)
    return(newColor)

}
