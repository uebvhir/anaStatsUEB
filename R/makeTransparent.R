#' @title Crear colores transparentes
#'
#' @description
#' Convierte uno o varios colores de R en colores con transparencia, devolviendo
#' su código hexadecimal con canal alfa.
#'
#' @param ... Uno o varios colores, como nombres de color de R (por ejemplo,
#'   \code{"red"}), códigos hexadecimales (\code{"#FF0000"}) o índices numéricos
#'   de la paleta. Se combinan todos los indicados en un único vector.
#' @param alpha Valor numérico único entre 0 (totalmente transparente) y 1
#'   (opaco) que indica el nivel de opacidad. Por defecto, 0.5.
#'
#' @return Vector de caracteres con un código hexadecimal \code{"#RRGGBBAA"} por
#'   cada color indicado.
#'
#' @details
#' El canal alfa se calcula como \code{floor(255 * alpha)}; por ejemplo,
#' \code{alpha = 0.5} da \code{"7F"}. Un \code{alpha} fuera del intervalo
#' \eqn{[0, 1]} produce un error, igual que un nombre de color no válido. La
#' función solo admite un valor de \code{alpha}, que se aplica a todos los
#' colores.
#'
#' @seealso \code{\link[grDevices]{rgb}}, \code{\link[grDevices]{col2rgb}},
#'   \code{\link[grDevices]{adjustcolor}}
#'
#' @examples
#' makeTransparent("mediumorchid4")
#'
#' # Varios colores a la vez, con 20\% de opacidad
#' makeTransparent("red", "blue", alpha = 0.2)
#'
#' # Combinando vectores y codigos hexadecimales
#' makeTransparent(c("red", "blue"), "#00FF00")
#'
#' @keywords color transparent alpha
#'
#' @importFrom grDevices col2rgb rgb
#'
#' @author
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
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
