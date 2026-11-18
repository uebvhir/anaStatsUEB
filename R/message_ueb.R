#' @title Mostrar un mensaje en color (HTML)
#'
#' @description
#' Imprime un mensaje con \code{cat()} dentro de una etiqueta HTML \code{<span>}
#' con el color indicado. Sirve para destacar avisos y cambios aplicados a los
#' datos en informes de R Markdown.
#'
#' @param x Cadena de texto (o vector de cadenas) con el mensaje a mostrar.
#' @param col Color del texto, escrito tal cual en el estilo CSS (por ejemplo,
#'   \code{"purple"} o \code{"#B22222"}). Por defecto, \code{"blue"}.
#' @param ls Lógico. Si es \code{TRUE}, antepone \code{" * "} al mensaje, de modo
#'   que se interprete como elemento de una lista de Markdown. Por defecto,
#'   \code{FALSE}.
#'
#' @return
#' \code{NULL}, de forma invisible. El mensaje se imprime por la salida estándar.
#'
#' @details
#' La salida tiene la forma
#' \code{<span style='color:col'>x</span>}, precedida y seguida de saltos de línea.
#' Para que se interprete como HTML en un informe, el chunk debe usar
#' \code{results = 'asis'}. El texto no se escapa y el color no se valida. Si
#' \code{x} es un vector, cada elemento se imprime con su propia etiqueta.
#'
#' A diferencia de \code{\link{desc_changes}}, esta función no guarda el
#' mensaje: solo lo imprime.
#'
#' @seealso \code{\link{desc_changes}}, \code{\link{filter_sparse_levels}}
#'
#' @examples
#' message_ueb("se ha realizado un cambio en el nombre de la variable", col = "purple")
#'
#' # Como elemento de una lista de Markdown
#' message_ueb("variable recodificada", ls = TRUE)
#'
#' # Con un color hexadecimal
#' message_ueb("nivel eliminado", col = "#B22222")
#'
#' @keywords message report changes
#'
#' @author
#' Miriam Mota \email{miriam.mota@@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
message_ueb <- function(x, col = "blue", ls = FALSE){
  x <- paste0("\n" ,ifelse(ls," * ", ""), "<span style='color:",col,"'>",x,"</span> \n ")
  return(cat(x))
}
