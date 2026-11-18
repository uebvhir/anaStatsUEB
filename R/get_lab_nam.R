#' @title Obtener etiquetas o nombres de variables de un data frame
#'
#' @description
#' Devuelve las etiquetas (\code{label}, del paquete \pkg{Hmisc}) de un conjunto
#' de variables de un \code{data.frame}, si existen. Si alguna variable no tiene
#' etiqueta, se devuelve su nombre.
#'
#' @param data \code{data.frame} que contiene las variables. Debe ser el data
#'   frame completo, no una columna.
#' @param variable_names Vector de caracteres con los nombres de las variables
#'   que se desea consultar.
#'
#' @return Vector de caracteres con nombres (los de \code{variable_names}) que
#'   contiene la etiqueta de cada variable o, si no tiene etiqueta, el nombre de
#'   la variable.
#'
#' @details
#' Una etiqueta vacía (\code{""}) se trata como si la variable no tuviera
#' etiqueta. Si un nombre de \code{variable_names} no existe en \code{data}, no
#' se produce ningún error y se devuelve el propio nombre. Si
#' \code{variable_names} es un vector vacío, se devuelve una lista vacía con
#' nombres.
#'
#' @seealso \code{\link[Hmisc]{label}}, \code{\link{desc_unimods}},
#'   \code{\link{desc_unimods_multi}}, \code{\link{filter_sparse_levels}}
#'
#' @examples
#' data <- data.frame(id = 1:3, sexe = c("M", "F", "F"))
#' Hmisc::label(data$id) <- "Identificador del Paciente"
#'
#' get_lab_nam(data, c("id", "sexe"))
#' # id: "Identificador del Paciente"; sexe: "sexe"
#'
#' # Una variable que no existe devuelve su propio nombre
#' get_lab_nam(data, "otra")
#'
#' @keywords labels variables data-frame Hmisc
#'
#' @importFrom Hmisc label
#'
#' @author
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
get_lab_nam <- function(data, variable_names) {
  # Aplicar la lógica a cada variable en la lista
  labels <- sapply(variable_names, function(var) {
    # Verificar si existe una etiqueta para la variable
    label_value <- label(data[[var]])

    # Si existe una etiqueta, devolverla; si no, devolver el nombre de la variable
    if (!is.null(label_value) && label_value != "") {
      return(label_value)  # Devuelve la etiqueta si existe
    } else {
      return(var)  # Devuelve el nombre de la variable si no tiene etiqueta
    }
  })

  return(labels)
}
