#' Obtener etiquetas o nombres de variables en un data frame
#'
#' Esta función devuelve las etiquetas (`label`) de una serie de variables en un `data.frame`
#' si existen. Si alguna variable no tiene una etiqueta, se devuelve el nombre de la variable.
#'
#' @param data Un `data.frame` que contiene las variables.
#' @param variable_names Un vector de caracteres con los nombres de las variables que se desean consultar.
#'
#' @return Un vector de caracteres con las etiquetas de las variables si están disponibles,
#' o los nombres de las variables si no tienen etiqueta asignada.
#'
#' @examples
#' library(Hmisc)
#' data <- data.frame(id = 1:3, sexe = c("M", "F", "F"))
#' label(data$id) <- "Identificador del Paciente"
#' get_lab_nam(data, c("id", "sexe"))
#' # Resultado: "Identificador del Paciente" "sexe"
#'
#' @importFrom Hmisc label
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
