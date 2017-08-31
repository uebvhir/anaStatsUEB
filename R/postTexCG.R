#' A postTexCG Function
#'
#' Corrige problema con funcion export2latex
#' @param nameFile Character vector of length 1 containing the name of file.
#' @keywords compareGroups export2latex error tex
#' @export postTexCG
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @return Reescribe el fichero eliminando '2*'


postTexCG <- function(nameFile) {
    FileInput <- readLines(nameFile)
    gs_pattern <- "{2}{*}"
    fileoutput <- gsub(gs_pattern, "", FileInput, fixed = T)
    write(fileoutput, file = nameFile)
}
