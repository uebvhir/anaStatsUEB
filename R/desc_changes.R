#' desc_changes Function
#'
#' The function desc_changes is used to save messages of changes applied in database
#' @param x   a string
#' @export desc_changes
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' desc_changes("")
#'
#' @return list_changes list with messages to report.
#' var_sino <- c("Fum","alcohol")
#' desc_changes(paste0("Los valores, 0 y 1 se han considerado 'No' y 'Si' respectivamente para las variables: ", paste(var_sino,collapse = ", ") ))
#' desc_changes("Los valores superiores a 10000 para la variable n_il6_f han sido considerados NA ")
#' list_changes
#' #With results = 'asis' rmarkdown chunk
#' for(i in seq_along(list_changes)){
#' cat(list_changes[[i]])
#' }

#' @keywords message report changes


desc_changes <- function(x, col = "blue"){
  x <- paste("\n * <span style='color:",col,"'>",x,"</span> \n")
  if(!exists("list_changes")){
    list_changes <<- list()
    i = 0
  }else{
    i <- length(list_changes)
  }
  list_changes[[i+1]] <<- x
  colors <- sapply(list_changes, extract_color)
  list_changes <- list_changes[order(match(colors, unique(colors)), na.last = TRUE)]
}


extract_color <- function(text) {
  matches <- regmatches(text, regexpr("color: #[0-9a-fA-F]+|color: [a-zA-Z]+", text))
  if (length(matches) > 0) {
    return(matches)
  } else {
    return(NA)
  }
}
