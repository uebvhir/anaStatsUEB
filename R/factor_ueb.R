#' factor_ueb Function
#'
#' The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors).
#' @param x   a vector of data, usually taking a small number of distinct values.
#' @param levels an optional vector of the values (as character strings) that x might have taken. The default is the unique set of values taken by as.character(x), sorted into increasing order of x. Note that this set can be specified as smaller than sort(unique(x))
#' @param labels either an optional character vector of (unique) labels for the levels (in the same order as levels after removing those in exclude), or a character string of length 1.
#' @param del.empty.val elimina los niveles que tienen 0 casos
#' @export factor_ueb
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' variable <- c(0,0,0,1,1,1,0,"a",1,0)
#' factor_ueb(variable, levels = c(0,1), labels = c("No", "Si"))
#'
#' variable <- c(0,0,0,1,1,1,0,10,1,0)
#' factor_ueb(variable, levels = c(0,1), labels = c("No", "Si"))
#'
#' variable <- c(0,0,0,1,1,1,0,1,1,0)
#' factor_ueb(variable, levels = c(0,1), labels = c("No", "Si"))
#' @return factor returns an object of class "factor" which has a set of integer codes the length of x with a "levels" attribute of mode character and unique (!anyDuplicated(.)) entries. If argument ordered is true (or ordered() is used) the result has class c("ordered", "factor"). Undocumentedly for a long time, factor(x) loses all attributes(x) but "names", and resets "levels" and "class".

#' @keywords factor variable class levels labels


factor_ueb <- function(x, levels, labels, del.empty.val = T){
  levels_dif <- unique(x) %in% levels
  if (!all(levels_dif)) { warning("Los individuos con valor '", unique(x)[!levels_dif],
                                 "' han sido considerados NA \n", call. = FALSE) }
  var_factor <- factor(x, levels = levels, labels = labels )
  if (del.empty.val) var_factor <- factor(var_factor)
  return(var_factor)
}
