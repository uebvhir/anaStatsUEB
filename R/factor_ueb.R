#' factor_ueb Function
#'
#' The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors). Return original label.
#' @param x   a vector of data, usually taking a small number of distinct values.
#' @param levs a vector of the values (as character strings) that x might have taken. The default is the unique set of values taken by as.character(x), sorted into increasing order of x. Note that this set can be specified as smaller than sort(unique(x))
#' @param labs a character vector of labels for the levels (in the same order as levels after removing those in exclude).
#' @param del.empty.val elimina los niveles que tienen 0 casos
#' @param name.var nom de la variable. Per defecte agafa la part dreta del "$" (exemple: mtcars$mpg, utilitza "mpg")
#' @export factor_ueb
#' @import Hmisc
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#' #create variable
#' variable <- c(0,0,0,1,1,1,0,"a",1,0, "B")
#' # automatic factor
#' factor_ueb(x = variable)
#' # assign levels and label factor
#' (var_factor <- factor_ueb(variable, levs = c(0,1,2), labs = c("No", "Si","NSNC")))
#' #reorder factor
#' factor_ueb(x = var_factor, labs = c("Si","No"))
#'
#' var_factor2 <- factor(variable)
#' var_factor2[var_factor2 =="B"] <- NA
#' factor_ueb(var_factor2, labs = c("1", "a","B","0") )
#'
#' ## Other examples
#' variable <- c(0,0,0,1,1,1,0,10,1,0)
#' factor_ueb(variable)
#' factor_ueb(variable, levs = c(0,1), labs = c("No", "Si"))
#'
#' variable <- c(0,0,0,1,1,1,0,1,1,0, NA)
#' factor_ueb(variable)
#' factor_ueb(variable, levs = c(0,1), labs = c("No", "Si"))
#' @return factor returns an object of class "factor" which has a set of integer codes the length of x with a "levels" attribute of mode character and unique (!anyDuplicated(.)) entries. If argument ordered is true (or ordered() is used) the result has class c("ordered", "factor"). Undocumentedly for a long time, factor(x) loses all attributes(x) but "names", and resets "levels" and "class".

#' @keywords factor variable class levels labels

factor_ueb <- function(x, levs = NULL, labs = NULL, del.empty.val = TRUE, name.var = NULL,write_changes = T){

  lab_var <- Hmisc::label(x)
  name_sep <- strsplit(deparse(substitute(x)), "$",fixed = T)[[1]]
  name.var <- ifelse(is.null(name.var), name_sep[length(name_sep)], name.var)
  unique_noNA <- unique(x)[!unique(x) %in% NA]

  if(!is.null(levs) & !is.null(labs)){
    levels_dif <- unique_noNA  %in% levs
    if (!all(levels_dif)) {
      mss <- paste0("Los individuos con valor '", paste(unique_noNA[!levels_dif], collapse = "', '"),
                    "' para la variable ",name.var," han sido considerados NA \n")
      if(write_changes) desc_changes(mss)
      warning(mss, call. = FALSE)

      }
    var_factor <- factor(x, levels = levs, labels = labs )
  } else if (is.null(levs) & !is.null(labs)) {
    var_factor <- factor_reorder(x, labs = labs )
  } else {
    var_factor <- factor(x)
  }


  if (del.empty.val) var_factor <- droplevels(var_factor)
  Hmisc::label(var_factor) <- lab_var
  return(var_factor)
}



factor_reorder <- function(x, labs){

  if(length(labs) != length(levels(x))) stop ("There are different number of 'labs' and 'levels(x)'")
  if(!all (levels(x) %in% labs) ) stop ("'levels(x)' and 'labs' are different")

  lab_var <- Hmisc::label(x) ## guardem label
  var_factor <- factor(x, labs) ## reorder factor
  Hmisc::label(var_factor) <- lab_var # assignem de nou la label original
  if(!all.equal(as.character(x), as.character(var_factor)) ) stop("Houston! We have a problem!  Review code 'factor_reorder")
  return(var_factor)
}
