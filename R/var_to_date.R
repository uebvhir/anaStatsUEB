#' var_to_num Function
#'
#' The function var_to_num is used to convert a character variable to numeric variable. Change ',' by '.'. Delete spaces. Return a numeric variable and a warning with the delete values.
#' @param x   a character variable with dates
#' @param
#' @param name.var a string with variable name
#' @export var_to_num
#' @author Miriam Mota  \email{miriam.mota@@vhir.org}
#' @examples
#'
#' @return a POSIXct variable and a warning with delete values.
#' date1 <- c("09-01-01", "09-01-02", "09-01-03", "30/02/2020", "exitus", "2024-05-12")
#' Hmisc::label(date1) <- "Fecha ingreso"
#' date1_tcd <- var_to_date(date1, formats = c("%d-%m-%y", "%d/%m/%Y", "Y%-%m-%d"))
#' date1_tcd
#' Hmisc::label(date1_tcd)
#'
#' #' @keywords numeric character class change warnings
#' #'
#' #'
#' var_to_date
#'
#' my_select <-   function(trained, drop=FALSE, ...){
#'   n_fmts <- nchar(gsub("[^%]", "", names(trained))) + grepl("%y", names(trained))*1.5
#'   names(trained[ which.max(n_fmts) ])
#' }
#'
#' var_to_date <- function(x, formats = NULL, name.var = NULL){
#'
#'
#'   # preprocessat
#'   lbl <- Hmisc::label(x)
#'   x <- as.character(x)
#'   id_naFecha <- is.na(x)
#'   var_or <- x
#'   x <- gsub("/","-", x, fixed = T)
#'
#'   # canvi de formats
#'   date_format <- parse_date_time2(x = x, formats, exact = T)
#'   date_excel <- as.Date(as.numeric(as.character(x)), origin = "1899-12-30")
#'   if (!all(complete.cases(date_format) & complete.cases(date_excel))) {x <- pmin(date_format, date_excel, na.rm = TRUE)}
#'
#'   # neteja d'individus
#'   id_rem <- is.na(x) != id_naFecha| x > today() |  x < "1925-01-01"
#'   val <- unique(var_or[which(id_rem)])
#'   x[id_rem] <- NA
#'
#'   if(length(val>0)){
#'     name.var <- ifelse(!is.null(name.var), name.var,"")
#'     mss <- paste0(" \n *  Los individuos con valor  '",
#'                   paste0(val, collapse = ", "),"' para la variable ",name.var, " han sido eliminados.  \n ")
#'     desc_changes(mss)
#'     warning(mss)
#'   }
#'   Hmisc::label(x) <- lbl
#'
#'   return(x)
#'
#' }
