#' A desc_data Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param data data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.
#' @param format a character string; possible values are latex, html, markdown, pandoc, and rst; this will be automatically determined if the function is called within knitr; it can also be set in the global option knitr.table.format; if format is a function, it must return a character string
#' @param size A numeric input for table font size
#' @param maxlev an integer indicating the maximum number of levels of variable. Default value is 7.
#' @param maxNA an integer indicating the maximum number of missing data.
#' @param caption Character vector containing the table's caption or title. Default value is a summary.
#' @param remove_cols logical value. Removes all columns from a data that are composed entirely of NA values.
#' @param df logical value. return data.frame
#' @keywords read clean data summary depurate
#' @export desc_data
#' @import kableExtra Hmisc dplyr janitor
#' @examples
#' # desc_data(data = iris, format = "html")
#' # desc_data(airquality, format = "html", maxNA = 20)





desc_data <- function(data,
                format = "html",
                maxlev = 7,
                maxNA = 80,
                size = 13,
                caption = NULL,
                remove_cols = TRUE,
                df = FALSE) {

  new_line <- switch(format, "html" = " <br> ", "latex" = " \\\\ " ) #, "R" = " \n ")
  caption <- paste0("Summary Data.", new_line,
                    "N variables: ", ncol(data), ".  N observaciones: ", nrow(data), new_line,
                    "Max level: ", maxlev, ".  Max NA: ", maxNA, "%")

  ## eliminem columnes buides
  data <- remove_empty(data, which = "cols")

  ## creacio de les diferents columnes
  nms <- names(data)

  clss <- sapply(data, function(x) class(x)[length(class(x))])

  lbl <- Hmisc::label(data,self = F)

  mm_lev <- unlist(lapply(data, function(x) {
    class_x <- class(x)[length(class(x))]
    switch(class_x ,
           "logical" = unique(x),
           "factor" = paste("N LEVELS =", length(levels(x)), new_line,
                            paste(1:min(maxlev, length(levels(x))),":",levels(x)[1:min(maxlev, length(levels(x)))], collapse = new_line)),
           "numeric" = paste0( "[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           "integer" = paste0( "[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           "Date" = paste0( "[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           "POSIXt" = paste0( "[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           "character" = "-") }))

  compl_mis <- apply(data,2,function(x) paste0(sum(complete.cases(x))," (", round((sum(complete.cases(x))/length(x))*100,3) , "%)", new_line,
                                               sum(is.na(x))," (", round((sum(is.na(x))/length(x))*100,3) , "%)"))

  #informacio necesaria per a resaltar
  nlev <- sapply(data, function(x)length(levels(x) ))
  percNA <- apply(data,2,function(x)sum(is.na(x))/length(x)*100 )


  # Creacio data frame final
  df_res <- as.data.frame( cbind(nms,clss,lbl, mm_lev, compl_mis))
  colnames <- c("Variable",
                      "Type",
                      "Description",
                      "[Min,Max] or Levels",
                      paste0("Valid cases, n (%)",new_line," Missings, n (%)"))


  # Eliminació de columna en cas de que no hi hagi labels
  if (length(table(df_res$lbl)) == 1) {
    df_res <- df_res[,-which(names(df_res) %in% "lbl")]
    colnames <- colnames[!colnames %in% "Description"]
  }

  # Informació Final
  if(!df) {
    df_res  %>%
    mutate(
      mm_lev = cell_spec(mm_lev, color = ifelse(nlev > maxlev, "red", "black"), escape = F),
      compl_mis = cell_spec(compl_mis, color = ifelse(percNA > maxNA, "red", "black"), escape = F)
    ) %>%
    kable(format = format, booktabs = T,  col.names = colnames, caption = caption, longtable = TRUE, escape = F) %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = size, full_width = F, position = "left")
  }else{
      return(df_res)
    }
}
