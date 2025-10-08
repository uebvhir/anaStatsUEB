#' @title Descriptive Summary Table of a Data Frame
#' @description
#' Generates a summary table of a dataset with key metadata: variable types, labels, number of levels (for factors),
#' value ranges (for numerics), and missing data. Optionally highlights problematic variables based on level count or missing values.
#'
#' @param data A data frame to summarize.
#' @param format Output format: "html" or "latex". Used for line breaks and rendering style. Default is "html".
#' @param maxlev Maximum number of levels to display for factor variables (default = 7).
#' @param maxNA Threshold (in %) to highlight variables with too many missing values (default = 80).
#' @param size Font size in output (for `kable` rendering, default = 13).
#' @param caption Optional caption text. If `NULL`, a default is generated with variable and observation counts.
#' @param remove_cols Logical. If `TRUE`, removes empty columns before processing (default = TRUE).
#' @param df Logical. If `TRUE`, returns the summary as a data frame. If `FALSE`, returns a formatted table.
#'
#' @return A `kable`-styled summary table or a data frame, depending on `df`.
#' @importFrom Hmisc label
#' @importFrom kableExtra cell_spec kable kable_styling
#' @importFrom janitor remove_empty
#' @export
#' @author
#' Miriam Mota-Foix <mmota.foix@gmail.com>
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
    switch(class_x,
           logical = paste(unique(x), collapse = ", "),
           factor = paste("N LEVELS =", length(levels(x)), new_line,
                          paste(1:min(maxlev, length(levels(x))),
                                ":", levels(x)[1:min(maxlev, length(levels(x)))],
                                collapse = new_line)),
           numeric = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           integer = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           Date = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           POSIXt = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           character = "-")
  }))

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
    kable(format = format, booktabs = T,  col.names = colnames, caption = caption, longtable = TRUE, escape = F, row.names= FALSE) %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = size, full_width = F, position = "left")
  }else{
      return(df_res)
    }
}
