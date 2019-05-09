#' A desc_group Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param covariates a character string with names of variables.
#' @param group factor variable. Outcome. Default value is NULL
#' @param data data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.
#' @param method character string indicating the method to test use; possible values are 'param' or 'nonparam'. Default values is 'param'.
#' @param font_size A numeric input for table font size
#' @param byrow logical or NA. Percentage of categorical variables must be reported by rows (TRUE), by columns (FALSE) or by columns and rows to sum up 1 (NA). Default value is FALSE, which means that percentages are reported by columns (withing groups).
#' @param width_lev defines the maximum width of table columns. Default value is 8em
#' @param pval_cut cut p.value colored
#' @keywords summary ci qualitative descriptive exploratory
#' @export desc_group
#' @import kableExtra
#' @examples
#'  # set.seed(1)
#'  # data <- df <- data.frame(MUT = factor(c(rep("A", 12),rep("B",13))),
#'  #                          var = factor(sample(c("Yes", "no"), 25, replace = T)),
#'  #                          size = rnorm(25),
#'  #                          id = paste0("a",1:25))
#'  # data$id <- as.character(data$id)
#'  # desc_group(group  = "MUT",covariates = c("var","size","id"), data = data)


desc_group <- function(...) {
  .Deprecated("descGroup") #include a package argument, too
  descGroup(...)
}

desc_group <- function(covariates,
                      group = NULL,
                      data,
                      method = "non-param",
                      font_size = 11,
                      width_lev = "8em",
                      byrow = FALSE,
                      pval_cut = 0.05, ...){

  ## Seleccionem variables i etiquetes
  data <- data[,names(data) %in% c(covariates,group)]
  if (!is.null(group)) {
    data[,group] <- factor(data[,group])
    varname_group <- ifelse( Hmisc::label(data[,group]) != "", Hmisc::label(data[,group]), group)
  }

  ## guardem class de cada variable
  class_data <- unlist(lapply(data, function(x) class(x)[length(class(x))]))
  class_data[which(class_data == "numeric" | class_data == "integer")] <- "numeric"
  class_data <- class_data[!names(class_data) %in% group]

  ## realitzem analisis descriptiu i/o comparatiu
  list_var <- list()
  for (i in seq_along(class_data)) {
    list_var[[names(class_data)[i]]] <- switch(class_data[i],
                                               "numeric" = summary.quanti( x = names(class_data)[i] , group = group ,
                                                                           method = method, data = data, prep2sum = TRUE,... ) ,
                                               "factor" = summary.quali( x = names(class_data)[i], group = group ,data = data, byrow = byrow, ...),
    )
  }
  list_var_sum <- lapply(list_var, function(x)x[["summary"]])
  results <- do.call("rbind", list_var_sum)

  pvalues <- unlist(lapply(list_var, function(x)x[["pval"]]))

  ## Caption de la taula final
  footnote <- NULL
  typevar <- c("factor", "numeric")
  for (i in seq_along(typevar)) {
    desc <- unique(lapply(list_var[names(list_var) %in% names(class_data)[class_data == typevar[i]] ], function(x)x[["methods"]]))
    tst <- paste0(unique(lapply(list_var[names(list_var) %in% names(class_data)[class_data == typevar[i]]], function(x)x[["txt_test"]])), collapse = "")
    footnote <- unique(gsub("NULL","",c(footnote, paste0(desc, tst))))
  }


  # footnote <-  do.call("cbind", unique(list_var_met))
  caption <- ifelse(is.null(group),
                    "Summary statistics table",
                    paste0("Summary of results by groups of ", varname_group))

  ## CREACIO DE LA TAULA FINAL
  # variables per files
  var <- sapply(strsplit(rownames(results), ".", fixed = T),"[[", 1)

  ## alineament a la taula
  align = rep("c",ncol(results))
  align[names(results) == "levels"] <- "l"
  # results <- results[,!names(results) %in% "variable"]
  # groups_row <- table(var)[unique(var)]

  ## parametres per donar color a les variables amb p.value inferior a punt de tall
  pval_trunc <- as.numeric(sub("su.*", "",gsub("<","",results$p.value, fixed = T)))
  condition <- pval_trunc > pval_cut | is.na(pval_trunc)
  colorRow <- which(rownames(results) %in% grep(paste0(var[which(!condition)],collapse = "|"), rownames(results), value = T) )
  # groups_row <- cumsum(groups_row)

  ## Taula HTML
  results_ht <- results %>%
    # mutate(p.value = cell_spec(p.value, "html", color = ifelse(condition,"black", "white"),
    #                            background = ifelse(condition, "white", "#993489"))) %>%
    kable(escape = F, row.names = F,align = align, caption = caption)  %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = font_size, full_width = F) %>%
    row_spec(0,background = "#993489", color = "white") %>%
    column_spec(which(names(results) == "levels"), width_max = width_lev) %>%
    column_spec(which(names(results) == "variable"), bold = T)   %>%
    column_spec(which(names(results) == "ALL"), bold = T)   %>%
    add_footnote(footnote, escape = F,
                 notation = "symbol" )

  if (!is.null(group) & (sum(pval_trunc < 0.05, na.rm = T) != 0)) {
    results_ht <- results_ht %>% row_spec(colorRow, bold = F, color = "black",background = "#ebe0e9") }#%>%
  # pack_rows(groups_row ,hline_after = F, indent = F)


  return(list(group = group, covariates = covariates, selVar = unique(var[colorRow]), pvalues = pvalues, results = results_ht))
}
