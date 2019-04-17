descGroup <- function(covariates,
                      group = NULL,
                      data,
                      method = "non-param",
                      font_size = 11,
                      width_lev = "8em",
                      pval_cut = 0.05, ...){


  data <- data[,names(data) %in% c(covariates,group)]
  if (!is.null(group)) varname_group <- ifelse( Hmisc::label(data[,group]) != "", Hmisc::label(data[,group]), group)


  class_data <- unlist(lapply(data, function(x) class(x)[length(class(x))]))
  class_data[which(class_data == "numeric" | class_data == "integer")] <- "numeric"
  class_data <- class_data[!names(class_data) %in% group]

  list_var <- list()
  for (i in seq_along(class_data)) {
    list_var[[names(class_data)[i]]] <- switch(class_data[i],
                                               "numeric" = summary.quanti( x = names(class_data)[i] , group = group ,
                                                                           method = method, data = data, prep2sum = TRUE, ... ) ,
                                               "factor" = summary.quali( x = names(class_data)[i], group = group ,data = data, ...),
    )
  }
  list_var_sum <- lapply(list_var, function(x)x[["summary"]])
  list_var_met <- lapply(list_var, function(x)x[["methods"]])
  pvalues <- unlist(lapply(list_var, function(x)x[["pval"]]))
  results <- do.call("rbind", list_var_sum)
  footnote <-  do.call("cbind", unique(list_var_met))
  caption <- ifelse(is.null(group),
                    "Summary descriptives table",
                    paste0("Summary of results by groups of ", varname_group))
  # names(caption) <- c("Legend", ".")
  var <- sapply(strsplit(rownames(results), ".", fixed = T),"[[", 1)
  align = rep("c",ncol(results))
  align[names(results) == "levels"] <- "l"
  # results <- results[,!names(results) %in% "variable"]
  # groups_row <- table(var)[unique(var)]

  condition <- as.numeric(results$p.value) < pval_cut | complete.cases(as.numeric(results$p.value))
  colorRow <- which(rownames(results) %in% grep(paste0(var[which(condition)],collapse = "|"), rownames(results), value = T) )
  # groups_row <- cumsum(groups_row)
  results_ht <- results %>%
    # mutate(p.value = cell_spec(p.value, "html", color = ifelse(condition,"black", "white"),
    #                            background = ifelse(condition, "white", "#993489"))) %>%
    kable(escape = F, row.names = F,align = align, caption = caption)  %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = font_size, full_width = F) %>%
    row_spec(0,background = "#993489", color = "white") %>%
    column_spec(2, width_max = width_lev) %>%
    column_spec(1, bold = T)  %>%
    row_spec(colorRow, bold = F, color = "black",
             background = "#ebe0e9") %>%
    add_footnote(footnote, escape = F,
                 notation = "symbol" ) #%>%
  # pack_rows(groups_row ,hline_after = F, indent = F)

  return(list(group = group, covariates = covariates, selVar = unique(var[colorRow]), pvalues = pvalues, results = results_ht))
}
