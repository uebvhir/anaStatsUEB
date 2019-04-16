#' A descGroup Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param covariates a character string with names of variables.
#' @param group factor variable. Outcome. Default value is NULL
#' @param data data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.
#' @param method character string indicating the method to test use; possible values are 'param' or 'nonparam'. Default values is 'param'.
#' @keywords summary ci qualitative descriptive exploratory
#' @export descGroup
#' @import kableExtra
#' @examples
#'  # set.seed(1)
#'  # data <- df <- data.frame(MUT = factor(c(rep("A", 12),rep("B",13))),
#'  #                          var = factor(sample(c("Yes", "no"), 25, replace = T)),
#'  #                          size = rnorm(25),
#'  #                          id = paste0("a",1:25))
#'  # data$id <- as.character(data$id)
#'  # descGroup(group  = "MUT",covariates = c("var","size","id"), data = data)

descGroup <- function(covariates, group = NULL,  data, method = "non-param", ...){


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
  # var <- sapply(strsplit(rownames(results), ".", fixed = T),"[[", 1)

  results_ht <- kable(results,escape = F, row.names = F,align = "c",
        caption = caption)  %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = 11) %>%
    row_spec(0,background = "#993489", color = "white") %>%
    add_footnote(footnote, escape = F,
                  threeparttable = T, notation = "symbol" )
    # pack_rows(index = table(var)[unique(var)])

return(list(group = group, covariates = covariates,pvalues = pvalues, results = results_ht))
}
