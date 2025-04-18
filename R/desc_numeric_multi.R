#' Summary Table of Multiple Numeric Variables by Multiple Explanatory Variables
#'
#' Generates a styled HTML summary table using `kable` that describes multiple numeric variables
#' grouped by multiple explanatory (categorical  and numerical) variables, including p-values from statistical tests.
#'
#' @param var_numeric A character vector of column names in `dat` representing numeric variables to summarize.
#' @param var_expl A character vector of column names in `dat` representing explanatory variables.
#' @param dat A data frame containing the variables specified in `var_numeric` and `var_expl`.
#'
#' @return Prints an HTML summary table with descriptive statistics and p-values for each explanatory variable
#' across all numeric outcomes. The table is grouped and styled for reporting purposes.
#'
#' @details
#' For each numeric variable in `var_numeric`, the function loops over each explanatory variable in `var_expl`,
#' computes descriptive statistics using the helper function `desc_numeric()` (which is expected to return
#' a data frame with at least `summary`, `p.value`, and `variable` columns), and combines the results.
#' The output is styled using `kableExtra` functions.
#'
#' The helper functions `desc_numeric()` and `get_lab_nam()` must be available in the environment.
#' @author
#' Miriam Mota-Foix <mmota.foix@gmail.com>
#'
#' @importFrom dplyr select rename %>%
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling row_spec column_spec pack_rows add_footnote
#' @examples
#' \dontrun{
#' desc_numeric_multi(
#'   var_numeric = c("age", "income"),
#'   var_expl = c("gender", "education"),
#'   dat = your_dataframe
#' )
#' }
#'
#' @author
#' Miriam Mota-Foix <mmota.foix@gmail.com>
#' @export

desc_numeric_multi <- function(var_numeric, var_expl, dat ) {
  sumar <- list()
  results <- list()
  resul <- matrix()
  for (i in seq_along(var_numeric)) {
    sumar[[i]] <- list()
    results[[i]] <- data.frame()
    rows <- c()
    lab <- Hmisc::label(dat[,var_numeric[i]])
    varnumeric_name <- get_ln(dat, var_numeric[i])
    for (j in seq_along(var_expl))  {
      dn <- desc_numeric(data = dat, covariates = , var_expl[j],y = var_numeric[i], show.all = F, show.n = F)$df_all %>%
        select(-variable) %>%
        rename(!!varnumeric_name := summary)
      pval <- dn$p.value[dn$p.value != ""]
      dn_fin <- rbind(dn %>% select(-p.value), c("p.value",pval))

      rows <- c(rows, rep(get_ln(dat,var_expl[j]),nrow(dn_fin)))
      results[[i]] <- rbind(results[[i]],dn_fin)
    }

    resul <- cbind(resul,results[[i]])
  }

  r <- resul %>% select(-c(levels,resul))
  n <- resul$levels

  final_re <- cbind(n,r)
  colnames(final_re)[1] <- " "

  kk <- final_re %>% kable("html", longtable = T,escape = F, row.names = F, caption = "Summary of results for time variables",align = "c" )  %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"), full_width = F, fixed_thead = T) %>%
    row_spec(0, color = "white", background = "#993489") %>%
    column_spec(which(names(final_re) == " "), width_max = "9em",bold = T) %>%
    pack_rows(index  = table(rows)) %>%
    add_footnote(escape = F, notation = "symbol" )%>%
    row_spec(which(final_re$` ` == "p.value"), bold = T, align = "right")

  print(kk)
}

get_ln <- function(data, variable_names) { ##es la mateixa funcion que mmotaF::get_label_names pero la he copiada per no haber de fer una dependencia de mmotf
  # Aplicar la lógica a cada variable en la lista
  labels <- sapply(variable_names, function(var) {
    # Verificar si existe una etiqueta para la variable
    label_value <- label(data[[var]])

    # Si existe una etiqueta, devolverla; si no, devolver el nombre de la variable
    if (!is.null(label_value) && label_value != "") {
      return(label_value)  # Devuelve la etiqueta si existe
    } else {
      return(var)  # Devuelve el nombre de la variable si no tiene etiqueta
    }
  })

  return(labels)
}

