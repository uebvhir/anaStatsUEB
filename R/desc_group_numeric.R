#' A desc_group_numeric Function
#'
#' Genera una tabla resumen para covariables numéricas y factores respecto a una o varias variables respuesta numéricas.
#' Es la funcion equivalente a desc_group pero cuando tu variable outcome o de interés es numérica.
#' Para covariables numéricas calcula correlaciones y para covariables categóricas realiza comparaciones entre grupos,
#' mostrando únicamente las variables que cumplen los criterios de selección definidos.
#'
#' @param data data frame que contiene las variables a analizar.
#' @param covariates variables explicativas a evaluar. Puede indicarse mediante selección tidyverse.
#' @param y variable o variables respuesta. Puede indicarse mediante selección tidyverse.
#' @param frml fórmula del tipo y ~ x1 + x2 + ... . Si se especifica, tiene prioridad sobre los argumentos \code{covariates} y \code{y}.
#' @param method método para la comparación de variables numéricas según grupos. Por defecto \code{"non-param"}.
#' @param method.cor método de correlación utilizado para variables numéricas. Por defecto \code{"pearson"}. Falta incluir valores que acepta. Spearman no funciona
#' @param caption título de la tabla. Si es NULL se genera automáticamente.
#' @param font_size tamaño de letra de la tabla html. Por defecto 13.
#' @param width_lev ancho de la columna de niveles. Por defecto \code{"9em"}.
#' @param col.background color de fondo de la cabecera de la tabla html.
#' @param show.pval TRUE o FALSE indicando si se muestran los p-valores. Por defecto TRUE.
#' @param show.all TRUE o FALSE indicando si se muestran las filas resumen "ALL". Por defecto TRUE.
#' @param show.n TRUE o FALSE indicando si se muestra el tamaño muestral. Por defecto TRUE.
#' @param corplot TRUE o FALSE indicando si se genera gráfico de correlación. Por defecto FALSE.
#' @param nround número de decimales para mostrar los resultados. Por defecto 1.
#' @param cor_cut punto de corte para seleccionar variables numéricas según el coeficiente de correlación. Por defecto 0.7.
#' @param pval_cut punto de corte para seleccionar variables categóricas según el p-valor. Por defecto 0.05.
#'
#' @return Una lista con:
#' \itemize{
#'   \item \code{outcome}: variable o variables respuesta analizadas.
#'   \item \code{covariates}: variables explicativas evaluadas.
#'   \item \code{df_all}: tabla resumen en formato data.frame.
#'   \item \code{selVar}: variables seleccionadas según los criterios definidos.
#'   \item \code{results}: tabla formateada en html mediante kableExtra.
#' }
#'
#' @export desc_group_numeric
#' @import dplyr magrittr knitr kableExtra
#' @author Àlex Martí Barrera, Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$am <- factor(mtcars$am)
#'
#' # Una variable respuesta
#' desc_group_numeric(
#'   data = mtcars,
#'   covariates = c(mpg, wt, cyl),
#'   y = disp
#' )
#'
#' # Utilizando fórmula
#' desc_group_numeric(
#'   data = mtcars,
#'   frml = disp ~ mpg + wt + cyl
#' )
#'
#' # Varias variables respuesta
#' desc_group_numeric(
#'   data = mtcars,
#'   covariates = c(mpg, wt, cyl),
#'   y = c(disp, drat)
#' )
#'
#' # Modificando puntos de corte
#' desc_group_numeric(
#'   data = mtcars,
#'   covariates = c(mpg, wt, cyl),
#'   y = disp,
#'   cor_cut = 0.5,
#'   pval_cut = 0.10
#' )
#'
#' @keywords descriptives summary correlations tables


desc_group_numeric <- function (data, covariates, y = NULL, frml = NULL, method = "non-param",
          method.cor = "pearson", caption = NULL, font_size = 13, width_lev = "9em",
          col.background = "#993489", show.pval = TRUE, show.all = TRUE,
          show.n = TRUE, corplot = FALSE, nround = 1, cor_cut = 0.7,
          pval_cut = 0.05) {
  selVar_list <- list()
  results_list <- list()
  results_ht_list <- list()
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {
      y <- lhs.vars(frml)
    }
  }
  else{covariates <- names(data %>% dplyr::select({
    {
      covariates
    }
  }))
  y <- names(data %>% dplyr::select({
    {
      y
    }
  }))}
  for (j in 1:length(y)){
  if (is.null(frml) & !is.null(y[j])) {
    if (!y[j] %in% names(data))
      stop("The variable/s '", y[j], "' do not exist.")
  }
  if (is.null(frml)) {
    if (any(!covariates %in% names(data))) {
      stop("The variable/s '", paste0(covariates[!covariates %in%
                                                   names(data)], collapse = "' , '"), "' do not exist.")
    }
  }
  if (!is.null(y[j]) & all(is.na(data[, y[j]]))) {
    stop("Variable '", y[j], "' is empty")
  }
  if (!show.pval)
    pval_cut <- -1
  data_2 <- data[, names(data) %in% c(covariates, y[j])]
  if (length(covariates) != 1 | !is.null(y[j])) {
    emptyvar <- colSums(is.na(data_2)) != nrow(data_2)
    var2del <- names(emptyvar[which(emptyvar == FALSE)])
    if (length(var2del) > 0) {
      warning(paste0("Las variable ", var2del, " ha sido eliminada. Todos sus valores son NA. \n"))
      data_2 <- data_2[, !names(data_2) %in% var2del]
    }
  }
  class_data <- unlist(lapply(data_2, function(x) class(x)[length(class(x))]))
  class_data[which(class_data == "numeric" | class_data ==
                     "integer")] <- "numeric"
  class_data <- sort(class_data[!names(class_data) %in% c(y[j])])
  data_2 %<>% dplyr::select(y[j], names(class_data))
  if (any(class_data == "character")) {
    message("La variable/s '", paste0(names(class_data)[class_data ==
                                                          "character"], collapse = "' , '"), "' es tipo caracter y no se ha analizado")
    covariates <- covariates[!covariates %in% names(class_data)[class_data ==
                                                                  "character"]]
  }
  list_var <- list()
  for (i in seq_along(class_data)) {
    list_var[[names(class_data)[i]]] <- switch(class_data[i],
                                               numeric = quickCor(x = names(class_data)[i], y = y[j],  dat = data_2, prep.tab = T, corplot = corplot, xtab = F,
                                                                  nround = nround, method = method.cor),
                                               factor = summary.quanti(x = y[j], group = names(class_data)[i], data = data_2, var.tidy = F, prep.tab = T, method = method,
                                                                       nround = nround),
                                               character = (next)())
  }
  select_num <- unlist(lapply(list_var[names(which(class_data ==
                                                     "numeric"))], function(x) any(var_to_num(x[["result"]][,
                                                                                                            "rho"]) > cor_cut)))
  select_fac <- unlist(lapply(list_var[names(which(class_data ==
                                                     "factor"))], function(x) any(var_to_num(x[["pval"]]) <
                                                                                    pval_cut)))
  list_var_sum <- lapply(list_var, function(x) x[["df_prep_tab"]])
  results <- do.call("rbind", list_var_sum)
  footnote <- NULL
  typevar <- c("factor", "numeric")
  for (i in seq_along(typevar)) {
    desc <- unique(lapply(list_var[names(list_var) %in% names(class_data)[class_data ==
                                                                            typevar[i]]], function(x) x[["methods"]]))
    tst <- paste0(unique(lapply(list_var[names(list_var) %in%
                                           names(class_data)[class_data == typevar[i]]], function(x) x[["txt_test"]])),
                  collapse = "")
    footnote <- unique(gsub("NULL", "", c(footnote, paste0(desc,
                                                           tst))))
  }
  if (is.null(caption)) {
    if(length(y) == 1){
    caption <- ifelse(is.null(y[j]), "Summary statistics table",
                      paste0("Summary of results for ", y[j]))}
    else{caption <- "Summary statistics table"}
  }
  var <- sapply(strsplit(rownames(results), ".", fixed = T),
                "[[", 1)
  align = rep("c", ncol(results))
  align[names(results) == "levels"] <- "c"
  options(knitr.kable.NA = "")
  if (!show.pval) {
    results <- results[, !names(results) %in% c("p.value")]
    pvalues <- NA
  }
  if (!show.all) {
    results <- results[!grepl("ALL", rownames(results)),
    ]
  }
  if (!show.n) {
    results <- results[, !names(results) %in% c("n")]
  }
  selVar_list[[j]] <- names(which(c(select_fac, select_num)))
  results_list[[j]] <- results %>%
    rename_at(levels(data_2[, y[j]]), ~paste0(levels(data_2[, y[j]]), "<br>", table(data_2[, y[j]]), " (", round(prop.table(table(data_2[, y[j]])) * 100, 2), "%)"))

  if (length(y) > 1){
    results_list[[j]] %<>%
      rename(!!paste0("summary ", y[j]) := summary, !!paste0("p.value ", y[j]) := p.value, !!paste0("n ", y[j]) := n)
  }
  }

  names(selVar_list) <- y

  if (length(results_list) > 1){
  for(i in 2:length(results_list)){
    results_list[[1]] <- merge(results_list[[1]], results_list[[i]], by = c("variable", "levels"))
  }}
  results <- results_list[[1]]
  vert <- seq(from = 2, to = length(names(results)), by=3)

  results_ht <- results %>% dplyr::select(-variable) %>% kable(escape = F, row.names = F, align = align, caption = caption, format = "html") %>%
    kable_styling(latex_options = c("striped", "hold_position", "repeat_header"), font_size = font_size, full_width = F, fixed_thead = T) %>%
    row_spec(0, background = col.background, color = "white") %>%
    column_spec(vert[-length(vert)], border_left = TRUE ) %>%
    column_spec(which(names(results) == "levels"), width_max = width_lev) %>%
    column_spec(which(names(results) == "variable"), bold = T) %>%
    add_footnote(footnote, escape = F, notation = "symbol") %>% row_spec(which(results$levels == "ALL"), bold = T, align = "right") %>%
    pack_rows(index = table(results$variable)[unique(results$variable)])

  return(list(outcome = y, covariates = covariates, df_all = results,
              selVar = selVar_list, results = results_ht))
}
