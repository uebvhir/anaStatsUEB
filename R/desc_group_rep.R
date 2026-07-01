#' A desc_group_rep Function
#'
#' Genera una tabla descriptiva para datos longitudinales o de medidas repetidas,
#' comparando las variables entre distintos momentos temporales de seguimiento.
#' Para variables numéricas realiza comparaciones de medidas repetidas y para
#' variables categóricas compara la distribución de frecuencias entre tiempos.
#'
#' @details
#' Actualmente la funcion únicamente admite que la variable tiempo contenga 2 niveles.
#'
#'
#' @param data data frame que contiene las variables a analizar.
#' @param covariates variables a evaluar. Puede indicarse mediante selección tidyverse.
#' @param tiempo variable que identifica el momento temporal o visita. Puede indicarse mediante selección tidyverse.
#' @param id identificador único de sujeto.
#' @param method método estadístico para variables numéricas. Por defecto \code{"non-param"}.
#' @param caption título de la tabla. Si es NULL se genera automáticamente.
#' @param font_size tamaño de letra de la tabla html. Por defecto 13.
#' @param width_lev ancho de la columna de niveles. Por defecto \code{"8em"}.
#' @param byrow valor lógico indicando si los resultados se presentan por filas. Por defecto FALSE.
#' @param show.pval.adj TRUE o FALSE indicando si se muestran los p-valores ajustados mediante corrección de Benjamini-Hochberg. Por defecto FALSE.
#' @param pval_cut punto de corte para seleccionar variables significativas. Por defecto 0.05.
#' @param col.background color de fondo de la cabecera de la tabla html.
#' @param col.varsel color utilizado para resaltar las variables seleccionadas según el criterio de p-valor.
#' @param show.pval TRUE o FALSE indicando si se muestran los p-valores. Por defecto TRUE.
#' @param show.all TRUE o FALSE indicando si se muestran los resultados globales. Por defecto TRUE.
#' @param show.n TRUE o FALSE indicando si se muestra el tamaño muestral. Por defecto TRUE.
#' @param show.stat TRUE o FALSE indicando si se muestran los estadísticos de contraste. Por defecto FALSE.
#' @param show.or TRUE o FALSE indicando si se muestra la odds ratio cuando corresponda. Por defecto FALSE.
#' @param idvar nombre de la variable identificadora. Reservado para compatibilidad. Por defecto NULL.
#' @param prep2sum TRUE o FALSE indicando si se preparan los resultados para tablas resumen. Por defecto TRUE.
#' @param include.NA TRUE o FALSE indicando si los valores perdidos se incluyen como categoría. Por defecto FALSE.
#' @param patt.NA etiqueta utilizada para los valores perdidos. Por defecto \code{"No"}.
#' @param correct TRUE o FALSE indicando si se aplica corrección en los contrastes para variables categóricas. Por defecto FALSE.
#' @param ... argumentos adicionales.
#'
#' @return Una lista con:
#' \itemize{
#'   \item \code{tiempo}: variable temporal utilizada para la comparación.
#'   \item \code{covariates}: variables evaluadas.
#'   \item \code{selVar}: variables seleccionadas según el punto de corte definido.
#'   \item \code{pvalues}: vector con los p-valores obtenidos.
#'   \item \code{df_all}: tabla resumen en formato data.frame.
#'   \item \code{results}: tabla formateada en html mediante kableExtra.
#' }
#'
#' @export desc_group_rep
#' @import dplyr Hmisc knitr kableExtra
#' @author Àlex Martí Barrera, Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' set.seed(1111)
#'
#' df <- data.frame(
#'   id = rep(1:20, each = 2),
#'   visita = factor(rep(c("Basal", "Seguimiento"), 20)),
#'   edad = rnorm(40, 65, 10),
#'   dolor = factor(sample(c("Sí", "No"), 40, replace = TRUE))
#' )
#'
#' # Comparación longitudinal básica
#' desc_group_rep(
#'   data = df,
#'   covariates = c(edad, dolor),
#'   tiempo = visita,
#'   id = "id"
#' )
#'
#' # Mostrando únicamente variables significativas
#' desc_group_rep(
#'   data = df,
#'   covariates = c(edad, dolor),
#'   tiempo = visita,
#'   id = "id",
#'   pval_cut = 0.01
#' )
#'
#' # Mostrar p-valores ajustados
#' desc_group_rep(
#'   data = df,
#'   covariates = c(edad, dolor),
#'   tiempo = visita,
#'   id = "id",
#'   show.pval.adj = TRUE
#' )
#'
#' @keywords descriptives longitudinal repeated-measures tables

desc_group_rep <- function (data, covariates, tiempo, id, method = "non-param",
                      caption = NULL, font_size = 13, width_lev = "8em", byrow = FALSE,
                      show.pval.adj = FALSE, pval_cut = 0.05, col.background = "#993489",
                      col.varsel = "#ebe0e9", show.pval = TRUE, show.all = TRUE,
                      show.n = TRUE, show.stat = FALSE, show.or = FALSE,
                      idvar = NULL, prep2sum = TRUE, include.NA = FALSE, patt.NA = "No",
                      correct = FALSE, ...)
{
  data <- data %>% as.data.frame()
  covariates <- names(data %>% select({
    {
      covariates
    }
  }))
  tiempo <- names(data %>% select({
    {
      tiempo
    }
  }))
  if (length(tiempo > 0)) {
    if (!tiempo %in% names(data))
      stop("The variable/s '", tiempo, "' do not exist.")
  }
  else {
    tiempo <- NULL
  }
  if (!is.null(tiempo) & all(is.na(data[, tiempo]))) {
    stop("Variable '", tiempo, "' is empty")
  }
  if (!show.pval)
    pval_cut <- -1

  data <- data[, names(data) %in% c(covariates, tiempo, id)]

  data[, tiempo] <- factor_ueb(data[, tiempo])
  varname_tiempo <- ifelse(Hmisc::label(data[, tiempo]) !=
                              "", Hmisc::label(data[, tiempo]), tiempo)
  if (length(covariates) != 1 | !is.null(tiempo)) {
    emptyvar <- colSums(is.na(data)) != nrow(data)
    var2del <- names(emptyvar[which(emptyvar == FALSE)])
    if (length(var2del) > 0) {
      warning(paste0("Las variable ", var2del, " ha sido eliminada. Todos sus valores son NA. \n"))
      data <- data[, !names(data) %in% var2del]
    }
  }
  if (is.null(dim(data))) {
    lbl <- Hmisc::label(data)
    data <- data.frame(data)
    names(data) <- covariates
    Hmisc::label(data) <- lbl
  }
  class_data <- unlist(lapply(data, function(x) class(x)[length(class(x))]))
  class_data[which(class_data == "numeric" | class_data ==
                     "integer")] <- "numeric"
  class_data <- class_data[!names(class_data) %in% c(tiempo,
                                                     id)]
  if (any(class_data == "character")) {
    message("La variable/s '", paste0(names(class_data)[class_data ==
                                                          "character"], collapse = "' , '"), "' es tipo caracter y no se ha analizado")
    covariates <- covariates[!covariates %in% names(class_data)[class_data ==
                                                                  "character"]]
  }
  list_var <- list()
  for (i in seq_along(class_data)) {
    list_var[[names(class_data)[i]]] <- switch(class_data[i],
                                               numeric = summary.quanti.rep(x = names(class_data)[i],
                                                                        tiempo = tiempo, id = id, data = data,
                                                                        prep2sum = prep2sum, show.pval = show.pval, show.stat = show.stat,
                                                                        var.tidy = FALSE), factor = summary.quali.rep(x = names(class_data)[i],
                                                                                                                       tiempo = tiempo, id = id, data = data, show.pval = show.pval,
                                                                                                                       show.stat = show.stat, var.tidy = FALSE, correct = correct,
                                                                                                                       ), character = (next)())
}
  list_var_sum <- lapply(list_var, function(x) x[["summary"]])
  results <- do.call("rbind", list_var_sum)
  pvalues <- unlist(lapply(list_var, function(x) x[["pval"]]))
  if (show.pval.adj) {
    if (anyNA(results$p.value))
      stop("P.value NA")
    results$p.val.adj <- NA
    results$p.val.adj[which(results$p.value != "")] <- round(p.adjust(as.numeric(as.character(pvalues)),
                                                                      method = "BH"), 2)
    results$p.val.adj[which(results$p.value == "")] <- ""
    results <- results[, c(names(results)[1:which(names(results) ==
                                                    "p.value")], "p.val.adj", "n")]
    results$p.val.adj[which(results$p.val.adj != "" & results$p.val.adj <
                              0.001)] <- "<0.001"
  }
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
    caption <- ifelse(is.null(tiempo), "Summary statistics table",
                      paste0("Summary of results by tiempos of ", varname_tiempo))
  }
  var <- sapply(strsplit(rownames(results), ".", fixed = T),
                "[[", 1)
  align = rep("c", ncol(results))
  align[names(results) == "levels"] <- "l"
  if (show.pval.adj) {
    pval_valid <- results$p.val.adj
  }
  else {
    pval_valid <- results$p.value
  }
  pval_trunc <- as.numeric(sub("su.*", "", gsub("<", "", pval_valid,
                                                fixed = T)))
  condition <- pval_trunc > pval_cut | is.na(pval_trunc)
  if (!all(condition)) {
    var_pval_cut <- var[which(!condition)]
    colorRow <- which(var %in% var_pval_cut)
  }
  else {
    var_pval_cut <- NA
  }
  options(knitr.kable.NA = "")
  if (!show.pval) {
    results <- results[, !names(results) %in% c("p.value")]
    pvalues <- NA
  }
  if (!show.all) {
    results <- results[, !names(results) %in% c("ALL")]
  }
  if (!show.n) {
    results <- results[, !names(results) %in% c("n")]
  }
  if (!show.or) {
    results <- results[, !names(results) %in% c("OR")]
  }
  results <- results %>% rename_at(levels(data[, tiempo]), ~paste0(levels(data[,
                                                                              tiempo]), "<br>", table(data[, tiempo]), " (", round(prop.table(table(data[,
                                                                                                                                                       tiempo])) * 100, 2), "%)"))
  results_ht <- results %>% kable(escape = F, row.names = F,
                                  align = align, caption = caption, format = "html") %>%
    kable_styling(latex_options = c("striped", "hold_position",
                                    "repeat_header"), font_size = font_size, full_width = F,
                  fixed_thead = T) %>% row_spec(0, background = col.background,
                                                color = "white") %>% column_spec(which(names(results) ==
                                                                                         "levels"), width_max = width_lev) %>% column_spec(which(names(results) ==
                                                                                                                                                   "variable"), bold = T) %>% column_spec(which(names(results) ==
                                                                                                                                                                                                  "ALL"), bold = T) %>% add_footnote(footnote, escape = F,
                                                                                                                                                                                                                                     notation = "symbol")
  if (!is.null(tiempo) & (sum(pval_trunc < pval_cut, na.rm = T) !=
                         0)) {
    results_ht <- results_ht %>% row_spec(colorRow, bold = F,
                                          color = "black", background = col.varsel)
  }
  return(list(tiempo = tiempo, covariates = covariates, selVar = var_pval_cut,
              pvalues = as.numeric(as.character(pvalues)), df_all = results,
              results = results_ht))
}
