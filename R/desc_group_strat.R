#' A desc_group_strat Function
#'
#' Genera tablas descriptivas estratificadas por una variable de estratificación.
#' Para cada estrato realiza un análisis descriptivo comparando las variables
#' seleccionadas según los grupos definidos y posteriormente combina los resultados
#' en una única tabla resumen. Las variables numéricas se resumen mediante
#' \code{summary.quanti()} y las categóricas mediante \code{summary.quali()}.
#'
#' @param data data frame que contiene las variables a analizar.
#' @param frml fórmula del tipo \code{grupo ~ x1 + x2 + ...}. Si se especifica,
#' tiene prioridad sobre los argumentos \code{covariates} y \code{group}.
#' @param covariates variables a evaluar. Puede indicarse mediante selección tidyverse.
#' @param group variable de agrupación para la comparación entre grupos.
#' Puede indicarse mediante selección tidyverse.
#' @param strat variable de estratificación. Se generará una tabla independiente
#' para cada nivel de esta variable.
#' @param method método estadístico para variables numéricas. Por defecto
#' \code{"non-param"}.
#' @param caption título de la tabla. Actualmente no se utiliza internamente.
#' @param font_size tamaño de letra de las tablas html. Por defecto 13.
#' @param width_lev ancho de la columna de niveles. Por defecto \code{"8em"}.
#' @param byrow TRUE o FALSE indicando si los porcentajes de las variables
#' categóricas se calculan por filas. Por defecto FALSE.
#' @param show.pval.adj TRUE o FALSE indicando si se muestran p-valores ajustados
#' mediante corrección de Benjamini-Hochberg. Por defecto FALSE.
#' @param pval_cut punto de corte para seleccionar variables significativas.
#' Por defecto 0.05.
#' @param col.background color de fondo de la cabecera de las tablas html.
#' @param col.varsel color utilizado para resaltar las variables seleccionadas
#' según el criterio de p-valor.
#' @param show.pval TRUE o FALSE indicando si se muestran los p-valores.
#' Por defecto TRUE.
#' @param show.all TRUE o FALSE indicando si se muestra la columna resumen ALL.
#' Por defecto FALSE.
#' @param show.n TRUE o FALSE indicando si se muestra el tamaño muestral.
#' Por defecto FALSE.
#' @param show.stat TRUE o FALSE indicando si se muestran los estadísticos de
#' contraste. Por defecto FALSE.
#' @param paired TRUE o FALSE indicando si se trata de datos apareados.
#' Por defecto FALSE.
#' @param idvar nombre de la variable identificadora cuando \code{paired = TRUE}.
#' Por defecto NULL.
#' @param prep2sum TRUE o FALSE indicando si las tablas se preparan para funciones
#' resumen. Por defecto TRUE.
#' @param include.NA TRUE o FALSE indicando si los valores perdidos se incluyen
#' como categoría. Por defecto FALSE.
#' @param patt.NA etiqueta utilizada para los valores perdidos. Por defecto
#' \code{"No"}.
#' @param ... argumentos adicionales.
#'
#' @return Una lista con:
#' \itemize{
#'   \item \code{strat}: variable de estratificación utilizada.
#'   \item \code{strat_levels}: niveles analizados de la variable de estratificación.
#'   \item \code{results_list}: lista con los resultados individuales de cada estrato.
#'   \item \code{results_all}: tabla combinada con todos los estratos.
#'   \item \code{results_ht_all}: tabla html formateada mediante kableExtra.
#' }
#'
#' @details
#' Para cada nivel de la variable de estratificación se ejecuta internamente
#' un análisis equivalente a \code{desc_group()}. Los resultados se combinan
#' posteriormente en una única tabla con cabeceras agrupadas por estrato.
#'
#' Los estratos que no presentan suficientes observaciones en los grupos
#' comparados son excluidos automáticamente del análisis.
#'
#' @export desc_group_strat
#' @import dplyr Hmisc knitr kableExtra magrittr
#' @author Marcos Esteve \email{marcos.esteve@vhir.org}
#' @examples
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am)
#' mtcars$vs <- factor(mtcars$vs)
#' mtcars$cyl <- factor(mtcars$cyl)
#
#' # Análisis estratificado básico
#' desc_group_strat(
#'   data = mtcars,
#'   covariates = c(mpg, wt),
#'   group = am,
#'   strat = cyl
#' )
#'
#' # Utilizando fórmula
#' desc_group_strat(
#'   data = mtcars,
#'   frml = am ~ mpg + wt,
#'   strat = cyl
#' )
#'
#' # Mostrar tamaños muestrales y p-valores ajustados
#' desc_group_strat(
#'   data = mtcars,
#'   covariates = c(mpg, wt),
#'   group = am,
#'   strat = cyl,
#'   show.n = TRUE,
#'   show.pval.adj = TRUE
#' )
#'
#' # Incluir valores perdidos como categoría
#' desc_group_strat(
#'   data = mtcars,
#'   covariates = c(vs),
#'   group = am,
#'   strat = cyl,
#'   include.NA = TRUE
#' )
#'
#' @keywords descriptives stratified subgroup tables summary


desc_group_strat <- function(data,
                             frml = NULL,
                             covariates,
                             group = NULL,
                             strat = NULL,
                             method = "non-param",
                             caption = NULL,
                             font_size = 13,
                             width_lev = "8em",
                             byrow = FALSE,
                             show.pval.adj = FALSE,
                             pval_cut = 0.05,
                             col.background = "#993489",
                             col.varsel = "#ebe0e9",
                             show.pval = TRUE,
                             show.all = FALSE,
                             show.n = FALSE,
                             show.stat = FALSE,
                             paired = FALSE,
                             idvar = NULL,
                             prep2sum = TRUE,
                             include.NA = FALSE,
                             patt.NA = "No",
                             ...) {

  data <- data %>% as.data.frame()
  covariates <- names(data %>% select({{covariates}}))
  group <- names(data %>% select({{group}}))
  strat <- names(data %>% select({{strat}}))

  ## comprobacions
  if (!is.null(strat) && !strat %in% names(data)) stop("The variable '", strat, "' does not exist.")

  if (is.null(frml) & (!is.null(group) & length(group > 0) )) {
    if (!group %in% names(data)) stop("The variable/s '", group, "' do not exist.")
  }else{
    group <- NULL
  }
  if (is.null(frml) ) {
    if (any(!covariates %in% names(data))) {stop("The variable/s '",
                                                 paste0(covariates[!covariates %in% names(data)], collapse = "' , '"),
                                                 "' do not exist.")}}

  if (!is.null(group) & all(is.na(data[,group]))) {stop("Variable '", group, "' is empty")}
  if (paired) {
    names(data)[names(data) == idvar] <- "id"
    idvar <- "id"
  }


  if (!show.pval)  pval_cut <- -1
  ## en el cas de que hi hagi formula seleccionem el grup i les covariates
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {group <- lhs.vars(frml)}
  }

  data[[strat]] <- factor(data[[strat]])
  strat_levels <- levels(data[[strat]])

  covariates <- setdiff(covariates, strat)

  results_list <- lapply(strat_levels, function(s) {
    data_sub <- data[data[[strat]] == s, ]

    ## Copia de desc_group empieza aquí ----------------------
    data_sub <- data_sub %>% as.data.frame()
    covariates_sub <- covariates
    group_sub <- group

    if (!is.null(frml)) {
      covariates_sub <- rhs.vars(frml)
      if (!is.null(lhs.vars(frml))) {
        group_sub <- lhs.vars(frml)
      }
    }

    data_sub <- data_sub[,names(data_sub) %in% c(covariates_sub, group_sub, idvar)]
    if (!is.null(group_sub)) {
      data_sub[, group_sub] <- factor_ueb(data_sub[, group_sub])
      varname_group <- ifelse(Hmisc::label(data_sub[, group_sub]) != "", Hmisc::label(data_sub[, group_sub]), group_sub)
    }

    ## eliminem variables buides
    if (length(covariates ) != 1 || !is.null(group) ) {
      emptyvar <- colSums(is.na(data_sub)) != nrow(data_sub)
      var2del <- names(emptyvar[which(emptyvar == FALSE)])
      if (length(var2del) > 0 ) {
        warning(paste0("Las variable ", var2del, " ha sido eliminada. Todos sus valores son NA. \n"))
        data_sub <- data_sub[, !names(data_sub) %in% var2del]
      }
    }

    if (is.null(dim(data_sub))) {
      lbl <- Hmisc::label(data_sub)
      data_sub <- data.frame(data_sub)
      names(data_sub) <- covariates_sub
      Hmisc::label(data_sub) <- lbl
    }

    ## guardem class de cada variable
    class_data <- unlist(lapply(data_sub, function(x) class(x)[length(class(x))]))
    class_data[class_data %in% c("numeric", "integer")] <- "numeric"
    class_data <- class_data[!names(class_data) %in% c(group_sub, idvar)]

    if (any(class_data == "character")) {
      message("La variable/s '",
              paste0(names(class_data)[class_data == "character"], collapse = "' , '"),
              "' es tipo caracter y no se ha analizado")
      covariates_sub <- covariates_sub[!covariates_sub %in% names(class_data)[class_data == "character"]]
    }

    list_var <- list()
    for (i in seq_along(class_data)) {
      list_var[[names(class_data)[i]]] <- switch(class_data[i],
                                                 "numeric" = summary.quanti(x = names(class_data)[i], group = group_sub, method = method,
                                                                            data = data_sub, prep2sum = prep2sum, show.pval = show.pval,
                                                                            show.stat = show.stat, paired = paired, idvar = idvar, var.tidy = FALSE),
                                                 "factor" = summary.quali(x = names(class_data)[i], group = group_sub, data = data_sub,
                                                                          byrow = byrow, show.pval = show.pval, show.stat = show.stat,
                                                                          include.NA = include.NA, patt.NA = patt.NA, var.tidy = FALSE),
                                                 "character" = next()
      )
    }

    list_var_sum <- lapply(list_var, function(x) x[["summary"]])

    # Si algunos levels no tienen valores, se eliminan con el summary.quali. Por ello añadimos filas con niveles faltantes para factores
    list_var_sum <- mapply(function(summary_df, var_name) {
      # Solo aplicamos esto a data frames válidos
      if (is.null(summary_df) || !("levels" %in% names(summary_df))) return(summary_df)

      # Obtener todos los niveles posibles de la variable original
      all_levels <- levels(data_sub[[var_name]])
      levels_presentes <- summary_df$levels
      levels_faltantes <- setdiff(all_levels, levels_presentes)

      if (length(levels_faltantes) > 0) {
        # Crear un data frame con los niveles faltantes
        df_faltantes <- data.frame(matrix(NA, nrow = length(levels_faltantes), ncol = ncol(summary_df)))
        colnames(df_faltantes) <- colnames(summary_df)
        df_faltantes$variable <- ""
        df_faltantes$levels <- levels_faltantes
        summary_df <- rbind(summary_df, df_faltantes)
      }

      summary_df
    }, list_var_sum, names(list_var_sum), SIMPLIFY = FALSE)

    results <- do.call("rbind", list_var_sum)
    pvalues <- unlist(lapply(list_var, function(x) x[["pval"]]))

    if (show.pval.adj) {
      results$p.val.adj <- NA
      results$p.val.adj[which(results$p.value != "")] <- round(p.adjust(as.numeric(as.character(pvalues)), method = "BH"), 2)
      results$p.val.adj[which(results$p.value == "")] <- ""
      results <- results[, c(names(results)[1:which(names(results) == "p.value")], "p.val.adj", "n")]
      results$p.val.adj[which(results$p.val.adj != "" & results$p.val.adj < 0.001)] <- "<0.001"
    }

    var <- sapply(strsplit(rownames(results), ".", fixed = TRUE), "[[", 1)

    if (show.pval.adj) {
      pval_valid <- results$p.val.adj
    } else {
      pval_valid <- results$p.value
    }

    pval_trunc <- as.numeric(sub("su.*", "", gsub("<", "", pval_valid, fixed = TRUE)))
    condition <- pval_trunc > pval_cut | is.na(pval_trunc)
    colorRow <- if (!all(condition)) which(var %in% var[!condition]) else NULL

    options(knitr.kable.NA = '')
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

    if (!is.null(group_sub)) {
      results <- results %>% rename_at(levels(data_sub[, group_sub]), ~ paste0(levels(data_sub[, group_sub]), "<br>",
                                                                               table(data_sub[, group_sub]), " (",
                                                                               round(prop.table(table(data_sub[, group_sub])) * 100, 2), "%)"))
    }

    results_ht <- results %>%
      kable(escape = FALSE, row.names = FALSE, align = rep("c", ncol(results)), caption = paste0("Estrato: ", strat, " = ", s), format = "html") %>%
      kable_styling(latex_options = c("striped", "hold_position", "repeat_header"),
                    font_size = font_size, full_width = FALSE, fixed_thead = TRUE) %>%
      row_spec(0, background = col.background, color = "white") %>%
      column_spec(which(names(results) == "levels"), width_max = width_lev) %>%
      column_spec(which(names(results) == "variable"), bold = TRUE) %>%
      column_spec(which(names(results) == "ALL"), bold = TRUE)

    if (!is.null(group_sub) && (sum(pval_trunc < pval_cut, na.rm = TRUE) != 0)) {
      results_ht <- results_ht %>% row_spec(colorRow, bold = FALSE, color = "black", background = col.varsel)
    }

    list(group = group_sub, covariates = covariates_sub, selVar = var[!condition],
         pvalues = as.numeric(as.character(pvalues)), df_all = results, results = results_ht)
  })

  names(results_list) <- strat_levels # Nombramos



  # Inicializamos con el primer dataframe
  results_combined <- results_list[[strat_levels[1]]]$df_all

  # Añadir sufijo al primer estrato
  names(results_combined)[!names(results_combined) %in% c("variable", "levels")] <-
    paste0(
      strat_levels[1],
      "_",
      names(results_combined)[!names(results_combined) %in% c("variable", "levels")]
    )

  # Crear row_id
  results_combined$row_id <- rownames(results_combined)

  # Bucle para unir el resto
  for (s in strat_levels[-1]) {

    df_next <- results_list[[s]]$df_all

    # Renombrar columnas excepto variable y levels
    names(df_next)[!names(df_next) %in% c("variable", "levels")] <-
      paste0(
        s,
        "_",
        names(df_next)[!names(df_next) %in% c("variable", "levels")]
      )

    # Crear row_id
    df_next$row_id <- rownames(df_next)

    # Join manteniendo solo columnas nuevas
    results_combined <- results_combined %>%
      left_join(
        df_next %>%
          select(-variable, -levels),
        by = "row_id"
      )
  }

  # Opcional: eliminar row_id final
  results_combined <- results_combined %>%
    select(-row_id)

  # Reemplazar NA por ""
  results_combined[is.na(results_combined)] <- ""

  # Construimos cabecera superior para la tabla
  header_vector <- c(" " = 2) # para las columnas 'variable' y 'levels'
  for (s in strat_levels) {
    ncols <- ncol(results_list[[s]]$df_all) - 2 # número de columnas de ese estrato (excluyendo variable y levels)
    header_vector[s] <- ncols
  }

  # Construimos tabla con formateo
  tabla_final <- results_combined %>%
    kable(format = "html", escape = FALSE, row.names = FALSE,
          align = rep("c", ncol(.)), table.attr = 'class="table-with-group-header"') %>%
    add_header_above(header_vector, background = col.background,color = "white",bold = TRUE) %>%
    kable_styling(latex_options = c("striped", "hold_position", "repeat_header"),
                  font_size = font_size, full_width = FALSE, fixed_thead = TRUE) %>%
    row_spec(0, background = col.background, color = "white") %>%
    column_spec(which(names(results_combined) == "levels"), width_max = width_lev) %>%
    column_spec(which(names(results_combined) == "variable"), bold = TRUE)

  return(list(
    strat = strat,
    strat_levels = strat_levels,
    results_list = results_list,
    results_all = results_combined,
    results_ht_all = tabla_final
  ))

}
