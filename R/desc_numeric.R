#' A desc_numeric Function
#'
#' DESCRIPCIO DE LA FUNCIO
#' @param frml Right side of ~ must have the terms in an additive way, and left side of ~ must contain the name of the grouping variable or can be left in blank (in this latter case descriptives for whole sample are calculated and no test is performed).
#' @param covariates a character string with names of variables.
#' @param y numeric variable. Outcome
#' @param data data frame, list or environment (or object coercible by 'as.data.frame' to a data frame) containing the variables in the model. If they are not found in 'data', the variables are taken from 'environment(formula)'.
#' @param method character string indicating the method to test use; possible values are 'param' or 'non-param'. Default values is 'non-param'.
#' @param font_size A numeric input for table font size
#' @param width_lev defines the maximum width of table columns. Default value is 9em
#' @param show.pval logical indicating whether p-value of overall groups significance ('p.overall' column) is displayed or not. Default value is TRUE.
#' @param caption Character vector containing the table's caption or title.
#' @keywords summary ci quantitative outcome descriptive exploratory
#' @export desc_numeric
#' @import magrittr
#' @examples




desc_numeric <- function(data,
                        covariates,
                        y,
                        frml = NULL,
                        method = "non-param",
                        caption = NULL,
                        font_size = 13,
                        width_lev = "9em",
                        col.background = "#993489",
                        show.pval = TRUE,
                        show.all = TRUE,
                        show.n = TRUE,
                        corplot = FALSE,
                        nround = 1,
                        ...){

  covariates <- names(data %>% select({{covariates}}))
  y <- names(data %>% select({{y}}))

  ## comprobacions
  if (is.null(frml) & !is.null(y) ) {
    if (!y %in% names(data)) stop("The variable/s '", y, "' do not exist.")}
  if (is.null(frml) ) {
    if (any(!covariates %in% names(data))) {stop("The variable/s '",
                                                 paste0(covariates[!covariates %in% names(data)], collapse = "' , '"),
                                                 "' do not exist.")}}

  if (!is.null(y) & all(is.na(data[,y]))) {stop("Variable '", y, "' is empty")}



  if (!show.pval)  pval_cut <- -1
  ## en el cas de que hi hagi formula seleccionem el grup i les covariates
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {y <- lhs.vars(frml)}
  }

  ## Seleccionem variables i etiquetes
  data <- data[,names(data) %in% c(covariates,y)]


  ## eliminem variables buides
  if (length(covariates ) != 1 | !is.null(y) ) {
    emptyvar <- colSums(is.na(data)) != nrow(data)
    var2del <- names(emptyvar[which(emptyvar == FALSE)])
    if (length(var2del) > 0 ) {
      warning(paste0("Las variable ",var2del, " ha sido eliminada. Todos sus valores son NA. \n" ))
      data <- data[,!names(data) %in% var2del]
    }}
  # Hmisc::label(data,self = F)[Hmisc::label(data) == ""] <- names(data)[Hmisc::label(data) == ""]



  ## guardem class de cada variable
  class_data <- unlist(lapply(data, function(x) class(x)[length(class(x))]))
  class_data[which(class_data == "numeric" | class_data == "integer")] <- "numeric"
  class_data <- sort(class_data[!names(class_data) %in% c(y)])
  data %<>% select(y, names(class_data) )

  if (any(class_data == "character")) {
    message("La variable/s '",
            paste0(names(class_data)[class_data == "character"],collapse = "' , '"),
            "' es tipo caracter y no se ha analizado")
    covariates <- covariates[!covariates %in% names(class_data)[class_data == "character"]]
  }

  ## realitzem analisis descriptiu i/o comparatiu
  list_var <- list()
  for (i in seq_along(class_data)) {
    list_var[[names(class_data)[i]]] <- switch(class_data[i],
                                               "numeric" = quickCor(x = names(class_data)[i], y = y,dat = data, prep.tab = T,
                                                                    corplot = corplot, xtab = F, nround = nround, ...) ,
                                               "factor" = summary.quanti( x = y, group =  names(class_data)[i],data = data,
                                                                          show.pval = show.pval,var.tidy = F, prep.tab = T, method = method,
                                                                          nround = nround, ...),
                                               "character" = next()
    )
  }

  list_var_sum <- lapply(list_var, function(x)x[["df_prep_tab"]])
  results <- do.call("rbind", list_var_sum)
  # results <- plyr::rbind.fill(list_var_sum)



  ## Caption de la taula final
  footnote <- NULL
  typevar <- c("factor", "numeric")
  for (i in seq_along(typevar)) {
    desc <- unique(lapply(list_var[names(list_var) %in% names(class_data)[class_data == typevar[i]] ], function(x)x[["methods"]]))
    tst <- paste0(unique(lapply(list_var[names(list_var) %in% names(class_data)[class_data == typevar[i]]], function(x)x[["txt_test"]])), collapse = "")
    footnote <- unique(gsub("NULL","",c(footnote, paste0(desc, tst))))
  }


  # footnote <-  do.call("cbind", unique(list_var_met))
  if (is.null(caption)) {
    caption <- ifelse(is.null(y),
                      "Summary statistics table",
                      paste0("Summary of results for ", y))
  }
  ## CREACIO DE LA TAULA FINAL
  # variables per files
  var <- sapply(strsplit(rownames(results), ".", fixed = T),"[[", 1)

  ## alineament a la taula
  align = rep("c",ncol(results))
  align[names(results) == "levels"] <- "c"


  options(knitr.kable.NA = '')
  ## Taula HTML
  if (!show.pval) {
    results <- results[,!names(results) %in% c("p.value")]
    pvalues <- NA}
  if (!show.all) {
    results <- results[,!names(results) %in% c("ALL")]
  }
  if (!show.n) {
    results <- results[,!names(results) %in% c("n")]
  }

  ## Afegim percentatges als titols
  results  <- results %>% rename_at(levels(data[,y]), ~ paste0(levels(data[,y]), "<br>",
                                                               table(data[,y])," (",round(prop.table(table(data[,y]))*100,2),"%)"  ))


   results_ht <- results %>%
    select(-variable)%>%
    # mutate(p.value = cell_spec(p.value, "html", color = ifelse(condition,"black", "white"),
    #                            background = ifelse(condition, "white", "#993489"))) %>%
    kable(escape = F, row.names = F,align = align, caption = caption)  %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = font_size, full_width = F, fixed_thead = T) %>%
    row_spec(0,background = col.background, color = "white") %>%
    column_spec(which(names(results) == "levels"), width_max = width_lev) %>%
    column_spec(which(names(results) == "variable"), bold = T)   %>%
    # column_spec(which(names(results) == "ALL"), bold = T)   %>%
    add_footnote(footnote, escape = F,
                 notation = "symbol" )%>%
    row_spec(which(results$levels == "ALL"), bold = T, align = "right")  %>%
    pack_rows(index  = table(results$variable)[unique(results$variable)])

  return(list(outcome = y, covariates = covariates,
              # selVar = var_pval_cut,
              # pvalues = as.numeric(as.character(pvalues)),
              df_all = results, results = results_ht))
}






# Hmisc::label(mtc_bis, self = F) <- paste0("Labels", names(mtc_bis))
# desc_numeric(data = mtc_bis, covariates = c("am", "mpg", "cyl", "drat"), y = "wt")



#













#
#
#
# prep_tab <- function(...){
#
# }
#
# df <- data.frame(variable = character(), levels = character(), summary = character(), p.value = character(), n = character())
#
# qc <- quickCor(x = qsec, y = mpg,dat = data, prep.tab = T)$df_prep_tab
# df %<>% bind_rows( qc)
#
#
# sq <- summary.quanti( data, x = mpg, group = vs, prep.tab = T)$df_prep_tab
# df %<>% bind_rows( sq)
#
#
# require(kableExtra)
#
# df %>%
#   select(-variable) %>%
#   kable_ueb(row.names = F, caption = paste0( "Summary of results for ", y), align = "c"  ) %>%
#   pack_rows(index  = table(df$variable)[unique(df$variable)]) %>%
#   row_spec(which(df$levels == "ALL"), bold = T)
#
#
#
# # sq <- summary.quanti( data, x = mpg, group = vs)$summary
# # sq_s <- data.frame(sq$summary)
#
# # sq_sum <- t(sq_s %>% select(-variable,-p.value, - n))
#
# # df %<>% bind_rows(data.frame(variable = sq$columns,
# #                              levels= rownames(sq_sum),
# #                             summary = sq_sum[,1],
# #                             p.value = unlist(c(sq_s %>% select(p.value), rep("", nrow(sq_sum)-1))),
# #                             n =  unlist(c(sq_s %>% select(n), rep("", nrow(sq_sum)-1)))))
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# # debug(quickCor)
# quickCor(x = "qsec", y = "mpg", dat = mtc_bis)
# quickCor(x = qsec, y = mpg, dat = mtc_bis)
# mtc_bis %>%quickCor(x = "qsec", y = "mpg")
# mtc_bis %>%quickCor(x = qsec, y = mpg)
# undebug(quickCor)
#
#
#
#
# debug(summary.quali)
#
# mtc_bis <- mtc_bis
# mtc_bis %>% summary.quali( x = gear, group = vs)
# summary.quali( mtc_bis, x = gear, group = vs)
# mtc_bis %>% summary.quali( x = "gear", group = "vs")
# summary.quali( mtc_bis, x = "gear", group = "vs")
#
# mtc_bis %>% summary.quanti( x = qsec)
# summary.quanti( mtc_bis, x = qsec)$summary %>% kable_ueb()
# mtc_bis %>% summary.quanti( x = "qsec")
# summary.quanti( mtc_bis, x = "qsec", prep.tab = T)
#
#
# mtc_bis %>% summary.quanti( x = qsec, group = vs)
# summary.quanti( mtc_bis, x = qsec, group = vs)
# mtc_bis %>% summary.quanti( x = "qsec", group = "vs")
# summary.quanti( mtc_bis, x = "qsec", group = "vs")
#
# mtc_bis %>% summary.quanti( x = qsec)
# summary.quanti( mtc_bis, x = qsec)$summary %>% kable_ueb()
# mtc_bis %>% summary.quanti( x = "qsec")
# summary.quanti( mtc_bis, x = "qsec")
#
#
#
#
# # debug(desc_group)
# mtc_bis %>% desc_group(covariates = "qsec", group = "vs")
# mtc_bis %>% desc_group(covariates = "gear", group = "vs")
# mtc_bis %>% desc_group(covariates = qsec, group = vs)
# desc_group(covariates = "qsec", group = "vs", data = mtc_bis)
# desc_group(covariates = qsec, group = vs, data = mtc_bis)
#
#
# desc_group(covariates = "qsec", data = mtc_bis)
# desc_group(covariates = c("qsec", "mpg", "am") , group = "vs", data = mtc_bis)
#
# desc_group(covariates = qsec, group = vs, data = mtc_bis)
# desc_group(covariates = gear, group = vs, data = mtc_bis)
# mtc_bis %>% desc_group(covariates = qsec, group = vs)
# mtc_bis %>% desc_group(covariates = c(qsec, mpg, gear), group = vs)
#
#
#
# # undebug(desc_group)
#
#
# desc_group <- function(data,
#                        frml = NULL,
#                        covariates,
#                        group = NULL,
#                        method = "non-param",
#                        caption = NULL,
#                        font_size = 13,
#                        width_lev = "8em",
#                        byrow = FALSE,
#                        show.pval.adj = FALSE,
#                        pval_cut = 0.05,
#                        col.background = "#993489",
#                        col.varsel = "#ebe0e9",
#                        show.pval = TRUE,
#                        show.all = TRUE,
#                        show.n = TRUE,
#                        paired = FALSE,
#                        idvar = NULL,
#                        prep2sum = TRUE,
#                        include.NA = FALSE,
#                        patt.NA = "No",
#                        ...){
#
#   ## Les 3 seguents linies permeten pasar el nom de la variable com a text o estil tidyverse
#
#   covariates <- names(data %>% select({{covariates}}))
#   group <- names(data %>% select({{group}}))
#
#   ## comprobacions
#   if (is.null(frml) & !is.null(group) ) {
#     if (!group %in% names(data)) stop("The variable/s '", group, "' do not exist.")}
#   if (is.null(frml) ) {
#     if (any(!covariates %in% names(data))) {stop("The variable/s '",
#                                                  paste0(covariates[!covariates %in% names(data)], collapse = "' , '"),
#                                                  "' do not exist.")}}
#
#   if (!is.null(group) & all(is.na(data[,group]))) {stop("Variable '", group, "' is empty")}
#   if (paired) {
#     names(data)[names(data) == idvar] <- "id"
#     idvar <- "id"
#   }
#
#
#   if (!show.pval)  pval_cut <- -1
#   ## en el cas de que hi hagi formula seleccionem el grup i les covariates
#   if (!is.null(frml)) {
#     covariates <- rhs.vars(frml)
#     if (!is.null(lhs.vars(frml))) {group <- lhs.vars(frml)}
#   }
#
#   ## Seleccionem variables i etiquetes
#   data <- data[,names(data) %in% c(covariates,group,idvar)]
#   if (!is.null(group)) {
#     data[,group] <- factor_ueb(data[,group])
#     varname_group <- ifelse( Hmisc::label(data[,group]) != "", Hmisc::label(data[,group]), group)
#   }
#
#   ## eliminem variables buides
#   if (length(covariates ) != 1 | !is.null(group) ) {
#     emptyvar <- colSums(is.na(data)) != nrow(data)
#     var2del <- names(emptyvar[which(emptyvar == FALSE)])
#     if (length(var2del) > 0 ) {
#       warning(paste0("Las variable ",var2del, " ha sido eliminada. Todos sus valores son NA. \n" ))
#       data <- data[,!names(data) %in% var2del]
#     }}
#   if (is.null(dim(data))) {
#     lbl <- Hmisc::label(data)
#     data <- data.frame(data)
#     names(data) <- covariates
#     Hmisc::label(data) <- lbl
#   }
#
#
#   ## guardem class de cada variable
#   class_data <- unlist(lapply(data, function(x) class(x)[length(class(x))]))
#   class_data[which(class_data == "numeric" | class_data == "integer")] <- "numeric"
#   class_data <- class_data[!names(class_data) %in% c(group,idvar)]
#
#   if (any(class_data == "character")) {
#     message("La variable/s '",
#             paste0(names(class_data)[class_data == "character"],collapse = "' , '"),
#             "' es tipo caracter y no se ha analizado")
#     covariates <- covariates[!covariates %in% names(class_data)[class_data == "character"]]
#   }
#
#   ## realitzem analisis descriptiu i/o comparatiu
#   list_var <- list()
#   for (i in seq_along(class_data)) {
#     list_var[[names(class_data)[i]]] <- switch(class_data[i],
#                                                "numeric" = summary.quanti( x = names(class_data)[i] , group = group ,
#                                                                            method = method, data = data, prep2sum = prep2sum,
#                                                                            show.pval = show.pval, paired = paired, idvar = idvar, var.tidy=FALSE, ... ) ,
#                                                "factor" = summary.quali( x = names(class_data)[i], group = group ,data = data, byrow = byrow,
#                                                                          show.pval = show.pval, include.NA = include.NA, patt.NA = patt.NA, var.tidy=FALSE,...),
#                                                "character" = next()
#     )
#   }
#   list_var_sum <- lapply(list_var, function(x)x[["summary"]])
#   results <- do.call("rbind", list_var_sum)
#   # results <- plyr::rbind.fill(list_var_sum)
#
#   pvalues <- unlist(lapply(list_var, function(x)x[["pval"]]))
#
#
#   if (show.pval.adj) {
#     if (anyNA(results$p.value)) stop("P.value NA")
#     results$p.val.adj <- NA
#     results$p.val.adj[which(results$p.value != "")] <- round(p.adjust(as.numeric(as.character(pvalues)), method = "BH"),2)
#     results$p.val.adj[which(results$p.value == "")] <- ""
#     results <- results[,c(names(results)[1:which(names(results) == "p.value")], "p.val.adj", "n") ]
#     results$p.val.adj[which(results$p.val.adj != "" & results$p.val.adj < 0.001)] <- "<0.001"
#   }
#   ## Caption de la taula final
#   footnote <- NULL
#   typevar <- c("factor", "numeric")
#   for (i in seq_along(typevar)) {
#     desc <- unique(lapply(list_var[names(list_var) %in% names(class_data)[class_data == typevar[i]] ], function(x)x[["methods"]]))
#     tst <- paste0(unique(lapply(list_var[names(list_var) %in% names(class_data)[class_data == typevar[i]]], function(x)x[["txt_test"]])), collapse = "")
#     footnote <- unique(gsub("NULL","",c(footnote, paste0(desc, tst))))
#   }
#
#
#   # footnote <-  do.call("cbind", unique(list_var_met))
#   if (is.null(caption)) {
#     caption <- ifelse(is.null(group),
#                       "Summary statistics table",
#                       paste0("Summary of results by groups of ", varname_group))
#   }
#   ## CREACIO DE LA TAULA FINAL
#   # variables per files
#   var <- sapply(strsplit(rownames(results), ".", fixed = T),"[[", 1)
#
#   ## alineament a la taula
#   align = rep("c",ncol(results))
#   align[names(results) == "levels"] <- "l"
#   # results <- results[,!names(results) %in% "variable"]
#   # groups_row <- table(var)[unique(var)]
#
#   ## parametres per donar color a les variables amb p.value inferior a punt de tall
#   if (show.pval.adj) {
#     pval_valid <- results$p.val.adj
#   }else{
#     pval_valid <- results$p.value
#   }
#
#   pval_trunc <- as.numeric(sub("su.*", "",gsub("<","",pval_valid,
#                                                fixed = T)))
#   condition <- pval_trunc > pval_cut | is.na(pval_trunc)
#   if (!all(condition)) {
#     var_pval_cut <- var[which(!condition)]
#     colorRow <- which(var %in% var_pval_cut )
#   }else{
#     var_pval_cut <- NA
#   }
#   # colorRow <- which(!condition)
#   # groups_row <- cumsum(groups_row)
#   options(knitr.kable.NA = '')
#   ## Taula HTML
#   if (!show.pval) {
#     results <- results[,!names(results) %in% c("p.value")]
#     pvalues <- NA}
#   if (!show.all) {
#     results <- results[,!names(results) %in% c("ALL")]
#   }
#   if (!show.n) {
#     results <- results[,!names(results) %in% c("n")]
#   }
#
#   ## Afegim percentatges als titols
#   results  <- results %>% rename_at(levels(data[,group]), ~ paste0(levels(data[,group]), "<br>",
#                                                                    table(data[,group])," (",round(prop.table(table(data[,group]))*100,2),"%)"  ))
#
#
#
#
#   results_ht <- results %>%
#     # mutate(p.value = cell_spec(p.value, "html", color = ifelse(condition,"black", "white"),
#     #                            background = ifelse(condition, "white", "#993489"))) %>%
#     kable(escape = F, row.names = F,align = align, caption = caption)  %>%
#     kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
#                   font_size = font_size, full_width = F, fixed_thead = T) %>%
#     row_spec(0,background = col.background, color = "white") %>%
#     column_spec(which(names(results) == "levels"), width_max = width_lev) %>%
#     column_spec(which(names(results) == "variable"), bold = T)   %>%
#     column_spec(which(names(results) == "ALL"), bold = T)   %>%
#     add_footnote(footnote, escape = F,
#                  notation = "symbol" )
#
#   if (!is.null(group) & (sum(pval_trunc < pval_cut, na.rm = T) != 0)) {
#     results_ht <- results_ht %>%
#       row_spec(colorRow, bold = F, color = "black",background = col.varsel ) }#%>%
#   # pack_rows(groups_row ,hline_after = F, indent = F)
#
#
#   return(list(group = group, covariates = covariates,
#               selVar = var_pval_cut,
#               pvalues = as.numeric(as.character(pvalues)),df_all = results,results = results_ht))
# }
