#' @rdname desc_group
#' @export
descGroup <- function(...) {
  .Deprecated("desc_group") #include a package argument, too
  desc_group(...)
}

#' @title Tabla descriptiva de covariables según una variable de grupo
#'
#' @description
#' Genera una tabla descriptiva de las covariables indicadas, globalmente o según los
#' niveles de una variable de grupo. Las variables numéricas se resumen con
#' \code{summary.quanti()} y las categóricas con \code{summary.quali()}, e incluye el
#' p-valor del test de comparación entre grupos. Devuelve la tabla en formato
#' data.frame y formateada en HTML mediante \pkg{kableExtra}, y resalta las variables
#' con p-valor inferior o igual a \code{pval_cut}.
#'
#' @param data Data frame, lista o entorno (u objeto convertible a data frame mediante
#'   \code{as.data.frame}) que contiene las variables del modelo. Si no se encuentran en
#'   \code{data}, las variables se toman de \code{environment(formula)}.
#' @param frml Fórmula. La parte derecha de \code{~} debe contener los términos de forma
#'   aditiva y la parte izquierda el nombre de la variable de agrupación. La parte
#'   izquierda puede dejarse en blanco: en ese caso se calculan los descriptivos para
#'   toda la muestra y no se realiza ningún test.
#' @param covariates Nombres de las variables a describir (texto o selección tidyverse).
#' @param group Variable factor de agrupación (outcome). Por defecto \code{NULL}.
#' @param method Cadena de caracteres con el método del test: \code{"param"} o
#'   \code{"non-param"}. Por defecto \code{"non-param"}.
#' @param caption Vector de caracteres con el título o leyenda de la tabla.
#' @param font_size Valor numérico con el tamaño de letra de la tabla.
#' @param width_lev Ancho máximo de la columna de niveles. Por defecto \code{"8em"}.
#' @param byrow Lógico o \code{NA}. Los porcentajes de las variables categóricas se
#'   calculan por filas (\code{TRUE}), por columnas (\code{FALSE}) o sobre el total, de
#'   forma que filas y columnas suman 1 (\code{NA}). Por defecto \code{FALSE}: los
#'   porcentajes se calculan por columnas (dentro de cada grupo).
#' @param show.pval.adj Lógico. Si \code{TRUE}, añade una columna con los p-valores
#'   ajustados mediante la corrección de Benjamini-Hochberg. Por defecto \code{FALSE}.
#' @param pval_cut Punto de corte del p-valor. Las variables con p-valor inferior o igual
#'   a \code{pval_cut} se devuelven en \code{selVar} y se resaltan en la tabla con
#'   \code{col.varsel}. Por defecto 0.05.
#' @param col.background Color de fondo de la cabecera de la tabla HTML.
#' @param col.varsel Color de fondo de las filas de las variables seleccionadas según
#'   \code{pval_cut}.
#' @param show.pval \code{logical}. Si \code{TRUE}, muestra el valor p del test de asociación.
#' @param show.all \code{logical}. Si \code{TRUE}, añade columna con resultados globales (sin estratificar).
#' @param show.n \code{logical}. Si \code{TRUE}, añade una columna con el número total de observaciones.
#' @param show.stat \code{logical}. Si \code{TRUE}, añade el estadístico de prueba (Chi-cuadrado).
#' @param show.or \code{logical}. Si \code{TRUE} y \code{group} tiene exactamente dos niveles,
#'   añade la columna con la odds ratio (\code{OR}).
#' @param paired Lógico. Indica si los datos son apareados. Por defecto \code{FALSE}.
#' @param idvar Cadena de caracteres con el nombre de la variable identificadora.
#' @param prep2sum Lógico. Se pasa a \code{summary.quanti()}. Por defecto \code{TRUE}.
#' @param include.NA Lógico. Si \code{TRUE}, los valores perdidos se incluyen como
#'   categoría en las variables categóricas (se pasa a \code{summary.quali()}). Por
#'   defecto \code{FALSE}.
#' @param patt.NA Etiqueta utilizada para los valores perdidos (se pasa a
#'   \code{summary.quali()}). Por defecto \code{"No"}.
#' @param correct Lógico. Si \code{TRUE}, aplica la corrección de Yates en la prueba
#'   Chi-cuadrado (se pasa a \code{summary.quali()}). Por defecto \code{FALSE}.
#' @param full_width Lógico. Si \code{TRUE}, la tabla HTML ocupa todo el ancho
#'   disponible (se pasa a \code{kableExtra::kable_styling()}). Por defecto \code{FALSE}.
#' @param ... Argumentos adicionales para \code{summary.quanti} y \code{summary.quali} (p. ej. \code{nround}).
#'   Con \code{nround = "auto"} las variables cuantitativas usan decimales dinámicos; las cualitativas, 1 decimal.
#'
#' @return Una lista con:
#' \itemize{
#'   \item \code{group}: variable de agrupación utilizada.
#'   \item \code{covariates}: variables descritas.
#'   \item \code{selVar}: variables con p-valor inferior o igual a \code{pval_cut}
#'   (\code{NA} si no hay ninguna).
#'   \item \code{pvalues}: vector con los p-valores obtenidos.
#'   \item \code{df_all}: tabla resumen en formato data.frame.
#'   \item \code{results}: tabla formateada en HTML mediante \pkg{kableExtra}.
#' }
#'
#' @details
#' Desde mayo de 2025 la prueba Chi-cuadrado se ejecuta por defecto sin corrección de
#' Yates (\code{correct = FALSE}). Para replicar resultados anteriores, indique
#' \code{correct = TRUE} explícitamente.
#'
#' \code{descGroup()} está obsoleta (deprecated) y se mantiene solo por compatibilidad:
#' llama a \code{desc_group()} con los mismos argumentos. Utilice \code{desc_group()}.
#'
#' @seealso \code{\link{summary.quanti}}, \code{\link{summary.quali}},
#'   \code{\link{desc_group_numeric}}, \code{\link{desc_group_strat}},
#'   \code{\link{desc_group_rep}}
#'
#' @import kableExtra formula.tools
#'
#' @examples
#'  # set.seed(1)
#'  # dat <- df <- data.frame(MUT = factor(c(rep("A", 13),rep("B",13))),
#'  #                          var = factor(sample(c("Yes", "no"), 26, replace = TRUE)),
#'  #                          size = rnorm(26),
#'  #                          id = paste0("a",1:26))
#'  # dat$id <- as.character(dat$id)
#'  # res <- desc_group(group  = "MUT",covariates = names(dat), data = dat)
#'  # res$res
#'  # res <- desc_group(group  = "MUT",covariates = names(dat), data = dat, show.pval = FALSE)
#'  # res$res
#'  # desc_group(covariates = qsec, group = vs, data = mtc_bis)
#'  # desc_group(covariates = gear, group = vs, data = mtc_bis)
#'  # mtc_bis %>% desc_group(covariates = qsec, group = vs)
#'  # mtc_bis %>% desc_group(covariates = c(qsec, mpg, gear), group = vs)
#'
#' @keywords summary ci qualitative descriptive exploratory
#'
#' @author
#' Miriam Mota-Foix \email{mmota.foix@gmail.com}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
desc_group <- function(data,
                       frml = NULL,
                       covariates,
                       group = NULL,
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
                       show.all = TRUE,
                       show.n = TRUE,
                       show.stat = FALSE,
                       show.or = FALSE,
                       paired = FALSE,
                       idvar = NULL,
                       prep2sum = TRUE,
                       include.NA = FALSE,
                       patt.NA = "No",
                       correct = FALSE,
                       full_width = FALSE,
                       ...){

  message("A partir de mayo de 2025, la prueba chi-cuadrado se ejecuta por defecto con `correct = FALSE` (sin corrección de Yates).
          Para replicar resultados previos, especifique `correct = TRUE` explícitamente")


  ## Les 3 seguents linies permeten pasar el nom de la variable com a text o estil tidyverse

  data <- data %>% as.data.frame()
  covariates <- names(data %>% select({{covariates}}))
  group <- names(data %>% select({{group}}))

  ## comprobacions
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

  ## Seleccionem variables i etiquetes
  data <- data[,names(data) %in% c(covariates,group,idvar)]
  if (!is.null(group)) {
    data[,group] <- factor_ueb(data[,group])
    varname_group <- ifelse( Hmisc::label(data[,group]) != "", Hmisc::label(data[,group]), group)
  }

  ## eliminem variables buides
  if (length(covariates ) != 1 | !is.null(group) ) {
    emptyvar <- colSums(is.na(data)) != nrow(data)
    var2del <- names(emptyvar[which(emptyvar == FALSE)])
    if (length(var2del) > 0 ) {
      warning(paste0("Las variable ",var2del, " ha sido eliminada. Todos sus valores son NA. \n" ))
      data <- data[,!names(data) %in% var2del]
    }}
  if (is.null(dim(data))) {
    lbl <- Hmisc::label(data)
    data <- data.frame(data)
    names(data) <- covariates
    Hmisc::label(data) <- lbl
  }


  ## guardem class de cada variable
  class_data <- unlist(lapply(data, function(x) class(x)[length(class(x))]))
  class_data[which(class_data == "numeric" | class_data == "integer")] <- "numeric"
  class_data <- class_data[!names(class_data) %in% c(group,idvar)]

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
                                               "numeric" = summary.quanti( x = names(class_data)[i] , group = group ,
                                                                           method = method, data = data, prep2sum = prep2sum,
                                                                           show.pval = show.pval,show.stat = show.stat, show.or = show.or, paired = paired, idvar = idvar, var.tidy = FALSE, ... ) ,
                                               "factor" = summary.quali( x = names(class_data)[i], group = group ,data = data, byrow = byrow,
                                                                         show.pval = show.pval,show.stat = show.stat, show.or = show.or, include.NA = include.NA, patt.NA = patt.NA, var.tidy = FALSE,correct = correct,...),
                                               "character" = next()
    )
  }
  list_var_sum <- lapply(list_var, function(x)x[["summary"]])
  results <- do.call("rbind", list_var_sum)
  # results <- plyr::rbind.fill(list_var_sum)

  pvalues <- unlist(lapply(list_var, function(x)x[["pval"]]))


  if (show.pval.adj) {
    if (anyNA(results$p.value)) stop("P.value NA")
    results$p.val.adj <- NA
    results$p.val.adj[which(results$p.value != "")] <- round(p.adjust(as.numeric(as.character(pvalues)), method = "BH"),2)
    results$p.val.adj[which(results$p.value == "")] <- ""
    results <- results[,c(names(results)[1:which(names(results) == "p.value")], "p.val.adj", "n") ]
    results$p.val.adj[which(results$p.val.adj != "" & results$p.val.adj < 0.001)] <- "<0.001"
  }
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
    caption <- ifelse(is.null(group),
                      "Summary statistics table",
                      paste0("Summary of results by groups of ", varname_group))
  }
  ## CREACIO DE LA TAULA FINAL
  # variables per files
  var <- sapply(strsplit(rownames(results), ".", fixed = T),"[[", 1)

  ## alineament a la taula
  align = rep("c",ncol(results))
  align[names(results) == "levels"] <- "l"
  # results <- results[,!names(results) %in% "variable"]
  # groups_row <- table(var)[unique(var)]

  ## parametres per donar color a les variables amb p.value inferior a punt de tall
  if (show.pval.adj) {
    pval_valid <- results$p.val.adj
  }else{
    pval_valid <- results$p.value
  }

  pval_trunc <- as.numeric(sub("su.*", "",gsub("<","",pval_valid,
                                               fixed = T)))
  condition <- pval_trunc > pval_cut | is.na(pval_trunc)
  if (!all(condition)) {
    var_pval_cut <- var[which(!condition)]
    colorRow <- which(var %in% var_pval_cut )
  }else{
    var_pval_cut <- NA
  }
  # colorRow <- which(!condition)
  # groups_row <- cumsum(groups_row)
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
  if (!show.or) {
    results <- results[,!names(results) %in% c("OR")]
  }

  ## Afegim percentatges als titols
  results  <- results %>% rename_at(levels(data[,group]), ~ paste0(levels(data[,group]), "<br>",
                                                                   table(data[,group])," (",round(prop.table(table(data[,group]))*100,2),"%)"  ))




  results_ht <- results %>%
    # mutate(p.value = cell_spec(p.value, "html", color = ifelse(condition,"black", "white"),
    #                            background = ifelse(condition, "white", "#993489"))) %>%
    kable(escape = F, row.names = F,align = align, caption = caption, format = "html")  %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = font_size, full_width = full_width, fixed_thead = T) %>%
    row_spec(0,background = col.background, color = "white") %>%
    column_spec(which(names(results) == "levels"), width_max = width_lev) %>%
    column_spec(which(names(results) == "variable"), bold = T)   %>%
    column_spec(which(names(results) == "ALL"), bold = T)   %>%
    add_footnote(footnote, escape = F,
                 notation = "symbol" )

  if (!is.null(group) & (sum(pval_trunc < pval_cut, na.rm = T) != 0)) {
    results_ht <- results_ht %>%
      row_spec(colorRow, bold = F, color = "black",background = col.varsel ) }#%>%
  # pack_rows(groups_row ,hline_after = F, indent = F)


  return(list(group = group, covariates = covariates,
              selVar = var_pval_cut,
              pvalues = as.numeric(as.character(pvalues)),df_all = results,results = results_ht))
}

