#' @title Gráficos descriptivos con estadísticos (estilo ggstatsplot) coherentes con desc_group()
#'
#' @description
#' Genera un gráfico por variable con el aspecto de \code{ggstatsplot} (violín + boxplot + puntos en
#' numéricas; barras apiladas en factores), el test global en el subtítulo y, si \code{group} tiene más
#' de dos niveles, los corchetes del post-hoc.
#'
#' \strong{No calcula ningún test propio}: el test global se lee de \code{desc_group()} (\code{res_desc})
#' o, si no se indica, de \code{summary.quanti()} / \code{summary.quali()} (las mismas funciones que usa
#' \code{desc_group()}); el post-hoc se lee de \code{desc_group_posthoc()} (\code{res_posthoc}) o se calcula
#' con ella. Por eso los p-valores del gráfico coinciden con los de las tablas.
#'
#' @param data data frame con las variables.
#' @param covariates variables a graficar: texto (p. ej. \code{res$selVar}) o estilo tidyverse.
#' @param group variable de agrupación (factor). Texto o estilo tidyverse.
#' @param method \code{'param'} o \code{'non-param'} (también \code{'nonparam'}). Debe ser el mismo que en
#'   \code{desc_group()}. Valor por defecto: 'non-param'.
#' @param res_desc opcional: resultado de \code{desc_group()}. Si se indica, el test global (p, n y, si
#'   \code{desc_group(show.stat = TRUE)}, el estadístico) se lee de este objeto y no se recalcula; con Fisher
#'   simulado el p-valor es exactamente el de la tabla. \code{group} y \code{covariates} pueden omitirse.
#'   \code{method} debe ser el mismo que en \code{desc_group()} (el objeto no lo guarda).
#' @param res_posthoc opcional: resultado de \code{desc_group_posthoc()}. Si se indica, se dibuja tal cual
#'   (no se recalcula). Si es \code{NULL} y \code{group} tiene más de dos niveles, se llama a
#'   \code{desc_group_posthoc()} para las variables con p global < \code{pval_cut}.
#' @param type.num tipo de gráfico para variables numéricas: \code{'violin'} (violín + boxplot + puntos,
#'   por defecto), \code{'box'} (boxplot + puntos) o \code{'dots'} (puntos + línea de la media o mediana).
#' @param type.cat tipo de gráfico para factores: \code{'fill'} (barras apiladas en \%, por defecto),
#'   \code{'dodge'} (barras una al lado de otra, \% dentro de cada grupo) o \code{'count'} (barras apiladas en n).
#' @param p.label format de los p-valores del subtítulo y de los corchetes:
#'   \code{'value'} (por defecto; notación científica si p < 0.001), \code{'table'} (mismo formato que las
#'   tablas: \code{"<0.001"} o 3 decimales, como \code{desc_group()} en el subtítulo y como
#'   \code{desc_group_posthoc()} en los corchetes) o \code{'signif'} (\code{***} p < 0.001, \code{**} p < 0.01,
#'   \code{*} significativo, \code{ns}). La significación sigue el mismo criterio que las tablas: en el
#'   subtítulo, el de \code{desc_group()} (p redondeada a 3 decimales <= \code{pval_cut}); en los corchetes,
#'   el de \code{desc_group_posthoc()} (\code{p.adj < pval_cut}). No se ajusta ni se recalcula ningún p-valor.
#' @param show.posthoc \code{logical}. Si \code{TRUE}, dibuja los corchetes del post-hoc.
#' @param posthoc.display \code{'significant'} (solo parejas con \code{p.adj < pval_cut}), \code{'all'} o
#'   \code{'none'} (sin corchetes; equivale a \code{show.posthoc = FALSE}).
#' @param pval_cut punto de corte de significación (test global y \code{p.adj}). Valor por defecto: 0.05.
#' @param correct \code{logical}. Corrección de Yates en Chi-cuadrado (igual que \code{desc_group()}).
#' @param bw \code{logical}. Si \code{TRUE}, añade los puntos individuales (jitter) en \code{'violin'} y
#'   \code{'box'}. En \code{'dots'} los puntos se muestran siempre.
#' @param show.freq \code{logical}. Si \code{TRUE}, muestra los porcentajes (o las n con \code{'count'}) en las barras.
#' @param color.centre color del punto de media (\code{'param'}) o mediana (\code{'non-param'}).
#' @param size.title,size.subtitle tamaño del título y del subtítulo.
#' @param size.bracket tamaño del texto de los corchetes.
#' @param size.freq tamaño del texto de los porcentajes.
#' @param angle_x \code{'horizontal'}, \code{'diagonal'} o \code{'vertical'}.
#' @param legend.position posición de la leyenda en factores.
#' @param print.plots \code{logical}. Si \code{TRUE} (y \code{topdf = FALSE}), imprime los gráficos.
#' @param topdf \code{logical}. Si \code{TRUE}, guarda los gráficos con \code{ggplot_to_pdf()}.
#' @param nameFile nombre del PDF.
#' @param rowcol filas y columnas por página del PDF.
#' @param ... argumentos para \code{desc_group_posthoc()} (p. ej. \code{p.adjust.method}). Solo se usan
#'   si \code{res_posthoc = NULL}.
#'
#' @return Invisiblemente, una lista:
#' \describe{
#'   \item{group}{nombre de la variable de agrupación.}
#'   \item{covariates}{variables graficadas.}
#'   \item{method}{método usado.}
#'   \item{plots}{lista de objetos ggplot, con el nombre de cada variable.}
#'   \item{df_tests}{data.frame, una fila por variable: \code{variable, label, type, test, statistic,
#'   p, n, subtitle}.}
#'   \item{df_posthoc}{\code{df_posthoc} de \code{desc_group_posthoc()} más \code{label} (texto del
#'   corchete) e \code{y.position} (\code{NA} si no se dibuja); \code{NULL} si no hay post-hoc.}
#' }
#'
#' @details
#' \itemize{
#'   \item El test global es el de \code{desc_group()} con \code{include.NA = FALSE}: Kruskal-Wallis /
#'   Mann-Whitney (\code{'non-param'}), ANOVA / Student's T (\code{'param'}), Chi-cuadrado o Fisher.
#'   \item Solo se muestra lo que devuelven \code{summary.quanti()} / \code{summary.quali()}: nombre del
#'   test (de \code{txt_test}), estadístico (\code{show.stat = TRUE}, redondeado a 3 decimales), p-valor y
#'   n. Los grados de libertad no se devuelven y no se muestran.
#'   \item Fisher en tablas mayores de 2x2: \code{summary.quali()} calcula el p-valor por simulación
#'   (\code{simulate.p.value = TRUE}), por lo que puede variar ligeramente respecto a la tabla de
#'   \code{desc_group()}. El subtítulo lo indica como \code{"Fisher's exact (simulated p)"}.
#'   \item El punto central es la media con \code{'param'} y la mediana con \code{'non-param'}.
#'   \item El método de ajuste (\code{p.adjust.method}) no es un argumento: lo fija
#'   \code{desc_group_posthoc()}. Se lee de \code{df_posthoc$p.adjust.method} si existe y, si no, de la nota
#'   al pie de su tabla (\code{"p.adj: holm"}). Sin \code{res_posthoc}, el post-hoc se calcula con
#'   \code{desc_group_posthoc()} y el método se le pasa por \code{...}.
#'   \item No hay versión para datos apareados ni tamaños de efecto (no están en las tablas).
#' }
#'
#' @seealso \code{\link{desc_group}}, \code{\link{desc_group_posthoc}}, \code{\link{ggplot_to_pdf}}
#'
#' @importFrom ggplot2 ggplot aes geom_violin geom_boxplot geom_jitter geom_col geom_text geom_segment
#'   stat_summary scale_x_discrete scale_y_continuous scale_fill_discrete labs theme theme_minimal
#'   element_text element_blank element_rect margin position_stack position_dodge
#' @importFrom scales percent percent_format
#' @importFrom stringr str_wrap
#'
#' @examples
#'  # set.seed(1)
#'  # dat <- data.frame(MUT = factor(rep(c("A", "B", "C"), each = 20)),
#'  #                   size = rnorm(60, mean = rep(c(0, 1, 2), each = 20)),
#'  #                   var = factor(sample(c("Yes", "no"), 60, replace = TRUE)))
#'  # gs <- desc_ggstats(dat, covariates = c(size, var), group = MUT)
#'  # gs$df_tests
#'  # ## con el post-hoc ya calculado (flujo habitual de los informes)
#'  # res <- desc_group(data = dat, covariates = c("size", "var"), group = "MUT")
#'  # res_ph <- desc_group_posthoc(data = dat, covariates = res$selVar, group = "MUT")
#'  # gs <- desc_ggstats(dat, covariates = c(size, var), group = MUT, res_posthoc = res_ph)
#'  # ## test global y post-hoc leidos de las tablas, sin recalcular nada
#'  # res <- desc_group(data = dat, covariates = c("size", "var"), group = "MUT", show.stat = TRUE)
#'  # gs <- desc_ggstats(dat, res_desc = res, res_posthoc = res_ph)
#'  # gs <- desc_ggstats(dat, res_desc = res, res_posthoc = res_ph,
#'  #                    type.num = "box", type.cat = "dodge")
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
desc_ggstats <- function(data,
                         covariates,
                         group,
                         method = "non-param",
                         res_desc = NULL,
                         res_posthoc = NULL,
                         type.num = "violin",
                         type.cat = "fill",
                         p.label = "value",
                         show.posthoc = TRUE,
                         posthoc.display = "significant",
                         pval_cut = 0.05,
                         correct = FALSE,
                         bw = TRUE,
                         show.freq = TRUE,
                         color.centre = "#993489",
                         size.title = 10,
                         size.subtitle = 8,
                         size.bracket = 2.8,
                         size.freq = 2.5,
                         angle_x = "horizontal",
                         legend.position = "right",
                         print.plots = TRUE,
                         topdf = FALSE,
                         nameFile = "descriptive_stats_plots.pdf",
                         rowcol = c(1, 1),
                         ...) {

  ## 1. Arguments ---------------------------------------------------------------
  method <- switch(method,
                   "param" = "param",
                   "non-param" = ,
                   "nonparam" = "non-param",
                   stop("El argumento 'method' debe ser 'param' o 'non-param'."))
  posthoc.display <- match.arg(posthoc.display, c("significant", "all", "none"))
  type.num <- match.arg(type.num, c("violin", "box", "dots"))
  type.cat <- match.arg(type.cat, c("fill", "dodge", "count"))
  p.label <- match.arg(p.label, c("value", "table", "signif"))
  if (posthoc.display == "none") show.posthoc <- FALSE

  data <- data |> as.data.frame()

  ## amb 'res_desc' (desc_group) 'group' i 'covariates' es poden ometre: es llegeixen de l'objecte
  if (!is.null(res_desc)) {
    if (is.null(res_desc$df_all) || is.null(res_desc$pvalues) || is.null(res_desc$group)) {
      stop("'res_desc' debe ser el resultado de desc_group() con 'group' y show.pval = TRUE.")
    }
    ## variable de cada fila de df_all (mateix criteri que desc_group() per a 'selVar')
    var_row <- sapply(strsplit(rownames(res_desc$df_all), ".", fixed = TRUE), "[[", 1)
    vars_dg <- unique(var_row)
    if (missing(group)) group <- res_desc$group
    if (missing(covariates)) covariates <- vars_dg
  }

  ## noms com a text (p. ej. selVar, que pot ser NA) o estil tidyverse (mateix criteri que desc_group_posthoc)
  cov_chr <- tryCatch(covariates, error = function(e) NULL)
  if (is.character(cov_chr) || (is.logical(cov_chr) && all(is.na(cov_chr)))) {
    cov_chr <- as.character(cov_chr)
    covariates <- unique(cov_chr[!is.na(cov_chr) & cov_chr != ""])
  } else {
    covariates <- names(data |> dplyr::select({{covariates}}))
  }
  grp_chr <- tryCatch(group, error = function(e) NULL)
  group <- if (is.character(grp_chr)) grp_chr else names(data |> dplyr::select({{group}}))


  ## 2. Comprovacions -------------------------------------------------------------
  if (length(group) != 1) stop("Debe indicarse una sola variable 'group'.")
  if (!group %in% names(data)) stop("The variable/s '", group, "' do not exist.")
  covariates <- setdiff(covariates, group)
  if (length(covariates) == 0) {
    message("No hay variables para graficar.")
    return(invisible(NULL))
  }
  if (any(!covariates %in% names(data))) stop("The variable/s '",
                                              paste0(covariates[!covariates %in% names(data)], collapse = "' , '"),
                                              "' do not exist.")
  if (all(is.na(data[, group]))) stop("Variable '", group, "' is empty")

  ## 'group' com a desc_group(): factor_ueb() sense droplevels()
  data[, group] <- factor_ueb(data[, group])
  lbl_group <- ifelse(Hmisc::label(data[, group]) != "", Hmisc::label(data[, group]), group)

  class_data <- unlist(lapply(data[, covariates, drop = FALSE], function(x) class(x)[length(class(x))]))
  class_data[which(class_data == "numeric" | class_data == "integer")] <- "numeric"
  if (any(!class_data %in% c("numeric", "factor"))) {
    message("La variable/s '",
            paste0(names(class_data)[!class_data %in% c("numeric", "factor")], collapse = "' , '"),
            "' no es num\u00e9rica ni factor y no se ha graficado")
    class_data <- class_data[class_data %in% c("numeric", "factor")]
  }
  if (length(class_data) == 0) stop("Ninguna variable num\u00e9rica o factor para graficar.")


  ## 3. Test global -------------------------------------------------------------------
  ##  - amb 'res_desc': es llegeix de desc_group() (p, n i, si show.stat = TRUE, l'estadistic)
  ##  - sense 'res_desc': summary.quanti() / summary.quali() (les mateixes funcions que desc_group())
  if (!is.null(res_desc)) {
    if (!identical(res_desc$group, group)) {
      stop("'res_desc' se calcul\u00f3 con group = '", res_desc$group, "' y no con '", group, "'.")
    }
    if (length(vars_dg) != length(res_desc$pvalues)) {
      stop("No se pueden asociar los p-valores de 'res_desc' a sus variables (show.pval = FALSE?).")
    }
    p_dg <- stats::setNames(res_desc$pvalues, vars_dg)
    da <- res_desc$df_all
    falten <- setdiff(names(class_data), vars_dg)
    if (length(falten) > 0) {
      stop("Las variables '", paste(falten, collapse = "' , '"), "' no est\u00e1n en 'res_desc' ",
           "(o su nombre contiene un punto: desc_group() lo trunca).")
    }
  }

  list_tests <- list()
  for (var_i in names(class_data)) {

    if (!is.null(res_desc)) {

      fila <- which(var_row == var_i)[1]
      p_i <- unname(p_dg[var_i])
      n_i <- if ("n" %in% names(da)) suppressWarnings(as.numeric(as.character(da$n[fila]))) else NA_real_
      stat_i <- if ("stat" %in% names(da)) suppressWarnings(as.numeric(as.character(da$stat[fila]))) else NA_real_
      ## nom del test: mateixa regla de seleccio que summary.quanti() / summary.quali() (no es calcula cap test)
      test_i <- .test_rule_ueb(data[, var_i], data[, group], class_data[[var_i]], method)

    } else {

      ## show.stat = TRUE i show.n = TRUE: l'estadistic i la n surten de la mateixa crida que la p
      sq <- try(switch(class_data[[var_i]],
                       "numeric" = summary.quanti(data = data, x = var_i, group = group, method = method,
                                                  show.pval = TRUE, show.stat = TRUE, show.all = FALSE,
                                                  show.n = TRUE, prep2sum = TRUE, var.tidy = FALSE),
                       "factor" = summary.quali(data = data, x = var_i, group = group,
                                                show.pval = TRUE, show.stat = TRUE, show.all = FALSE,
                                                show.n = TRUE, correct = correct, var.tidy = FALSE)), silent = TRUE)
      if (inherits(sq, "try-error")) {
        warning("Error en el test de '", var_i, "': ", conditionMessage(attr(sq, "condition")))
        next
      }

      ## nomes es llegeix el que retornen summary.quanti() / summary.quali()
      p_i <- suppressWarnings(as.numeric(sq$pval))
      if (length(p_i) == 0) p_i <- NA_real_
      test_i <- .test_name_ueb(sq$txt_test)
      stat_i <- suppressWarnings(as.numeric(as.character(sq$summary$stat[1])))   # "." (Fisher) -> NA
      n_i <- suppressWarnings(as.numeric(as.character(sq$summary$n[1])))
    }

    ## Fisher en taules mes grans que 2x2: summary.quali() usa simulate.p.value = TRUE i la p es
    ## per Monte Carlo (en 2x2 fisher.test() ignora la simulacio i la p es exacta). S'indica al subtitol.
    if (identical(test_i, "Fisher's exact")) {
      cc <- !is.na(data[, var_i]) & !is.na(data[, group])
      if (!(nlevels(droplevels(data[cc, var_i])) == 2 && nlevels(data[, group]) == 2)) {
        test_i <- "Fisher's exact (simulated p)"
      }
    }

    list_tests[[var_i]] <- data.frame(
      variable = var_i,
      label = ifelse(Hmisc::label(data[, var_i]) != "", Hmisc::label(data[, var_i]), var_i),
      type = class_data[[var_i]],
      test = test_i, statistic = stat_i, p = p_i, n = n_i,
      subtitle = .subtitle_ueb(test_i, stat_i, p_i, n_i, p.label, pval_cut),
      stringsAsFactors = FALSE)
  }
  if (length(list_tests) == 0) stop("Ninguna variable v\u00e1lida.")
  df_tests <- do.call("rbind", list_tests)
  rownames(df_tests) <- NULL


  ## 4. Post-hoc: el calcula desc_group_posthoc() (aqui no s'ajusta res) ------------------
  df_ph <- NULL
  if (show.posthoc && nlevels(droplevels(data[, group])) > 2) {

    if (!is.null(res_posthoc)) {
      if (is.null(res_posthoc$df_posthoc)) stop("'res_posthoc' debe ser el resultado de desc_group_posthoc().")
      if (!identical(res_posthoc$group, group)) {
        stop("'res_posthoc' se calcul\u00f3 con group = '", res_posthoc$group, "' y no con '", group, "'.")
      }
      test_num <- unique(res_posthoc$df_posthoc$test[res_posthoc$df_posthoc$type == "numeric"])
      test_exp <- if (method == "param") "Tukey HSD" else "Dunn"
      if (length(test_num) > 0 && any(test_num != test_exp)) {
        stop("'res_posthoc' usa ", paste(test_num, collapse = ", "), " y method = '", method,
             "' corresponde a ", test_exp, ". Use el mismo 'method' en las dos funciones.")
      }
      df_ph <- res_posthoc$df_posthoc
      df_ph <- df_ph[df_ph$variable %in% df_tests$variable, , drop = FALSE]

      ## versio de desc_group_posthoc() sense la columna 'p.adjust.method': es llegeix de la nota al
      ## peu de la taula ("p.adj: holm"). Si no es troba, l'etiqueta queda "p.adj" (sense metode)
      if (!"p.adjust.method" %in% names(df_ph) && nrow(df_ph) > 0) {
        adj_foot <- .padj_from_footnote_ueb(res_posthoc$results)
        df_ph$p.adjust.method <- ifelse(df_ph$test == "Tukey HSD", "Tukey", adj_foot)
      }

    } else {
      ## mateix criteri que 'selVar' de desc_group(): p arrodonida a 3 decimals <= pval_cut
      vars_sig <- df_tests$variable[.sig_ueb(df_tests$p, pval_cut, "dg")]
      if (length(vars_sig) > 0) {
        res_ph <- desc_group_posthoc(data = data, covariates = vars_sig, group = group, method = method,
                                     pval_cut = pval_cut, correct = correct, ...)
        if (!is.null(res_ph)) df_ph <- res_ph$df_posthoc

        ## versio de desc_group_posthoc() sense la columna 'p.adjust.method': l'ajust es el que s'ha
        ## passat per '...' o, si no, el valor per defecte de desc_group_posthoc() (es llegeix de formals())
        if (!is.null(df_ph) && !"p.adjust.method" %in% names(df_ph)) {
          pam <- list(...)$p.adjust.method
          if (is.null(pam)) pam <- eval(formals(desc_group_posthoc)$p.adjust.method)
          pam <- match.arg(pam, stats::p.adjust.methods)
          df_ph$p.adjust.method <- ifelse(df_ph$test == "Tukey HSD", "Tukey", pam)
        }
      }
    }

    if (!is.null(df_ph) && nrow(df_ph) > 0) {
      ## p.label = "value": "p.adj (Holm) = 2.45e-10"; "table": "p.adj (Holm) < 0.001" (format de
      ## desc_group_posthoc()); "signif": nomes les estrelles (el metode i la llegenda van al peu)
      lbl_adj <- ifelse(is.na(df_ph$p.adjust.method), "p.adj",
                        paste0("p.adj (", .lbl_padj_ueb(df_ph$p.adjust.method), ")"))
      df_ph$label <- switch(p.label,
                            "value"  = paste0(lbl_adj, " = ", .fmt_p_ueb(df_ph$p_adj, "plot")),
                            "table"  = sub("= <", "< ", paste0(lbl_adj, " = ", .p_lbl_ueb(df_ph$p_adj, "ph")), fixed = TRUE),
                            "signif" = .p_lbl_ueb(df_ph$p_adj, "signif", pval_cut, "ph"))
      df_ph$y.position <- NA_real_
    } else {
      df_ph <- NULL
    }
  }


  ## 5. Grafics ---------------------------------------------------------------------
  plots <- list()
  for (k in seq_len(nrow(df_tests))) {

    var_i <- df_tests$variable[k]
    dd <- data.frame(y = data[, var_i], g = data[, group])
    dd <- dd[!is.na(dd$y) & !is.na(dd$g), , drop = FALSE]
    dd$g <- droplevels(dd$g)
    lev <- levels(dd$g)
    n_lev <- as.integer(table(dd$g))
    x_lbl <- paste0(lev, "\n(n = ", n_lev, ")")

    ## files del post-hoc d'aquesta variable que es dibuixen
    ph_v <- NULL
    if (!is.null(df_ph)) {
      idx <- which(df_ph$variable == var_i)
      if (posthoc.display == "significant") idx <- idx[!is.na(df_ph$p_adj[idx]) & df_ph$p_adj[idx] < pval_cut]
      if (length(idx) > 0) {
        ph_v <- df_ph[idx, , drop = FALSE]
        ph_v$idx <- idx
      }
    }

    ## peu del grafic
    caption_i <- NULL
    if (!is.null(ph_v)) {
      adj_i <- unique(ph_v$p.adjust.method)[1]
      caption_i <- paste0("Post-hoc: ", unique(ph_v$test)[1],
                          if (!is.na(adj_i)) paste0(", p.adj: ", .lbl_padj_ueb(adj_i)) else "",
                          if (posthoc.display == "significant") paste0(". Only pairs with p.adj < ", pval_cut) else "")
    }
    if (p.label == "signif") {
      caption_i <- paste(c(caption_i, paste0("*** p < 0.001, ** p < 0.01, * p < ", pval_cut,
                                       ", ns: no significativo (mismo criterio que las tablas)")),
                         collapse = "\n")
    }

    if (df_tests$type[k] == "numeric") {

      ## type.num: "violin" (violi + boxplot + punts) / "box" (boxplot + punts) / "dots" (punts + linia)
      ## el punt (o la linia) central es la mitjana amb 'param' i la mediana amb 'non-param'
      centre_fun <- if (method == "param") mean else stats::median
      yr <- range(dd$y)
      span <- diff(yr)
      if (span == 0) span <- max(abs(yr[1]), 1)
      step <- span * 0.09

      gg <- ggplot2::ggplot(dd, ggplot2::aes(x = g, y = y))
      gg <- switch(type.num,
        "violin" = gg +
          ggplot2::geom_violin(ggplot2::aes(fill = g), alpha = 0.2, colour = NA) +
          ggplot2::geom_boxplot(width = 0.15, fill = "white", alpha = 0.8, outlier.shape = if (bw) NA else 19) +
          (if (bw) ggplot2::geom_jitter(ggplot2::aes(colour = g), width = 0.15, height = 0, alpha = 0.6, size = 1.2)) +
          ggplot2::stat_summary(fun = centre_fun, geom = "point", size = 3, colour = color.centre),
        "box" = gg +
          ggplot2::geom_boxplot(ggplot2::aes(fill = g), width = 0.5, alpha = 0.3, outlier.shape = if (bw) NA else 19) +
          (if (bw) ggplot2::geom_jitter(ggplot2::aes(colour = g), width = 0.15, height = 0, alpha = 0.6, size = 1.2)) +
          ggplot2::stat_summary(fun = centre_fun, geom = "point", size = 3, colour = color.centre),
        "dots" = gg +
          ggplot2::geom_jitter(ggplot2::aes(colour = g), width = 0.15, height = 0, alpha = 0.7, size = 1.5) +
          ggplot2::stat_summary(fun = centre_fun, fun.min = centre_fun, fun.max = centre_fun,
                                geom = "crossbar", width = 0.5, colour = color.centre))

      gg <- gg +
        ggplot2::scale_x_discrete(labels = x_lbl) +
        ggplot2::labs(title = df_tests$label[k], subtitle = df_tests$subtitle[k], x = lbl_group, y = NULL,
                      caption = caption_i) +
        .theme_desc_ueb(size.title, size.subtitle) +
        ggplot2::theme(legend.position = "none", axis.text.x = .angle_x_ueb(angle_x))

      y0 <- yr[2] + step * 0.8
      tick <- step * 0.25

    } else {

      ## type.cat: "fill" (apilades en %) / "dodge" (barres al costat, % dins de cada grup) / "count" (apilades en n)
      tab <- as.data.frame(table(g = dd$g, y = droplevels(dd$y)), stringsAsFactors = FALSE)
      tab$g <- factor(tab$g, levels = lev)
      tab$y <- factor(tab$y, levels = levels(droplevels(dd$y)))
      tab$pct <- tab$Freq / stats::ave(tab$Freq, tab$g, FUN = sum)
      tab$txt <- ifelse(tab$Freq > 0, if (type.cat == "count") as.character(tab$Freq) else scales::percent(tab$pct, accuracy = 1), "")

      gg <- switch(type.cat,
        "fill" = ggplot2::ggplot(tab, ggplot2::aes(x = g, y = pct, fill = y)) +
          ggplot2::geom_col(colour = "black", linewidth = 0.3, width = 0.7) +
          ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, 1, 0.25)),
        "dodge" = ggplot2::ggplot(tab, ggplot2::aes(x = g, y = pct, fill = y)) +
          ggplot2::geom_col(colour = "black", linewidth = 0.3, width = 0.7, position = ggplot2::position_dodge(width = 0.8)) +
          ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)),
        "count" = ggplot2::ggplot(tab, ggplot2::aes(x = g, y = Freq, fill = y)) +
          ggplot2::geom_col(colour = "black", linewidth = 0.3, width = 0.7))

      gg <- gg +
        ggplot2::scale_x_discrete(labels = x_lbl) +
        ggplot2::scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
        ggplot2::labs(title = df_tests$label[k], subtitle = df_tests$subtitle[k], x = lbl_group,
                      y = if (type.cat == "count") "n" else "%", fill = NULL, caption = caption_i) +
        .theme_desc_ueb(size.title, size.subtitle) +
        ggplot2::theme(legend.position = legend.position, axis.text.x = .angle_x_ueb(angle_x))

      if (show.freq) {
        gg <- gg + if (type.cat == "dodge") {
          ggplot2::geom_text(ggplot2::aes(label = txt), position = ggplot2::position_dodge(width = 0.8),
                             vjust = -0.4, size = size.freq)
        } else {
          ggplot2::geom_text(ggplot2::aes(label = txt), position = ggplot2::position_stack(vjust = 0.5), size = size.freq)
        }
      }

      ## alçada maxima de les barres en l'escala de cada tipus (per situar els claudators)
      top <- switch(type.cat, "fill" = 1, "dodge" = max(tab$pct), "count" = max(table(dd$g)))
      step <- top * 0.1
      y0 <- top + step * (if (type.cat == "dodge") 1.2 else 0.75)
      tick <- step * 0.25
    }

    ## claudators del post-hoc
    if (!is.null(ph_v)) {
      br <- .brackets_ueb(ph_v, lev, y0 = y0, step = step)
      gg <- .add_brackets_ueb(gg, br, tick = tick, size = size.bracket)
      df_ph$y.position[ph_v$idx[br$row]] <- br$y
    }

    plots[[var_i]] <- gg
  }


  ## 6. Sortida ---------------------------------------------------------------------
  if (topdf) {
    ggplot_to_pdf(plots, row = rowcol[1], col = rowcol[2], name.file = nameFile)
  } else if (print.plots) {
    for (p in plots) print(p)
  }

  invisible(list(group = group,
                 covariates = df_tests$variable,
                 method = method,
                 plots = plots,
                 df_tests = df_tests,
                 df_posthoc = df_ph))
}



### helpers interns (no exportats) ---------------------------------------------
### Nomes donen format al que retornen summary.quanti(), summary.quali() i desc_group_posthoc(): no calculen tests.

## Format del p-valor
##  - 'table': "<0.001" o 3 decimals (mateix format que desc_group_posthoc())
##  - 'plot' : notacio cientifica si p < 0.001 (com ggstatsplot) o 3 decimals
.fmt_p_ueb <- function(p, style = c("table", "plot")) {
  style <- match.arg(style)
  p <- suppressWarnings(as.numeric(p))
  out <- switch(style,
                "table" = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
                "plot"  = ifelse(p < 0.001, formatC(p, format = "e", digits = 2), sprintf("%.3f", p)))
  ifelse(is.na(p), ".", out)
}


## Significacio amb el mateix criteri que les taules (no es recalcula cap test)
##   'dg': desc_group() pinta la variable si la p ARRODONIDA a 3 decimals es <= pval_cut
##         (condition <- pval_trunc > pval_cut; "<0.001" compta com 0.001)
##   'ph': desc_group_posthoc() pinta la parella si p.adj < pval_cut (p sense arrodonir)
.sig_ueb <- function(p, pval_cut = 0.05, rule = c("dg", "ph")) {
  rule <- match.arg(rule)
  p <- suppressWarnings(as.numeric(p))
  sig <- switch(rule,
                "dg" = ifelse(p < 0.001, 0.001, round(p, 3)) <= pval_cut,
                "ph" = p < pval_cut)
  !is.na(sig) & sig
}


## p-valor amb el mateix format que les taules del paquet (no s'ajusta ni es recalcula res)
##   'dg'    : com desc_group() / summary.quanti() / summary.quali(): "<0.001" o round(p, 3)
##   'ph'    : com desc_group_posthoc(): "<0.001" o sprintf("%.3f", p)
##   'signif': "ns" si no es significativa segons el criteri de la taula ('rule', veure .sig_ueb());
##             si ho es: "***" (< 0.001, la taula mostra "<0.001"), "**" (< 0.01) o "*"
##             amb 'dg' els talls s'apliquen a la p arrodonida que mostra desc_group()
##   NA o NaN -> "." (com a les taules)
.p_lbl_ueb <- function(p, style = c("dg", "ph", "signif"), pval_cut = 0.05, rule = c("dg", "ph")) {
  style <- match.arg(style)
  rule <- match.arg(rule)
  p <- suppressWarnings(as.numeric(p))
  out <- switch(style,
                "dg" = ifelse(p < 0.001, "<0.001", as.character(round(p, 3))),
                "ph" = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
                "signif" = {
                  p_tab <- if (rule == "dg") ifelse(p < 0.001, p, round(p, 3)) else p
                  ifelse(!.sig_ueb(p, pval_cut, rule), "ns",
                         ifelse(p_tab < 0.001, "***", ifelse(p_tab < 0.01, "**", "*")))
                })
  ifelse(is.na(p), ".", out)
}


## Format de l'estadistic
.fmt_num_ueb <- function(x, digits = 2) ifelse(is.na(x), ".", formatC(x, format = "f", digits = digits))


## Nom del test a partir de 'txt_test' de summary.quanti() / summary.quali() (no es recalcula res)
##   "<font size='1'> <br> p.value:  Kruskal-Wallis</font>"               -> "Kruskal-Wallis"
##   "<font size='1'> <br> p.value: <sup>3</sup>Fisher's exact</font>"    -> "Fisher's exact"
.test_name_ueb <- function(txt_test) {
  if (is.null(txt_test) || length(txt_test) == 0 || is.na(txt_test[1])) return(NA_character_)
  x <- gsub("<sup>.*?</sup>", "", txt_test[1], perl = TRUE)
  x <- gsub("<[^>]+>", "", x)
  x <- trimws(sub(".*p\\.value:", "", x))
  ## "Mann\u2013Whitney U" (guio llarg) -> "Mann-Whitney U": no depen de la codificacio del sistema
  gsub("Mann.{1,3}Whitney", "Mann-Whitney", x, useBytes = TRUE)
}


## Nom del test segons la regla de seleccio de summary.quanti() (linies 233-235) i summary.quali()
## (linia 170). Nomes decideix l'etiqueta: no calcula cap test. Dades independents.
.test_rule_ueb <- function(x, g, type, method) {
  if (type == "numeric") {
    k <- length(levels(g))
    if (method == "param") return(if (k > 2) "Anova" else "Student's T")
    return(if (k > 2) "Kruskal-Wallis" else "Mann-Whitney U")
  }
  cc <- !is.na(x) & !is.na(g)
  tb <- table(droplevels(x[cc]), g[cc])
  if (any(suppressWarnings(stats::chisq.test(tb)$expected) < 5)) "Fisher's exact" else "Chi-squared"
}


## Metode d'ajust a partir de la nota al peu de desc_group_posthoc()$results:
##   "... p.value: Dunn. p.adj: holm </font>"  -> "holm"
## Es descarta "Tukey" (Tukey HSD s'identifica per la columna 'test'). NA si no es troba o es ambigu.
.padj_from_footnote_ueb <- function(results) {
  h <- tryCatch(paste(as.character(results), collapse = " "), error = function(e) "")
  m <- regmatches(h, gregexpr("p\\.adj: [A-Za-z]+", h))[[1]]
  m <- setdiff(unique(sub("p\\.adj: ", "", m)), "Tukey")
  if (length(m) != 1 || !m %in% stats::p.adjust.methods) return(NA_character_)
  m
}


## Nom llegible del metode d'ajust (valors de stats::p.adjust.methods + "Tukey")
.lbl_padj_ueb <- function(m) {
  lbl <- c(holm = "Holm", hochberg = "Hochberg", hommel = "Hommel", bonferroni = "Bonferroni",
           BH = "BH", fdr = "FDR", BY = "BY", none = "none", Tukey = "Tukey")
  out <- unname(lbl[m])
  ifelse(is.na(out), m, out)
}


## Subtitol del grafic a partir del resultat de summary.quanti() / summary.quali()
## Nomes el que aquestes funcions retornen: test, estadistic (show.stat, 3 decimals), p i n.
## Els graus de llibertat no els retornen i no es mostren.
## (nomes text ASCII: evita problemes amb dispositius pdf() sense Unicode)
.subtitle_ueb <- function(test, statistic, p, n, p.label = "value", pval_cut = 0.05) {

  p_txt <- switch(p.label,
                  "value"  = paste0("p = ", .fmt_p_ueb(p, "plot")),
                  "table"  = sub("= <", "< ", paste0("p = ", .p_lbl_ueb(p, "dg")), fixed = TRUE),
                  "signif" = paste0("p: ", .p_lbl_ueb(p, "signif", pval_cut, "dg")))

  if (is.null(test) || is.na(test)) return(paste0(p_txt, ", n = ", n))

  st <- .fmt_num_ueb(statistic)
  head <- if (is.na(statistic)) test else switch(test,
                 "Kruskal-Wallis" = paste0("Kruskal-Wallis H = ", st),
                 "Anova" = paste0("ANOVA F = ", st),
                 "Student's T" = paste0("Student's T: t = ", st),
                 "Mann-Whitney U" = paste0("Mann-Whitney U: W = ", st),
                 "Chi-squared" = paste0("Chi-squared = ", st),
                 "Fisher's exact" = "Fisher's exact",
                 "Fisher's exact (simulated p)" = "Fisher's exact (simulated p)",
                 if (is.na(statistic)) test else paste0(test, ": ", st))

  paste0(head, ", ", p_txt, ", n = ", n)
}


## Posicio dels claudators del post-hoc
## ph: files de df_posthoc d'una variable (group1, group2, label); lev: nivells de l'eix X
## Els claudators curts van a sota (com ggstatsplot)
.brackets_ueb <- function(ph, lev, y0, step) {

  if (is.null(ph) || nrow(ph) == 0) {
    return(data.frame(x = numeric(0), xend = numeric(0), y = numeric(0), label = character(0), row = integer(0)))
  }
  x1 <- match(ph$group1, lev)
  x2 <- match(ph$group2, lev)
  br <- data.frame(x = pmin(x1, x2), xend = pmax(x1, x2), label = ph$label, row = seq_len(nrow(ph)),
                   stringsAsFactors = FALSE)
  br <- br[!is.na(br$x) & !is.na(br$xend), ]
  br <- br[order(br$xend - br$x, br$x), ]
  br$y <- y0 + step * (seq_len(nrow(br)) - 1)
  br
}


## Afegeix els claudators a un ggplot (eix X discret: posicions 1, 2, ...)
.add_brackets_ueb <- function(gg, br, tick, size) {
  if (nrow(br) == 0) return(gg)
  ## l'extrem de les marques es guarda com a columna: si aes() fes referencia a 'tick', ggplot2
  ## l'avaluaria en dibuixar el grafic (avaluacio mandrosa) i agafaria el 'tick' de l'ultima variable del bucle
  br$y_tick <- br$y - tick
  gg +
    ggplot2::geom_segment(data = br, ggplot2::aes(x = x, xend = xend, y = y, yend = y), inherit.aes = FALSE) +
    ggplot2::geom_segment(data = br, ggplot2::aes(x = x, xend = x, y = y, yend = y_tick), inherit.aes = FALSE) +
    ggplot2::geom_segment(data = br, ggplot2::aes(x = xend, xend = xend, y = y, yend = y_tick), inherit.aes = FALSE) +
    ggplot2::geom_text(data = br, ggplot2::aes(x = (x + xend) / 2, y = y, label = label),
                       vjust = -0.4, size = size, inherit.aes = FALSE)
}


## Tema comu dels grafics descriptius (el mateix que tenia desc_ggplot())
.theme_desc_ueb <- function(size.title = 10, size.subtitle = size.title - 2) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = size.title),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = size.subtitle),
      plot.caption = ggplot2::element_text(size = size.subtitle - 1, colour = "grey30"),
      panel.grid.major = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),   # PNG sense fons transparent
      plot.margin = ggplot2::margin(10, 10, 10, 10)
    )
}


## Orientacio de les etiquetes de l'eix X
.angle_x_ueb <- function(angle_x = "horizontal") {
  switch(angle_x,
         "horizontal" = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1),
         "diagonal"   = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
         "vertical"   = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
         ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1))
}


## variables usades dins aes() (evita les NOTE "no visible binding" de R CMD check)
utils::globalVariables(c("x", "xend", "y", "label", "g", "pct", "Freq", "txt", "y_tick"))