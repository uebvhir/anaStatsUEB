#' @title Resumen descriptivo de una variable numérica con medidas repetidas
#'
#' @description
#' Genera un resumen descriptivo de una variable numérica en estudios con medidas
#' repetidas: estadísticos globales y por cada momento temporal (visita). Opcionalmente
#' realiza un ANOVA de medidas repetidas y prepara la salida para integrarla en
#' otras tablas resumen.
#'
#' @param data Data frame con las variables, en formato largo (una fila por sujeto y
#'   visita).
#' @param x Variable numérica a resumir: nombre de columna, con o sin comillas (si
#'   \code{var.tidy = TRUE}).
#' @param tiempo Variable que identifica los momentos temporales o visitas, en el
#'   mismo formato que \code{x}. Debe ser un factor; véase \sQuote{Details}.
#' @param id Nombre (texto) de la variable con el identificador único de sujeto.
#' @param format Formato de los saltos de línea de las celdas: \code{"html"} (por
#'   defecto), \code{"latex"} o \code{"R"}. Cualquier otro valor elimina los saltos
#'   de línea.
#' @param show.pval Lógico. Si es \code{TRUE} (por defecto), realiza el ANOVA de
#'   medidas repetidas y muestra el p-valor.
#' @param show.all Lógico. Si es \code{TRUE} (por defecto), añade la columna
#'   \code{ALL} con el resumen global.
#' @param show.n Lógico. Si es \code{TRUE} (por defecto), añade la columna \code{n}
#'   con el número de observaciones completas (filas), no de sujetos.
#' @param show.stat Lógico. Si es \code{TRUE}, muestra el estadístico F del ANOVA.
#'   Solo se aplica si \code{show.pval = TRUE} (por defecto, \code{FALSE}).
#' @param prep2sum Lógico. Si es \code{TRUE}, añade una columna \code{levels} vacía
#'   a \code{summary} para integrarla con otras funciones resumen (por defecto,
#'   \code{FALSE}).
#' @param prep.tab Lógico. Si es \code{TRUE}, añade a la salida \code{df_prep_tab},
#'   la tabla auxiliar que usa \code{desc_group_rep}. Requiere \code{show.n = TRUE} y
#'   \code{show.pval = TRUE} (por defecto, \code{FALSE}).
#' @param sub.ht Lógico. Si es \code{TRUE} (por defecto), añade un superíndice de
#'   nota al nombre de la variable. Con \code{FALSE} la función falla; véase
#'   \sQuote{Details}.
#' @param paired Reservado por compatibilidad; no tiene efecto (por defecto,
#'   \code{FALSE}).
#' @param var.tidy Lógico. Si es \code{TRUE} (por defecto), \code{x} y
#'   \code{tiempo} pueden darse con o sin comillas (\code{id} debe ser siempre
#'   texto). Si es \code{FALSE}, deben ser texto.
#' @param nround Número de decimales de los resultados (por defecto, 1).
#'
#' @return Una lista con:
#' \describe{
#'   \item{\code{rows}}{Nombre de la variable analizada.}
#'   \item{\code{txt_test}}{Texto con el nombre del test (\code{NULL} si
#'     \code{show.pval = FALSE}).}
#'   \item{\code{pval}}{P-valor del ANOVA de medidas repetidas (\code{NULL} si
#'     \code{show.pval = FALSE}).}
#'   \item{\code{txt_caption}}{Título de la tabla.}
#'   \item{\code{methods}}{Descripción del contenido de las celdas.}
#'   \item{\code{summary}}{Data frame con las columnas \code{variable}, una por
#'     visita, \code{ALL}, \code{p.value}, \code{stat} y \code{n} (según los
#'     argumentos \code{show.*}). Cada celda contiene n, media (SD), IC 95 \% de la
#'     media y mediana [Q1, Q3].}
#'   \item{\code{df_prep_tab}}{Solo si \code{prep.tab = TRUE}: data frame con
#'     \code{variable}, \code{levels}, \code{summary}, \code{p.value} y \code{n}.}
#' }
#'
#' @details
#' \itemize{
#'   \item Para cada visita y para el conjunto global (\code{ALL}) se calculan el
#'     número de observaciones, la media y la desviación estándar, el intervalo de
#'     confianza al 95 \% de la media (\code{Publish::ci.mean}) y la mediana con
#'     los cuartiles.
#'   \item Con \code{show.pval = TRUE}, el p-valor procede de
#'     \code{rstatix::get_anova_table(rstatix::anova_test())}, que por defecto
#'     (\code{correction = "auto"}) aplica la corrección de Greenhouse-Geisser a los
#'     factores que no cumplen la esfericidad (test de Mauchly con p <= 0.05).
#'   \item Los valores perdidos en \code{x} se excluyen en cada visita. Los niveles
#'     de \code{tiempo} sin observaciones aparecen con \code{NA}.
#'   \item Nota: \code{sub.ht = FALSE} produce un error. Si \code{tiempo} no es un
#'     factor, la conversión automática falla (\code{could not find function
#'     "\%>\%<-"}). \code{prep.tab = TRUE} falla con \code{show.n = FALSE} o
#'     \code{show.pval = FALSE}. La función carga y adjunta \pkg{rstatix} en cada
#'     llamada (\code{pacman::p_load()}).
#' }
#'
#' @seealso \code{\link{summary.quali.rep}}, \code{\link{summary.quanti}},
#'   \code{\link{desc_group_rep}}, \code{\link[rstatix]{anova_test}},
#'   \code{\link[rstatix]{get_anova_table}}, \code{\link[Publish]{ci.mean}}
#'
#' @examples
#' set.seed(1111)
#' df <- data.frame(
#'   id = rep(1:20, each = 3),
#'   visita = factor(rep(c("Basal", "Mes1", "Mes3"), 20)),
#'   colesterol = rnorm(60, 200, 30)
#' )
#' # Resumen descriptivo y ANOVA de medidas repetidas
#' res <- summary.quanti.rep(data = df, x = "colesterol", tiempo = "visita", id = "id")
#' res$summary
#' res$pval
#'
#' # Nombres sin comillas y estadístico F
#' res_st <- summary.quanti.rep(df, x = colesterol, tiempo = visita, id = "id",
#'                              show.stat = TRUE)
#' res_st$summary[, c("p.value", "stat", "n")]
#'
#' # Tabla auxiliar para desc_group_rep
#' res_tab <- summary.quanti.rep(df, "colesterol", "visita", "id", prep.tab = TRUE)
#' res_tab$df_prep_tab[, c("variable", "levels", "p.value", "n")]
#'
#' @keywords descriptives repeated-measures longitudinal summary statistics
#'
#' @importFrom dplyr %>% pull select sym
#' @importFrom rstatix anova_test get_anova_table
#' @importFrom Publish ci.mean
#' @importFrom pacman p_load
#' @importFrom stats aggregate complete.cases sd median quantile
#'
#' @author
#' Àlex Martí Barrera
#' 
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
summary.quanti.rep <- function (
    data,
    x,
    tiempo,
    id,
    format = "html",
    show.pval = TRUE,
    show.all = TRUE,
    show.n = TRUE,
    show.stat = FALSE,
    prep2sum = FALSE,
    prep.tab = FALSE,
    sub.ht = TRUE,
    paired = FALSE,
    var.tidy = TRUE,
    nround = 1
) {

  # Cargar paquete necesario para ANOVA de medidas repetidas
  p_load(rstatix)


  # Permite pasar variables en formato tidyverse o como texto

  if (var.tidy) {
    x <- gsub("\"", "", deparse(substitute(x)))
    try(tiempo <- gsub("\"", "", deparse(substitute(tiempo))), TRUE)
  }


  # Comprobaciones iniciales


  # Error si la variable está completamente vacía
  if (all(is.na(data %>% pull(x))))
    stop(paste0("The variable '", x, "' is empty"))

  # Error si la variable dependiente no es numérica
  if (is.factor(data %>% pull(x)))
    stop(paste0("La variable '", x, "' debe ser numérica"))

  # Convertir tiempo a factor si no lo es
  if (!is.factor(data %>% pull(tiempo))) {
    data %>% pull(tiempo) <- factor(data %>% pull(tiempo))
    warning(paste0("La variable '", tiempo, "' ha sido transformada a factor"))
  }


  # Parámetros de formato

  new_line <- switch(
    format,
    html = " <br> ",
    latex = " \\\\ ",
    R = " \n "
  )

  # Variables de trabajo
  xx <- data %>% pull(x)
  yy <- data %>% pull(tiempo)

  # Etiquetas (si existen mediante Hmisc::label)
  varname_x <- ifelse(
    Hmisc::label(data %>% pull(x)) != "",
    Hmisc::label(data %>% pull(x)),
    x
  )

  varname_tiempo <- ifelse(
    Hmisc::label(data %>% pull(tiempo)) != "",
    Hmisc::label(data %>% pull(tiempo)),
    tiempo
  )

  # Texto descriptivo que se mostrará en las tablas
  if (sub.ht) sub <- "<sub>2</sub>"

  txt_descriptive <- "<br> <font size='1'> 2: N <br> mean(sd) <br> [CI95% mean] <br> median[IQR] </font>"
  txt_caption <- txt_descriptive


  # Resumen descriptivo global (ALL)


  ci_uni <- ci.mean(xx)

  mn_sd <- paste0(
    round(mean(xx, na.rm = TRUE), nround),
    " (",
    round(sd(xx, na.rm = TRUE), nround),
    ")"
  )

  md_iqr <- paste0(
    round(median(xx, na.rm = TRUE), nround),
    " [",
    round(quantile(xx, na.rm = TRUE, probs = 0.25), nround),
    ",",
    round(quantile(xx, na.rm = TRUE, probs = 0.75), nround),
    "]"
  )

  ci_uni <- paste0(
    "CI[",
    round(ci_uni$lower, nround),
    ";",
    round(ci_uni$upper, nround),
    "]"
  )

  n <- sum(complete.cases(xx) & complete.cases(yy))

  res_uni <- data.frame(
    ALL = paste0(
      n,
      new_line,
      mn_sd,
      new_line,
      ci_uni,
      new_line,
      md_iqr
    )
  )

  if (!prep2sum) {
    res_uni <- cbind(variable = paste0(varname_x, sub), res_uni)
  } else {
    res_uni <- cbind(variable = paste0(varname_x, sub),
                     levels = "",
                     res_uni)
  }

  if (show.n)
    res_uni$n <- n


  # Resumen descriptivo por cada tiempo


  sum_bi <- aggregate(
    xx ~ yy,
    data = data,
    FUN = function(x)
      c(
        n = sum(complete.cases(x)),
        mean = round(mean(x, na.rm = TRUE), nround),
        sd = round(sd(x, na.rm = TRUE), nround),
        median = round(median(x, na.rm = TRUE), nround),
        q25 = round(quantile(x, na.rm = TRUE, probs = 0.25), nround),
        q75 = round(quantile(x, na.rm = TRUE, probs = 0.75), nround)
      )
  )

  # Intervalos de confianza por tiempo
  ci_bi <- as.data.frame(ci.mean(xx ~ yy, data = data))[
    c("yy", "lower", "upper")
  ]


  # Completar niveles sin observaciones

  if (nrow(sum_bi) != length(levels(yy))) {

    sum_bi <- rbind(
      sum_bi,
      data.frame(
        yy = levels(yy)[!levels(yy) %in% sum_bi$yy],
        xx = rep(NA, length(levels(yy)) - nrow(sum_bi))
      )
    )

    rownames(sum_bi) <- sum_bi$yy
    sum_bi <- sum_bi[levels(yy), ]

    ci_bi <- rbind(
      ci_bi,
      data.frame(
        yy = levels(yy)[!levels(yy) %in% ci_bi$yy],
        lower = rep(NA, length(levels(yy)) - nrow(ci_bi)),
        upper = rep(NA, length(levels(yy)) - nrow(ci_bi))
      )
    )

    rownames(ci_bi) <- ci_bi$yy
    ci_bi <- ci_bi[levels(yy), ]
  }


  # Construcción de la tabla descriptiva final

  res_all <- data.frame(
    t(
      paste0(
        sum_bi$xx[, "n"], new_line,
        sum_bi$xx[, "mean"], " (", sum_bi$xx[, "sd"], ")",
        new_line,
        "CI[", round(ci_bi$lower, nround), "; ",
        round(ci_bi$upper, nround), "]",
        new_line,
        sum_bi$xx[, "median"], " [",
        sum_bi$xx[, "q25.25%"], ", ",
        sum_bi$xx[, "q75.75%"], "]"
      )
    )
  )

  colnames(res_all) <- levels(yy)
  rownames(res_all) <- paste0(varname_x, sub)

  if (!prep2sum) {
    res_all <- cbind(variable = paste0(varname_x, sub), res_all)
  } else {
    res_all <- cbind(variable = paste0(varname_x, sub),
                     levels = "",
                     res_all)
  }

  # Añadir columna ALL
  if (show.all)
    res_all$ALL <- res_uni$ALL


  # ANOVA de medidas repetidas

  if (show.pval) {

    res.aov <- suppressWarnings(
      anova_test(
        data = data,
        dv = !!sym(x),
        wid = !!sym(id),
        within = !!sym(tiempo)
      )
    )

    pval <- get_anova_table(res.aov)$p

    pval <- ifelse(grepl("Error", pval), ".", pval)
    pval_round <- ifelse(
      grepl("Error", try(round(pval, 3), TRUE)),
      ".",
      round(pval, 3)
    )

    res_all$p.value <- ifelse(
      pval != "." & pval < 0.001,
      "<0.001",
      pval_round
    )

    txt_pval <- paste0(
      "<font size='1'> <br> p.value: Repeated measures ANOVA</font>"
    )

    # Estadístico F
    if (show.stat) {

      stat <- get_anova_table(res.aov)$F

      stat <- ifelse(grepl("Error", stat), ".", stat)

      stat_round <- ifelse(
        grepl("Error", try(round(pval, 3), TRUE)),
        ".",
        round(stat, 3)
      )

      res_all$stat <- stat_round

    } else {
      stat <- NULL
    }

  } else {
    pval <- NULL
    txt_pval <- NULL
  }


  # Número de observaciones

  if (show.n) {
    res_all$n <- sum(complete.cases(xx) & complete.cases(yy))
  }


  # Texto descriptivo de la tabla

  txt_caption <- paste0(
    "Summary of results by tiempos of ",
    varname_tiempo,
    txt_descriptive
  )


  # Objeto de salida principal

  list_return <- list(
    rows = x,
    txt_test = txt_pval,
    pval = pval,
    txt_caption = txt_caption,
    methods = txt_descriptive,
    summary = res_all
  )


  # Formato auxiliar para otras funciones de tablas
  if (prep.tab) {

    sq_s <- data.frame(res_all)

    sq_sum <- t(
      res_all %>%
        select(-variable, -p.value, -n)
    )

    list_return$df_prep_tab <- data.frame(
      variable = varname_tiempo,
      levels = rownames(sq_sum),
      summary = sq_sum[, 1],
      p.value = unlist(
        c(
          sq_s %>% select(p.value),
          rep("", nrow(sq_sum) - 1)
        )
      ),
      n = unlist(
        c(
          sq_s %>% select(n),
          rep("", nrow(sq_sum) - 1)
        )
      )
    )
  }


  ifelse(
    !is.null(tiempo),
    return(list_return),
    return(
      list(
        variable = x,
        methods = txt_caption,
        txt_caption = txt_caption,
        summary = res_uni
      )
    )
  )
}
