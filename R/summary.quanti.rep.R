#' A summary.quanti.rep Function
#'
#' Genera un resumen descriptivo para una variable numérica en estudios con medidas repetidas,
#' mostrando estadísticas globales y por cada momento temporal. Adicionalmente puede realizar
#' un ANOVA de medidas repetidas y preparar la salida para su integración en tablas resumen.
#'
#' @param data data frame que contiene las variables a analizar.
#' @param x variable numérica a resumir.
#' @param tiempo variable que identifica los momentos temporales o visitas.
#' @param id identificador único de sujeto para el análisis de medidas repetidas.
#' @param format formato de salida utilizado para los saltos de línea. Valores posibles:
#' \code{"html"}, \code{"latex"} o \code{"R"}. Por defecto \code{"html"}.
#' @param show.pval TRUE o FALSE indicando si se realiza el contraste de medidas repetidas y se muestra el p-valor. Por defecto TRUE.
#' @param show.all TRUE o FALSE indicando si se muestra el resumen global (ALL). Por defecto TRUE.
#' @param show.n TRUE o FALSE indicando si se muestra el tamaño muestral. Por defecto TRUE.
#' @param show.stat TRUE o FALSE indicando si se muestra el estadístico F del ANOVA. Por defecto FALSE.
#' @param prep2sum TRUE o FALSE indicando si la salida se adapta para funciones resumen. Por defecto FALSE.
#' @param prep.tab TRUE o FALSE indicando si se prepara una estructura tabular auxiliar para integración en otras funciones. Por defecto FALSE.
#' @param sub.ht TRUE o FALSE indicando si se añade un subíndice identificativo al nombre de la variable. Por defecto TRUE.
#' @param paired argumento reservado para compatibilidad. Por defecto FALSE.
#' @param var.tidy TRUE o FALSE indicando si los nombres de las variables se obtienen automáticamente mediante evaluación no estándar. Por defecto TRUE.
#' @param nround número de decimales utilizados en los resultados. Por defecto 1.
#'
#' @return Una lista con:
#' \itemize{
#'   \item \code{rows}: nombre de la variable analizada.
#'   \item \code{txt_test}: descripción del contraste estadístico realizado.
#'   \item \code{pval}: p-valor del ANOVA de medidas repetidas.
#'   \item \code{txt_caption}: texto descriptivo para tablas.
#'   \item \code{methods}: descripción de las estadísticas mostradas.
#'   \item \code{summary}: tabla resumen con los resultados.
#'   \item \code{df_prep_tab}: tabla preparada para funciones resumen (si \code{prep.tab = TRUE}).
#' }
#'
#' @details
#' Para cada nivel de la variable temporal se calculan:
#' \itemize{
#'   \item Número de observaciones (N).
#'   \item Media y desviación estándar.
#'   \item Intervalo de confianza del 95\% para la media.
#'   \item Mediana y rango intercuartílico (IQR).
#' }
#'
#' Cuando \code{show.pval = TRUE}, se realiza un ANOVA de medidas repetidas utilizando
#' la función \code{anova_test()} del paquete \code{rstatix}.
#'
#' @export summary.quanti.rep
#' @import Hmisc dplyr rstatix pacman
#' @author Àlex Martí Barrera, Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' set.seed(1111)
#'
#' df <- data.frame(
#'   id = rep(1:20, each = 3),
#'   visita = factor(rep(c("Basal", "Mes1", "Mes3"), 20)),
#'   colesterol = rnorm(60, 200, 30)
#' )
#'
#' # Resumen descriptivo básico
#' summary.quanti.rep(
#'   data = df,
#'   x = "colesterol",
#'   tiempo = "visita",
#'   id = "id"
#' )
#'
#' # Mostrar estadístico F
#' summary.quanti.rep(
#'   data = df,
#'   x = "colesterol",
#'   tiempo = "visita",
#'   id = "id",
#'   show.stat = TRUE
#' )
#'
#' # Preparar salida para tablas resumen
#' summary.quanti.rep(
#'   data = df,
#'   x = "colesterol",
#'   tiempo = "visita",
#'   id = "id",
#'   prep.tab = TRUE
#' )
#'
#' @keywords descriptives repeated-measures longitudinal summary statistics

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
