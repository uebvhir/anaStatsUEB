#' A summary.quali.rep Function
#'
#' Genera un resumen descriptivo para una variable categórica binaria en estudios
#' con medidas repetidas, mostrando frecuencias, porcentajes e intervalos de confianza
#' exactos tanto de forma global como por cada momento temporal. Adicionalmente puede
#' realizar el test de Cochran para comparar proporciones apareadas entre visitas.
#'
#' @param data data frame que contiene las variables a analizar.
#' @param x variable categórica binaria a resumir.
#' @param tiempo variable que identifica los momentos temporales o visitas.
#' @param id identificador único de sujeto para el análisis de medidas repetidas.
#' @param format formato de salida utilizado para los saltos de línea. Valores posibles:
#' \code{"html"}, \code{"latex"} o \code{"R"}. Por defecto \code{"html"}.
#' @param nround número de decimales utilizados en porcentajes e intervalos de confianza.
#' Por defecto 1.
#' @param show.pval TRUE o FALSE indicando si se realiza el test de Cochran y se muestra el p-valor. Por defecto TRUE.
#' @param show.all TRUE o FALSE indicando si se muestra el resumen global (ALL). Por defecto TRUE.
#' @param show.n TRUE o FALSE indicando si se muestra el tamaño muestral. Por defecto TRUE.
#' @param show.stat TRUE o FALSE indicando si se muestra el estadístico del test de Cochran. Por defecto FALSE.
#' @param byrow TRUE o FALSE indicando si los porcentajes se calculan por filas. Si FALSE se calculan por columnas. Por defecto FALSE.
#' @param sub.ht TRUE o FALSE indicando si se añade un subíndice identificativo al nombre de la variable. Por defecto TRUE.
#' @param var.tidy TRUE o FALSE indicando si los nombres de las variables se obtienen automáticamente mediante evaluación no estándar. Por defecto TRUE.
#' @param correct argumento reservado para compatibilidad. Por defecto FALSE.
#'
#' @return Una lista con:
#' \itemize{
#'   \item \code{rows}: nombre de la variable analizada.
#'   \item \code{columns}: variable temporal utilizada.
#'   \item \code{txt_test}: descripción del contraste estadístico realizado.
#'   \item \code{pval}: p-valor del test de Cochran.
#'   \item \code{txt_caption}: texto descriptivo para tablas.
#'   \item \code{methods}: descripción de las estadísticas mostradas.
#'   \item \code{summary}: tabla resumen con los resultados.
#' }
#'
#' @details
#' La función está diseñada para variables categóricas binarias y calcula:
#' \itemize{
#'   \item Frecuencias absolutas.
#'   \item Porcentajes.
#'   \item Intervalos de confianza exactos binomiales.
#' }
#'
#' Cuando \code{show.pval = TRUE}, se realiza un test de Cochran (\code{cochran_qtest})
#' para evaluar cambios en proporciones apareadas a lo largo del tiempo.
#'
#' @export summary.quali.rep
#' @import Hmisc dplyr rstatix pacman DescTools
#'
#' @author Àlex Martí Barrera, Miriam Mota \email{mmota.foix@@gmail.com}
#'
#' @examples
#' df <- data.frame(
#'   id = rep(1:30, each = 3),
#'   visita = factor(rep(c("Basal", "Mes1", "Mes3"), 30)),
#'   respuesta = factor(sample(c("Sí", "No"), 90, replace = TRUE))
#' )
#'
#' # Resumen descriptivo básico
#' summary.quali.rep(
#'   data = df,
#'   x = "respuesta",
#'   tiempo = "visita",
#'   id = "id"
#' )
#'
#' # Mostrar estadístico del test
#' summary.quali.rep(
#'   data = df,
#'   x = "respuesta",
#'   tiempo = "visita",
#'   id = "id",
#'   show.stat = TRUE
#' )
#'
#' # Porcentajes por fila
#' summary.quali.rep(
#'   data = df,
#'   x = "respuesta",
#'   tiempo = "visita",
#'   id = "id",
#'   byrow = TRUE
#' )
#'
#' @keywords descriptives repeated-measures longitudinal categorical proportions

summary.quali.rep <- function (
    data,
    x,
    tiempo,
    id,
    format = "html",
    nround = 1,
    show.pval = TRUE,
    show.all = TRUE,
    show.n = TRUE,
    show.stat = FALSE,
    byrow = FALSE,
    sub.ht = TRUE,
    var.tidy = TRUE,
    correct = FALSE
) {


  # Cargar paquete necesario para el test de Cochran

  p_load(rstatix,pacman)


  # Permite pasar variables como texto o estilo tidyverse

  if (var.tidy) {
    x <- gsub("\"", "", deparse(substitute(x)))
    try(tiempo <- gsub("\"", "", deparse(substitute(tiempo))), TRUE)
  }


  # Comprobaciones iniciales


  # Aviso si existen muchos tiempos de seguimiento
  if (length(length(table(data %>% pull(tiempo)))) > 10)
    warning("La variable tiempo tiene mas de 10 niveles")

  # La función sólo admite variables binarias
  if (length(length(table(data %>% pull(x)))) > 2)
    stop("La función no esta preparada para variables x con más de 2 niveles")

  # La variable tiempo debe ser factor
  if (class(data %>% pull(tiempo))[length(class(data %>% pull(tiempo)))] != "factor")
    stop("La variable tiempo debe ser factor")


  # Eliminar sujetos con algún NA en la variable respuesta

  data %<>%
    group_by(!!sym(id)) %>%
    filter(!any(is.na(!!sym(x)))) %>%
    ungroup()

  # La variable respuesta debe ser factor
  if (class(data %>% pull(x))[length(class(data %>% pull(x)))] != "factor")
    stop("La variable x debe ser factor")


  # Eliminar niveles vacíos del factor

  if (any(table(data %>% pull(x)) == 0)) {

    lb <- Hmisc::label(data %>% pull(x))

    if (is.factor(data %>% pull(x)))
      data %>% pull(x) <- droplevels(data %>% pull(x))

      message(
        "Some levels of ",
        x,
        " are removed since no observation in that/those levels"
      )

      Hmisc::label(data %>% pull(x)) <- lb
  }


  # Configuración de saltos de línea según formato

  new_line <- switch(
    format,
    html = " <br> ",
    latex = " \\\\ ",
    R = " \n "
  )


  # Variables de trabajo y etiquetas

  xx <- data %>% pull(x)

  varname_x <- ifelse(
    Hmisc::label(data %>% pull(x)) != "",
    Hmisc::label(data %>% pull(x)),
    x
  )

  if (!is.null(tiempo)) {

    varname_tiempo <- ifelse(
      Hmisc::label(data %>% pull(tiempo)) != "",
      Hmisc::label(data %>% pull(tiempo)),
      tiempo
    )

    yy <- data %>% pull(tiempo)
  }

  if (sub.ht)
    sub <- "<sub>1</sub>"


  # Resumen descriptivo global (ALL)


  if (!byrow) {
    n <- sum(table(xx))
  } else {
    n <- table(xx)
  }

  # Intervalos de confianza exactos binomiales
  uni <- binom.confint(
    table(xx),
    n,
    methods = "exact"
  )

  # Compatibilidad entre versiones de binom.confint()
  if (is.null(uni$x))
    uni$x <- uni$x.Freq

  if (is.null(uni$mean))
    uni$mean <- uni$mean.Freq

  # Tabla descriptiva global
  res_uni <- data.frame(
    ALL = paste0(
      uni$x,
      " (",
      round(uni$mean * 100, nround),
      "%)",
      new_line,
      "[",
      round(uni$lower * 100, nround),
      "; ",
      round(uni$upper * 100, nround),
      "]"
    ),
    row.names = levels(xx)
  )

  res_uni <- cbind(
    variable = c(
      paste0(varname_x, sub),
      rep("", nrow(res_uni) - 1)
    ),
    levels = levels(xx),
    res_uni
  )

  if (show.n)
    res_uni$n <- c(
      sum(table(xx)),
      rep("", nrow(res_uni) - 1)
    )

  txt_caption <- " <font size='1'> 1: n(%) <br> [Exact CI] </font>"


  # Resumen por tiempo


  if (!byrow) {

    # Por columnas: porcentaje dentro de cada tiempo
    res_bi <- apply(table(xx, yy), 2, function(x) {

      bb <- binom.confint(
        x,
        sum(x),
        methods = "exact"
      )

      if (is.null(bb$x))
        bb$x <- bb$x.Freq

      if (is.null(bb$mean))
        bb$mean <- bb$mean.Freq

      data.frame(
        paste0(
          bb$x,
          " (",
          round(bb$mean * 100, nround),
          "%)",
          new_line,
          "CI[",
          round(bb$lower * 100, nround),
          "; ",
          round(bb$upper * 100, nround),
          "]"
        )
      )
    })

    res_all <- do.call(cbind, res_bi)

    colnames(res_all) <- levels(yy)
    rownames(res_all) <- levels(xx)

    res_all <- cbind(
      variable = c(
        paste0(varname_x, sub),
        rep("", nrow(res_all) - 1)
      ),
      levels = levels(xx),
      res_all
    )

    txt_descriptive <- " <font size='1'> <br> 1: by col <br> n(%) <br> [Exact CI] </font>"

    txt_caption <- paste0(
      "Summary of results by tiempos of ",
      varname_tiempo,
      txt_descriptive
    )

  } else {

    # Por filas: porcentaje dentro de cada categoría de la variable
    res_bi <- apply(table(xx, yy), 1, function(x) {

      bb <- binom.confint(
        x,
        sum(x),
        methods = "exact"
      )

      if (is.null(bb$x))
        bb$x <- bb$x.Freq

      if (is.null(bb$mean))
        bb$mean <- bb$mean.Freq

      data.frame(
        paste0(
          bb$x,
          " (",
          round(bb$mean * 100, nround),
          "%)",
          new_line,
          "CI[",
          round(bb$lower * 100, nround),
          "; ",
          round(bb$upper * 100, nround),
          "]"
        )
      )
    })

    res_all <- data.frame(
      t(do.call(cbind, res_bi))
    )

    colnames(res_all) <- levels(yy)
    rownames(res_all) <- levels(xx)

    res_all <- cbind(
      variable = c(
        paste0(varname_x, sub),
        rep("", nrow(res_all) - 1)
      ),
      levels = levels(xx),
      res_all
    )

    txt_descriptive <- " <font size='1'> <br> 1: by row <br> n(%) <br> [Exact CI] </font>"

    txt_caption <- paste0(
      "Summary of results by tiempos of ",
      varname_tiempo,
      txt_descriptive
    )
  }

  # Añadir columna global
  if (show.all)
    res_all$ALL <- res_uni$ALL


  # Test de Cochran Q para datos apareados binarios


  if (show.pval) {

    cochran <- cochran_qtest(
      data,
      as.formula(
        paste(x, "~", tiempo, "|", id)
      )
    )

    pval <- cochran$p

    pval <- ifelse(
      grepl("Error", pval),
      ".",
      pval
    )

    pval_round <- ifelse(
      grepl("Error", try(round(pval, 3), TRUE)),
      "-",
      round(pval, 3)
    )

    pval_round[
      grep("-", pval_round, fixed = TRUE)
    ] <- "-"

    res_all$p.value <- c(
      ifelse(
        pval != "." & pval < 0.001,
        "<0.001",
        pval_round
      ),
      rep("", nrow(res_all) - 1)
    )

    txt_pval <- paste0(
      "<font size='1'> <br> p.value: Cochran's Q test</font>"
    )

    txt_caption <- paste(
      txt_caption,
      txt_descriptive,
      txt_descriptive,
      txt_pval
    )

    # Estadístico Q
    if (show.stat) {

      stat <- cochran$statistic

      stat_round <- ifelse(
        stat == "-",
        ".",
        round(stat, 3)
      )

      res_all$stat <- c(
        stat_round,
        rep("", nrow(res_all) - 1)
      )

    } else {
      stat <- NULL
    }

  } else {

    pval <- NULL
    txt_pval <- NULL
  }


  # Añadir tamaño muestral

  if (show.n) {

    res_all$n <- c(
      sum(table(xx, yy)),
      rep("", nrow(res_all) - 1)
    )
  }


  ifelse(
    !is.null(tiempo),

    return(
      list(
        rows = x,
        columns = tiempo,
        txt_test = txt_pval,
        pval = pval,
        txt_caption = txt_caption,
        methods = txt_descriptive,
        summary = res_all
      )
    ),

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
