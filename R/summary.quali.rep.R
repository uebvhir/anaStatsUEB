#' @title Resumen descriptivo de una variable binaria con medidas repetidas
#'
#' @description
#' Genera un resumen descriptivo de una variable categórica binaria (factor) en
#' estudios con medidas repetidas: frecuencias, porcentajes e intervalos de
#' confianza exactos binomiales, tanto globales como por cada momento temporal
#' (visita). Opcionalmente realiza el test Q de Cochran para comparar proporciones
#' apareadas entre visitas.
#'
#' @param data Data frame con las variables, en formato largo (una fila por sujeto y
#'   visita).
#' @param x Variable categórica binaria a resumir (un factor): nombre de columna,
#'   con o sin comillas (si \code{var.tidy = TRUE}).
#' @param tiempo Variable que identifica los momentos temporales o visitas (un
#'   factor), en el mismo formato que \code{x}.
#' @param id Nombre (texto) de la variable con el identificador único de sujeto.
#' @param format Formato de los saltos de línea de las celdas: \code{"html"} (por
#'   defecto), \code{"latex"} o \code{"R"}. Cualquier otro valor elimina los saltos
#'   de línea.
#' @param nround Número de decimales de los porcentajes y sus IC (por defecto, 1).
#' @param show.pval Lógico. Si es \code{TRUE} (por defecto), realiza el test Q de
#'   Cochran y muestra el p-valor.
#' @param show.all Lógico. Si es \code{TRUE} (por defecto), añade la columna
#'   \code{ALL} con el resumen global.
#' @param show.n Lógico. Si es \code{TRUE} (por defecto), añade la columna \code{n}
#'   con el número de observaciones (filas), no de sujetos.
#' @param show.stat Lógico. Si es \code{TRUE}, muestra el estadístico Q de Cochran.
#'   Solo se aplica si \code{show.pval = TRUE} (por defecto, \code{FALSE}).
#' @param byrow Lógico. Si es \code{FALSE} (por defecto), los porcentajes se
#'   calculan por columnas (dentro de cada visita); si es \code{TRUE}, por filas
#'   (dentro de cada categoría de \code{x}).
#' @param sub.ht Lógico. Si es \code{TRUE} (por defecto), añade un superíndice de
#'   nota al nombre de la variable. Con \code{FALSE} la función falla; véase
#'   \sQuote{Details}.
#' @param var.tidy Lógico. Si es \code{TRUE} (por defecto), \code{x} y
#'   \code{tiempo} pueden darse con o sin comillas (\code{id} debe ser siempre
#'   texto). Si es \code{FALSE}, deben ser texto.
#' @param correct Reservado por compatibilidad; no tiene efecto (por defecto,
#'   \code{FALSE}).
#'
#' @return Una lista con:
#' \describe{
#'   \item{\code{rows}}{Nombre de la variable analizada.}
#'   \item{\code{columns}}{Nombre de la variable temporal.}
#'   \item{\code{txt_test}}{Texto con el nombre del test (\code{NULL} si
#'     \code{show.pval = FALSE}).}
#'   \item{\code{pval}}{P-valor del test Q de Cochran sin redondear (\code{NULL} si
#'     \code{show.pval = FALSE}).}
#'   \item{\code{txt_caption}}{Título de la tabla.}
#'   \item{\code{methods}}{Descripción del contenido de las celdas.}
#'   \item{\code{summary}}{Data frame con las columnas \code{variable},
#'     \code{levels}, una por visita, \code{ALL}, \code{p.value}, \code{stat} y
#'     \code{n} (según los argumentos \code{show.*}).}
#' }
#'
#' @details
#' \itemize{
#'   \item La función está pensada para variables binarias y calcula frecuencias
#'     absolutas, porcentajes e intervalos de confianza exactos binomiales.
#'   \item Con \code{show.pval = TRUE}, se aplica
#'     \code{rstatix::cochran_qtest(x ~ tiempo | id)}.
#'   \item Se eliminan por completo los sujetos con algún valor perdido en
#'     \code{x}.
#'   \item Nota: \code{sub.ht = FALSE} produce un error. Los niveles vacíos de
#'     \code{x} producen un error (\code{could not find function "\%>\%<-"}). Si
#'     algún sujeto no tiene todas las visitas, o hay valores perdidos en
#'     \code{tiempo}, el test de Cochran falla y no se devuelve ningún resultado.
#'     No se comprueba que \code{x} sea binaria (con más de dos niveles no se
#'     produce error) ni se avisa de más de 10 visitas. La función carga y
#'     adjunta \pkg{rstatix} y \pkg{pacman} en cada llamada
#'     (\code{pacman::p_load()}). La leyenda (\code{txt_caption}) repite tres
#'     veces el texto descriptivo cuando \code{show.pval = TRUE}.
#' }
#'
#' @seealso \code{\link{summary.quanti.rep}}, \code{\link{summary.quali}},
#'   \code{\link{desc_group_rep}}, \code{\link[rstatix]{cochran_qtest}},
#'   \code{\link[binom]{binom.confint}}
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   id = rep(1:30, each = 3),
#'   visita = factor(rep(c("Basal", "Mes1", "Mes3"), 30)),
#'   respuesta = factor(sample(c("Si", "No"), 90, replace = TRUE))
#' )
#' # Resumen descriptivo y test de Cochran
#' res <- summary.quali.rep(data = df, x = "respuesta", tiempo = "visita", id = "id")
#' res$summary
#' res$pval
#'
#' # Nombres sin comillas y estadístico del test
#' res_st <- summary.quali.rep(df, x = respuesta, tiempo = visita, id = "id",
#'                             show.stat = TRUE)
#' res_st$summary[, c("levels", "p.value", "stat")]
#'
#' # Porcentajes por filas, sin la columna global
#' summary.quali.rep(df, "respuesta", "visita", "id", byrow = TRUE,
#'                   show.all = FALSE)$summary
#'
#' @keywords descriptives repeated-measures longitudinal categorical proportions
#'
#' @importFrom dplyr %>% pull group_by filter ungroup sym
#' @importFrom magrittr %<>%
#' @importFrom rstatix cochran_qtest
#' @importFrom binom binom.confint
#' @importFrom pacman p_load
#' @importFrom stats as.formula
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
