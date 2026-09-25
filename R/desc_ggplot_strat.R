#' Gráficos descriptivos automáticos estratificados por una variable
#'
#' Genera, para cada variable, un único gráfico descriptivo (barras o boxplot) que
#' compara todos los niveles de una variable de estratificación (\code{strat}) en el
#' eje X, permitiendo además una segunda variable de agrupación (\code{y}) mostrada
#' mediante color (variables numéricas) o mediante paneles/\code{facet_wrap} (variables
#' categóricas). Sigue la misma lógica y argumentario que \code{desc_ggplot()}, del
#' que reutiliza el tema común, el manejo de etiquetas (\code{Hmisc::label}), el
#' ajuste de orientación del eje X y el resto de opciones de personalización.
#'
#' @param dat Data frame que contiene las variables a analizar.
#' @param covariates Vector de nombres de variables a graficar. Si se usa \code{frml},
#' se sobreescribe.
#' @param frml Fórmula del tipo \code{y ~ x1 + x2 + ...} para definir la variable de
#' agrupación adicional (\code{y}) y las covariables. Se sobreescribe si se
#' proporciona \code{res_strat}.
#' @param y Variable de agrupación adicional (opcional). \code{strat} actúa siempre
#' como agrupación principal (arriba), e \code{y} como agrupación secundaria (abajo),
#' tanto para variables numéricas como categóricas: \code{strat} determina los
#' paneles (\code{facet_wrap()}, mostrando solo el nombre de cada nivel) e \code{y}
#' se sitúa en el eje X (con sus niveles y su etiqueta como título de eje). Para
#' variables numéricas, el color del boxplot también se mapea a \code{y} (redundante
#' con el eje X a propósito, para mantener un color consistente por nivel; por eso
#' no se muestra leyenda en ese caso).
#' @param strat Variable de estratificación. Actúa siempre como agrupación principal:
#' en el eje X cuando es la única variable de agrupación, o como panel/\code{facet}
#' (arriba, mostrando solo el nombre de cada nivel) cuando además se indica \code{y}.
#' @param res_strat Objeto devuelto por \code{desc_group_strat()}. Si se proporciona,
#' se toman de él la variable de estratificación (\code{strat}), la variable de
#' grupo (\code{y}) y la unión de covariables empleadas en dicho análisis, de forma
#' que los gráficos resulten coherentes con las tablas ya generadas. En ese caso los
#' argumentos \code{covariates}, \code{frml}, \code{y} y \code{strat} se ignoran.
#' @param nameFile Nombre del archivo PDF de salida si \code{topdf = TRUE}.
#' @param topdf Lógico. Si \code{TRUE}, guarda los gráficos en un archivo PDF.
#' @param list.plots Lógico. Si \code{TRUE}, retorna una lista con los objetos ggplot
#' generados.
#' @param color Color de relleno para histogramas (variables \code{Date}/\code{POSIXt}).
#' @param rowcol Vector de longitud 2 que indica el número de filas y columnas en el PDF.
#' @param strat_nrow Entero opcional. Número de filas del \code{facet_wrap()} que
#' organiza los paneles de \code{strat} (solo aplica cuando se indica \code{y}, o
#' para variables \code{Date}/\code{POSIXt}). Por defecto \code{NULL}, que deja el
#' layout automático de \code{ggplot2} (\code{ceiling(sqrt(n))} columnas). Con
#' \code{strat_nrow = 1} todos los niveles de \code{strat} se muestran en una
#' única fila, independientemente de \code{rowcol} (que solo afecta a la
#' paginación del PDF, no al \code{facet_wrap()} interno de cada gráfico).
#' @param show.freq Lógico. Si \code{TRUE}, muestra frecuencias sobre las barras.
#' @param bw Lógico. Si \code{TRUE}, agrega jitter (dispersión) a los boxplots.
#' @param size.n Tamaño del texto que muestra el número total de observaciones (\code{n}).
#' @param size.freq Tamaño del texto para frecuencias o porcentajes.
#' @param size.title Tamaño del título del gráfico.
#' @param size.pval Tamaño del texto del p-valor.
#' @param show.pval Lógico. Si \code{TRUE}, incluye el p-valor comparando los niveles
#' de \code{strat}. Solo se calcula cuando \code{y} es \code{NULL}: con dos variables
#' de agrupación simultáneas (\code{strat} e \code{y}) no se calcula un único
#' p-valor y esta opción se ignora con un aviso.
#' @param show.n Lógico. Si \code{TRUE}, muestra el número de observaciones.
#' @param show.na Lógico. Si \code{TRUE}, se incluyen los \code{NA} en los análisis.
#' @param legend.position Posición de la leyenda de relleno en los gráficos de
#' variables categóricas (por ejemplo, \code{"right"}, \code{"bottom"}). No afecta
#' a las variables numéricas: su color (mapeado a \code{y}) no muestra leyenda, ya
#' que \code{y} también está en el eje X.
#' @param angle_x Orientación de las etiquetas del eje X: \code{"horizontal"}
#' (por defecto), \code{"vertical"} o \code{"diagonal"}.
#' @param ... Argumentos adicionales (no usados actualmente).
#'
#' @return Por defecto imprime los gráficos. Si \code{list.plots = TRUE}, retorna una
#' lista de objetos ggplot. Si \code{topdf = TRUE}, guarda los gráficos en un único
#' archivo PDF.
#'
#' @details
#' La función selecciona automáticamente el tipo de gráfico según el tipo de variable.
#' Cuando se indica \code{y}, ambos tipos de variable comparten la misma jerarquía
#' visual: \code{strat} arriba (panel/\code{facet_wrap}, solo nombre de nivel) e
#' \code{y} abajo (eje X, con sus niveles y su etiqueta como título):
#' - Variables \code{factor}: gráfico de barras apiladas en porcentaje. Los
#'   porcentajes se calculan dentro de cada combinación exacta de \code{strat} e
#'   \code{y} (igual que \code{desc_group_strat()}), de forma que coinciden con
#'   los de la tabla.
#' - Variables numéricas: boxplot, coloreado por \code{y} (sin leyenda, ya que
#'   \code{y} también está en el eje X).
#' - Variables \code{Date} o \code{POSIXt}: histograma por fechas, facetado por
#'   \code{strat} (no admiten \code{y}).
#'
#' Si no se indica \code{y}, \code{strat} pasa a ser la única agrupación y se sitúa
#' directamente en el eje X (sin paneles).
#'
#' Variables de tipo \code{character} son ignoradas con una advertencia.
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' mtcars$am  <- factor(mtcars$am)
#' mtcars$cyl <- factor(mtcars$cyl)
#'
#' # Boxplot de mpg por cyl (eje X), coloreado por am
#' desc_ggplot_strat(mtcars, covariates = "mpg", y = "am", strat = "cyl")
#'
#' # A partir del objeto de desc_group_strat()
#' res <- desc_group_strat(data = mtcars, covariates = c(mpg, wt), group = am, strat = cyl)
#' desc_ggplot_strat(mtcars, res_strat = res)
#' }
#'
#' @import ggplot2 dplyr Hmisc scales stringr purrr
#' @author BIDU-UEB. VHIR \email{bioestadistica@vhir.org}
#' @export

desc_ggplot_strat <- function(dat,
                              covariates = NULL,
                              frml = NULL,
                              y = NULL,
                              strat = NULL,
                              res_strat = NULL,
                              nameFile = "descriptive_plots_strat.pdf",
                              topdf = FALSE,
                              list.plots = FALSE,
                              color = "#8D4ABA",
                              rowcol = c(1, 1),
                              strat_nrow = NULL,
                              show.freq = TRUE,
                              bw = TRUE,
                              size.n = 3,
                              size.freq = 2.5,
                              size.title = 10,
                              size.pval = 3,
                              show.pval = FALSE,
                              show.n = TRUE,
                              show.na = FALSE,
                              legend.position = "right",
                              angle_x = "horizontal",
                              ...) {

  graficos <- list()

  # ------------------------------------------------------------------------
  # 1. Si se recibe un objeto de desc_group_strat(), se toman de el la variable
  #    de estratificacion, la variable de grupo y la union de covariables
  # ------------------------------------------------------------------------
  if (!is.null(res_strat)) {

    if (!is.list(res_strat) || !all(c("strat", "strat_levels", "results_list") %in% names(res_strat))) {
      stop("'res_strat' debe ser el objeto devuelto por la funcion 'desc_group_strat()'.")
    }

    strat <- res_strat$strat
    y <- res_strat$results_list[[1]][["group"]]
    covariates <- unique(unlist(lapply(res_strat$results_list, function(x) x[["covariates"]])))
  }

  # ------------------------------------------------------------------------
  # 2. Comprobaciones y seleccion de variables (misma logica que desc_ggplot())
  # ------------------------------------------------------------------------
  if (is.null(strat)) {
    stop("Debe especificarse el argumento 'strat', o bien proporcionar el objeto 'res_strat' generado por 'desc_group_strat()'.")
  }
  if (!strat %in% names(dat)) {
    stop("La variable de estratificacion '", strat, "' no existe en 'dat'.")
  }
  if (!is.null(y) && !y %in% names(dat)) {
    stop("La variable de grupo '", y, "' no existe en 'dat'.")
  }

  ## en el cas de que hi hagi formula seleccionem el grup i les covariates
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {y <- lhs.vars(frml)}
  }

  covariates <- setdiff(covariates, c(strat, y))

  ## avisem si alguna covariate demanada no existeix a 'dat' (dplyr::select(any_of())
  ## les descartaria en silenci, amagant un possible error de l'usuari)
  if (!is.null(covariates)) {
    covariates_falten <- setdiff(covariates, names(dat))
    if (length(covariates_falten) > 0) {
      warning("Las siguientes covariates no existen en 'dat' y se ignoran: ",
              paste(covariates_falten, collapse = ", "))
    }
  }

  ## en el cas de que seleccionem variables a analitzar reduim bbdd a variables necesaies
  if (!is.null(covariates)) {
    dat <- dat |> dplyr::select(any_of(c(covariates, y, strat)))
  }

  ## eliminem columnes buides
  dat <- remove_empty(dat, which = c("cols"))

  ## capturem els labels de 'strat' i 'y' ABANS de convertir-los a factor():
  ## factor() no preserva l'atribut 'label' de Hmisc, i el perdriem si el
  ## llegissim despres de la conversio
  lbl_strat <- ifelse(Hmisc::label(dat[[strat]]) == "", strat, Hmisc::label(dat[[strat]]))
  lbl_y <- if (!is.null(y)) ifelse(Hmisc::label(dat[[y]]) == "", y, Hmisc::label(dat[[y]])) else NULL

  ## forcem 'strat' i 'y' a factor: evita comportaments inconsistents si
  ## arriben com character/integer (p.ex. espaiat incorrecte a l'eix X o
  ## als facets si 'y' no fos ja un factor)
  dat[[strat]] <- factor(dat[[strat]])
  if (!is.null(y)) dat[[y]] <- factor(dat[[y]])

  ## Labels i names de les variables a graficar (excepte y i strat)
  lbls <- Hmisc::label(dat[!names(dat) %in% c(y, strat)])
  lbls[lbls == ""] <- names(dat)[!names(dat) %in% c(y, strat)][lbls == ""]
  namevar <- names(lbls)

  if (show.pval && !is.null(y)) {
    message("show.pval se ignora: con 'strat' e 'y' simultaneos no se calcula un unico p-valor.")
    show.pval <- FALSE
  }

  common_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = size.title),
      panel.grid.major = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )

  angle_settings <- switch(
    angle_x,
    "horizontal" = element_text(angle = 0, hjust = 0.5, vjust = 1),
    "diagonal"   = element_text(angle = 45, hjust = 1, vjust = 1),
    "vertical"   = element_text(angle = 90, hjust = 1, vjust = 0.5),
    element_text(angle = 0, hjust = 0.5, vjust = 1)
  )

 
  for (i in seq_along(namevar)) {

    ##### variables factor -------------------------------------------------
    if (inherits(dat[[namevar[i]]], "factor")) {

      dd <- if (show.na) dat else dat |> dplyr::select(any_of(c(namevar[i], strat, y))) |> na.omit()

      ## 'strat' actua siempre como agrupacion principal: eje X cuando es la
      ## unica variable de agrupacion, o panel/facet cuando ademas hay 'y'
      ## (asi la jerarquia es consistente con el boxplot de las numericas,
      ## donde 'strat' tambien es la agrupacion principal en el eje X)
      x_var <- if (is.null(y)) strat else y
      lbl_x <- if (is.null(y)) lbl_strat else lbl_y

      group_vars <- if (is.null(y)) strat else c(strat, y)

      ## porcentajes calculados dentro de cada combinacion exacta de
      ## (strat[,y]) por separado, igual que 'desc_group_strat()', para que
      ## coincidan con los de la tabla
      df_plot <- dd |>
        dplyr::count(across(all_of(c(group_vars, namevar[i])))) |>
        dplyr::group_by(across(all_of(group_vars))) |>
        dplyr::mutate(total = sum(n), pct = n / total) |>
        dplyr::ungroup()

      graficos[[i]] <- ggplot(df_plot, aes_string(x = x_var, y = "pct", fill = namevar[i])) +
        geom_col(position = "fill", linewidth = 0.3) +
        scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
        scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
        labs(title = str_wrap(lbls[namevar[i]], width = 40), x = lbl_x, y = "%", fill = lbls[namevar[i]]) +
        common_theme +
        theme(legend.position = legend.position,
              legend.spacing.y = unit(0.5, "cm"),
              axis.text.x = angle_settings)

      if (!is.null(y)) {
        ## el facet solo muestra el nombre del nivel de 'strat' (labeller por
        ## defecto), sin repetir el nombre de la variable en cada panel
        graficos[[i]] <- graficos[[i]] +
          facet_wrap(stats::as.formula(paste("~", strat)), nrow = strat_nrow)
      }

      if (show.freq) {
        graficos[[i]] <- graficos[[i]] +
          geom_text(aes(label = scales::percent(pct, accuracy = 1)),
                    position = position_fill(vjust = 0.5), color = "black", size = size.freq)
      }

      if (show.n) {

        df_n <- df_plot |> dplyr::distinct(across(all_of(group_vars)), total)

        graficos[[i]] <- graficos[[i]] +
          geom_text(data = df_n,
                    aes_string(x = x_var, y = "1.01", label = "paste0('n = ', total)"),
                    inherit.aes = FALSE, size = (size.n - 0.5), color = "black",
                    angle = 0, hjust = 0.5, vjust = 0)

        ## la n total del panel solo se muestra sin facetado por 'y': con facetas
        ## las n por barra (arriba) ya informan del tamano de cada combinacion
        if (is.null(y)) {
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = Inf, y = 1.1, label = paste0("n = ", nrow(dd)),
                     hjust = 1.2, vjust = 1.5, size = (size.n + 0.5))
        }
      }

      ##### variables caracter ------------------------------------------------
    } else if (inherits(dat[[namevar[i]]], "character")) {
      message(paste("La variable", namevar[i], "es tipo caracter y no se ha realizado grafico"))

      ##### variables dates ----------------------------------------------------
    } else if (inherits(dat[[namevar[i]]], "Date") | inherits(dat[[namevar[i]]], "POSIXt")) {

      dd <- if (show.na) dat else dat |> dplyr::select(any_of(c(namevar[i], strat))) |> na.omit()

      graficos[[i]] <- ggplot(dd, aes_string(x = namevar[i])) +
        geom_histogram(binwidth = 1, fill = color, color = "black", linewidth = 0.3) +
        labs(title = lbls[namevar[i]], x = "Fecha", y = "Frecuencia") +
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "5 days") +
        facet_wrap(stats::as.formula(paste("~", strat)), nrow = strat_nrow) +
        common_theme +
        theme(axis.text.x = angle_settings)

      if (show.n) {
        df_n_strat <- dd |> dplyr::count(across(all_of(strat)))
        graficos[[i]] <- graficos[[i]] +
          geom_text(data = df_n_strat,
                    aes_string(x = Inf, y = Inf, label = "paste0('n = ', n)"),
                    inherit.aes = FALSE, hjust = 1.2, vjust = 1.5, size = size.n)
      }

      ##### variables numeriques ------------------------------------------------
    } else {

      dd <- if (show.na) dat else dat |> dplyr::select(any_of(c(namevar[i], strat, y))) |> na.omit()

      if (bw) outliers <- NA else outliers <- 1.5

      if (is.null(y)) {

        graficos[[i]] <- ggplot(dd, aes_string(x = strat, y = namevar[i], color = strat)) +
          geom_boxplot(fill = "gray80", alpha = 0.5, outlier.shape = outliers) +
          labs(title = lbls[namevar[i]], x = lbl_strat, y = "") +
          common_theme +
          theme(legend.position = "none", axis.text.x = angle_settings)

        if (bw) graficos[[i]] <- graficos[[i]] + geom_jitter(width = 0.2, height = 0, alpha = 0.7)

        if (show.pval) {
          info_test <- test_numericas(factor_col = strat, numerica_col = namevar[i], data = dd, parametrico = FALSE)
          graficos[[i]] <- graficos[[i]] +
            labs(title = paste(lbls[namevar[i]], ".", info_test$test, "p:",
                               format.pval(info_test$pvalor, digits = 3, eps = 0.001)))
        }

      } else {

        ## misma jerarquia que las categoricas: 'strat' arriba (facet, solo
        ## nombres de nivel), 'y' abajo (eje X, con sus niveles y su titulo)
        graficos[[i]] <- ggplot(dd, aes_string(x = y, y = namevar[i], color = y)) +
          geom_boxplot(fill = "gray80", alpha = 0.5, outlier.shape = outliers) +
          facet_wrap(stats::as.formula(paste("~", strat)), nrow = strat_nrow) +
          labs(title = lbls[namevar[i]], x = lbl_y, y = "") +
          common_theme +
          theme(legend.position = "none", axis.text.x = angle_settings)

        if (bw) graficos[[i]] <- graficos[[i]] + geom_jitter(width = 0.2, height = 0, alpha = 0.7)
      }

      if (show.n) {
        if (is.null(y)) {
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(dd)), hjust = 1.2, vjust = 1.5, size = size.n)
        } else {
          df_n_strat <- dd |> dplyr::count(across(all_of(strat)))
          graficos[[i]] <- graficos[[i]] +
            geom_text(data = df_n_strat,
                      aes_string(x = Inf, y = Inf, label = "paste0('n = ', n)"),
                      inherit.aes = FALSE, hjust = 1.2, vjust = 1.5, size = size.n)
        }
      }
    }
  }

  graficos <- graficos[!sapply(graficos, is.null)]

  if (topdf) {
    ggplot_to_pdf(graficos, row = rowcol[1], col = rowcol[2], name.file = nameFile)
  } else if (list.plots) {
    return(graficos)
  } else {
    return(walk(graficos, print))
  }
}
