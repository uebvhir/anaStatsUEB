#' Gráficos descriptivos automáticos para variables categóricas y numéricas
#'
#' Esta función genera gráficos descriptivos (barras, histogramas, boxplots) para un conjunto de variables,
#' de forma univariada o bivariada, con opciones para personalizar estilo, leyendas, etiquetas, tamaños de texto, etc.
#'
#' @param dat Data frame que contiene las variables a analizar.
#' @param covariates Vector de nombres de variables a graficar. Si se usa \code{frml}, se sobreescribe.
#' @param frml Fórmula del tipo \code{y ~ x1 + x2 + ...} para definir la variable dependiente y las independientes.
#' @param y Variable de agrupación (dependiente) para análisis bivariado. Ignorado si se usa \code{frml}.
#' @param method \code{'param'} o \code{'non-param'} (también \code{'nonparam'}). Test del p-valor cuando
#'   \code{show.pval = TRUE}; debe ser el mismo que en \code{desc_group()}. Default value is 'non-param'.
#' @param nameFile Nombre del archivo PDF de salida si \code{topdf = TRUE}.
#' @param topdf Lógico. Si \code{TRUE}, guarda los gráficos en un archivo PDF.
#' @param list.plots Lógico. Si \code{TRUE}, retorna una lista con los objetos ggplot generados.
#' @param color Color de relleno para histogramas.
#' @param rowcol Vector de longitud 2 que indica el número de filas y columnas en el PDF.
#' @param show.freq Lógico. Si \code{TRUE}, muestra frecuencias sobre las barras o dentro de los gráficos.
#' @param bw Lógico. Si \code{TRUE}, agrega jitter (dispersión) a los boxplots.
#' @param size.n Tamaño del texto que muestra el número total de observaciones (\code{n}).
#' @param size.freq Tamaño del texto para frecuencias o porcentajes.
#' @param size.title Tamaño del título del gráfico.
#' @param size.pval Tamaño del texto del p-valor.
#' @param show.pval Lógico. Si \code{TRUE}, incluye el p-valor en los gráficos bivariados.
#' @param show.n Lógico. Si \code{TRUE}, muestra el número total de observaciones.
#' @param show.na Lógico. Si \code{TRUE}, se incluyen los \code{NA} en los análisis.
#' @param legend.position Posición de la leyenda en los gráficos (por ejemplo, \code{"right"}, \code{"bottom"}).
#' @param angle_x Define la orientación de las etiquetas del eje X: puede ser \code{"horizontal"} (por defecto), \code{"vertical"} o \code{"diagonal"}.
#' @param idvar Nombre de la variable identificadora (texto). Si se indica, en los boxplots bivariados
#'   (\code{y} informado) se etiquetan con \code{ggrepel} los \emph{outliers} de cada grupo. Por defecto
#'   \code{NULL}: sin etiquetas y sin ningún cambio en el gráfico.
#' @param outlier.method Criterio para definir \emph{outlier}, calculado dentro de cada grupo:
#'   \code{'sd'} (media +/- k SD, por defecto), \code{'iqr'} (Q1 - k IQR, Q3 + k IQR; con k = 1.5 coincide
#'   con los bigotes del boxplot) o \code{'mad'} (mediana +/- k MAD).
#' @param outlier.k Número de SD (o de IQR / MAD). Por defecto 3.
#' @param outlier.max Número máximo de etiquetas por gráfico (los más extremos). Por defecto 10.
#' @param size.outlier Tamaño del texto de las etiquetas.
#' @param ... Argumentos adicionales (no usados actualmente).
#'
#' @return Por defecto imprime los gráficos. Si \code{list.plots = TRUE}, retorna una lista de objetos ggplot.
#' Con \code{idvar}, la lista lleva el atributo \code{"outliers"} (\code{attr(res, "outliers")}): un data.frame
#' con \code{variable, group, id, value, lower, upper, method, k}.
#' Si \code{topdf = TRUE}, guarda los gráficos en un archivo PDF.
#'
#' @details
#' La función selecciona automáticamente el tipo de gráfico según el tipo de variable:
#' - Variables \code{factor}: gráfico de barras (univariado o apilado bivariado).
#' - Variables numéricas: histograma (univariado) o boxplot (bivariado).
#' - Variables \code{Date} o \code{POSIXt}: histograma por fechas.
#'
#' Variables de tipo \code{character} son ignoradas con una advertencia.
#'
#' El p-valor (\code{show.pval = TRUE}) se calcula con \code{summary.quanti()} / \code{summary.quali()},
#' las mismas funciones que usa \code{desc_group()}, por lo que coincide con la tabla. Para un gráfico
#' con el test completo y el post-hoc, ver \code{desc_ggstats()}.
#'
#' @examples
#' \dontrun{
#' desc_ggplot(mtcars, covariates = c("mpg", "cyl"), y = "am", topdf = TRUE)
#' }
#'
#' @import ggplot2 dplyr Hmisc scales stringr
#' @export

desc_ggplot <- function(dat,
                        covariates = NULL,
                        frml = NULL,
                        y = NULL,
                        method = "non-param",
                        nameFile = "descriptive_plots.pdf",
                        topdf = FALSE,
                        list.plots = FALSE,
                        color = "#8D4ABA",
                        rowcol = c(1, 1),
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
                        idvar = NULL,
                        outlier.method = "sd",
                        outlier.k = 3,
                        outlier.max = 10,
                        size.outlier = 2.5,
                        ...) {

  graficos <- list()

  method <- switch(method,
                   "param" = "param",
                   "non-param" = ,
                   "nonparam" = "non-param",
                   stop("El argumento 'method' debe ser 'param' o 'non-param'."))

  ## outliers amb etiqueta (nomes si s'indica 'idvar')
  outlier.method <- match.arg(outlier.method, c("sd", "iqr", "mad"))
  if (!is.null(idvar)) {
    if (length(idvar) != 1 || !idvar %in% names(dat)) stop("La variable 'idvar' (", idvar, ") no existe.")
    if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Para etiquetar outliers ('idvar') instale el paquete 'ggrepel'.")
  }
  list_out <- list()

  ## en el cas de que hi hagi formula seleccionem el grup i les covariates
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {y <- lhs.vars(frml)}
  }

  ## en el cas de que seleccionem variables a analitzar reduim bbdd a variables necesaies
  if (!is.null(covariates)) {
    dat %<>% dplyr::select(any_of(c(covariates,y,idvar)))
  }

  ## eliminem columnes buides
  dat <- remove_empty(dat, which = c("cols"))

  ## Labels i names  de les a gràficar excepte la que crea grup.
  ## ('idvar' no es grafica)
  lbls <- Hmisc::label(dat[!names(dat) %in% c(y, idvar)])
  lbls[lbls == ""] <- names(dat)[!names(dat) %in% c(y, idvar)][lbls == ""]
  namevar <- names(lbls)

  lbl_y <- ifelse(Hmisc::label(dat[,y]) == "",y, Hmisc::label(dat[,y]))


  common_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = size.title),
      panel.grid.major = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )

  # Especificamos orientacion de las etiquetas del eje X
  angle_settings <- switch(
    angle_x,
    "horizontal" = element_text(angle = 0, hjust = 0.5, vjust = 1),
    "diagonal"   = element_text(angle = 45, hjust = 1, vjust = 1),
    "vertical"   = element_text(angle = 90, hjust = 1, vjust = 0.5),
    element_text(angle = 0, hjust = 0.5, vjust = 1)  # valor por defecto si no coincide
  )

  ## text del p-valor a partir de summary.quanti() / summary.quali(): nom del test (de 'txt_test') i p
  txt_pval <- function(sq) {
    tst <- gsub("<[^>]+>", "", gsub("<sup>.*?</sup>", "", sq$txt_test, perl = TRUE))
    tst <- trimws(sub(".*p\\.value:", "", tst))
    pv <- if (is.numeric(sq$pval)) format.pval(sq$pval, digits = 3, eps = 0.001) else "."
    paste(tst, "p:", pv)
  }


  for (i in seq_along(namevar)) {

    if (inherits(dat[[namevar[i]]], "factor")) {



      ## descriptiu univariat
      if (is.null(y)) {

        dd <- if (show.na) dat else dat %>% dplyr::select(any_of(c(namevar[i]))) %>% na.omit()

        dd[[namevar[i]]] <- factor(dd[[namevar[i]]], levels = names(sort(table(dd[[namevar[i]]]), decreasing = TRUE))) ## ordenar por freq

        # Crear el gráfico de barras
        graficos[[i]] <- ggplot(dd, aes_string(x = namevar[i])) +
          geom_bar(aes(y = (..count..)/sum(..count..) * 100, fill = !!sym(namevar[i])), color = "black", linewidth = 0.3) +  # Barras en porcentaje
          labs(title = lbls[namevar[i]], x = NULL, y = "%") +  # Etiquetas
          common_theme +
          theme(legend.position = "none") +
          scale_x_discrete(labels = wrap_format(10))

        if(show.freq) graficos[[i]] <- graficos[[i]] + geom_text(stat = "count", aes(y = (..count..)/sum(..count..) * 100, label = ..count..), vjust = -0.5,size = size.freq)   # Agregar valores sobre las barras

        if(show.n) graficos[[i]] <- graficos[[i]] + annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dd[,namevar[i]]))), hjust = 1.2, vjust = 1.5, size = size.n)

        ## descriptiu bivariat
      } else {

        dd <- if (show.na) dat else dat %>% dplyr::select(any_of(c(namevar[i], y))) %>% na.omit()



        # Crear gráfico de barras apiladas con porcentajes
        graficos[[i]] <- ggplot(dd, aes_string(x = y, fill = namevar[i])) +
          geom_bar(position = "fill", linewidth = 0.3) +
          scale_y_continuous(labels = percent_format(accuracy = 1)) +
          scale_fill_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +  # Cortar etiquetas largas
          labs(title = lbls[namevar[i]], x = lbl_y, y = "%", fill = "") +
          common_theme +
          theme(legend.position = legend.position,  # Mueve la leyenda abajo
                legend.spacing.y = unit(0.5, "cm"),  # Espaciado entre elementos de la leyenda
                axis.text.x = angle_settings # Orientacion de las etiquetas del eje X
          ) +
          ggtitle(label = str_wrap(lbls[namevar[i]], width = 40))

        # Agregar etiquetas con el porcentaje dentro de cada región
        if (show.freq) {
          graficos[[i]] <- graficos[[i]] +
            geom_text(
              aes(label = scales::percent(..count.. / tapply(..count.., ..x.., sum)[..x..], accuracy = 1)),
              stat = "count",
              position = position_fill(vjust = 0.5),
              color = "black",
              size = size.freq
            )
        }

        if (show.n) {
          df_counts <- dd %>%
            count(!!sym(y))  # Contar observaciones por grupo en el eje x

          graficos[[i]] <- graficos[[i]] +
            geom_text(data = df_counts,
                      aes(x = !!sym(y),
                          y = 1.01,  # Ubicación encima de la barra (ajusta según el gráfico)
                          label = paste0("n = ", n)),
                      inherit.aes = FALSE,
                      size = (size.n-0.5),
                      vjust = 0,  # Coloca el texto arriba
                      color = "black")

          total_n <- sum(df_counts$n)  # Calcula la n total
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = length(unique(dd[[y]])) + 0.5, y = 1.1,
                     label = paste0("n = ", total_n), hjust = 1, size = (size.n + 0.5))
        }

        # Agregar p-valor si es necesario: mateix test que desc_group() (summary.quali)
        if (show.pval) {
          sq <- try(summary.quali(data = as.data.frame(dd), x = namevar[i], group = y,
                                  show.all = FALSE, show.n = FALSE, var.tidy = FALSE), silent = TRUE)
          if (!inherits(sq, "try-error")) {
            graficos[[i]] <- graficos[[i]] +
              annotate("text", x = 1, y = 1.06,
                       label = txt_pval(sq),
                       hjust = 0, size = size.pval)
          }
        }
      }
      ##### variables caracter
    } else if (inherits(dat[[namevar[i]]], "character")) {
      message(paste("La variable",namevar[i], "es tipo caracter y no se ha realizado gr\u00e1fico"))

      ##### variables dates (abans el grafic no s'assignava i es perdia)
    } else if (inherits(dat[[namevar[i]]], "Date") |
               inherits(dat[[namevar[i]]], "POSIXt")){
      # Gráfico de histograma para visualizar la frecuencia de fechas
      graficos[[i]] <- ggplot(dat, aes_string(x = namevar[i])) +
        labs(title = lbls[namevar[i]], x = "Fecha", y = "Frecuencia") +
        common_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dat %>% dplyr::select(namevar[i])))),
                 hjust = 1.2, vjust = 1.5, size = size.n)
      ## scale_x_date() nomes serveix per a 'Date'; per a 'POSIXt' l'escala per defecte
      graficos[[i]] <- if (inherits(dat[[namevar[i]]], "Date")) {
        graficos[[i]] +
          geom_histogram(binwidth = 1, fill = "steelblue", color = "black", linewidth = 0.3) +
          scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "5 days")
      } else {
        graficos[[i]] +
          geom_histogram(bins = 30, fill = "steelblue", color = "black", linewidth = 0.3)
      }
      ##### variables numeriques
    }else {

      if (is.null(y)) {

        dd <- if (show.na) dat else dat %>% dplyr::select(any_of(c(namevar[i]))) %>% na.omit()
        ################# HISTOGRAMA
        ######### UNI
        n_obs <- sum(!is.na(dd[[namevar[i]]]))
        n_bins <- ceiling(log2(n_obs) + 1)
        x <- dd[[namevar[i]]]
        n_breaks <- nclass.Sturges(x)
        breaks <- pretty(range(x, na.rm = TRUE), n = n_breaks)

        graficos[[i]] <- ggplot(dd, aes_string(x = namevar[i])) +
          geom_histogram( fill = color, color = "black", alpha = 0.6, linewidth = 0.3,bins = n_bins,breaks =breaks) + # Histograma
          labs(title = lbls[namevar[i]], x = NULL, y = "Frequency") +
          annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dd %>% dplyr::select(namevar[i])))),
                   hjust = 1.2, vjust = 1.5, size = size.n) +
          common_theme

      } else {

        ## amb 'idvar' es conserva la columna; els NA de l'identificador no eliminen files
        dd <- if (show.na) {
          dat
        } else if (is.null(idvar)) {
          dat %>% dplyr::select(any_of(c(namevar[i], y))) %>% na.omit()
        } else {
          dat[complete.cases(dat[, c(namevar[i], y)]), c(namevar[i], y, idvar), drop = FALSE]
        }
        ######## BIVARIANT
        ## p-valor: mateix test que desc_group() (summary.quanti, segons 'method') i mateixes dades del grafic ('dd')
        sq <- NULL
        if (show.pval) {
          sq <- try(summary.quanti(data = as.data.frame(dd), x = namevar[i], group = y, method = method,
                                   show.all = FALSE, show.n = FALSE, var.tidy = FALSE), silent = TRUE)
          if (inherits(sq, "try-error")) sq <- NULL
        }

        if(bw) outliers <- NA else outliers <- 1.5 # Si mostramos los puntos dispersos, quitamos los outliers del boxplot para no duplicar puntos
        # sino, estos tendran el valor por defecto de este parametro en la funcion geom_boxplot

        graficos[[i]] <- ggplot(dd, aes_string(x = y, y = namevar[i], color = y)) +
          geom_boxplot(fill = "gray80", alpha = 0.5, outlier.shape = outliers) +  # Boxplot con o sin outliers (segun bw)
          labs(title = if (!is.null(sq)) paste0(lbls[namevar[i]], ". ", txt_pval(sq))
                       else lbls[namevar[i]],  # Título con etiqueta de Hmisc
               x = lbl_y,
               y = "") +
          common_theme +
          theme(legend.position = "none",
                axis.text.x = angle_settings # Orientacion de las etiquetas del eje X
                )

        if (is.null(idvar)) {

          if(bw) graficos[[i]] <- graficos[[i]] + geom_jitter(width = 0.2, height = 0, alpha = 0.7)   # Puntos dispersos

        } else {

          ### --- Outliers amb etiqueta: limits dins de cada grup segons 'outlier.method' ---
          v <- dd[[namevar[i]]]
          gr <- addNA(droplevels(factor(dd[[y]])), ifany = TRUE)   # mateix ordre que l'eix X
          q_g <- function(prob) stats::ave(v, gr, FUN = function(z) stats::quantile(z, prob, na.rm = TRUE, names = FALSE))
          lim <- switch(outlier.method,
                        "sd" = {
                          m <- stats::ave(v, gr, FUN = function(z) mean(z, na.rm = TRUE))
                          d <- stats::ave(v, gr, FUN = function(z) stats::sd(z, na.rm = TRUE))
                          list(lo = m - outlier.k * d, hi = m + outlier.k * d)
                        },
                        "iqr" = {
                          q1 <- q_g(0.25); q3 <- q_g(0.75)
                          list(lo = q1 - outlier.k * (q3 - q1), hi = q3 + outlier.k * (q3 - q1))
                        },
                        "mad" = {
                          md <- stats::ave(v, gr, FUN = function(z) stats::median(z, na.rm = TRUE))
                          dm <- stats::ave(v, gr, FUN = function(z) stats::mad(z, na.rm = TRUE))
                          list(lo = md - outlier.k * dm, hi = md + outlier.k * dm)
                        })
          is_out <- !is.na(v) & !is.na(lim$lo) & (v < lim$lo | v > lim$hi)

          ## posicio X dels punts: el jitter es calcula un sol cop (llavor fixa) perque l'etiqueta apunti al punt;
          ## es restaura l'estat del generador aleatori de l'usuari
          dd$.x_pt <- as.numeric(gr)
          if (bw) {
            seed_old <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
            set.seed(1234)
            dd$.x_pt <- dd$.x_pt + stats::runif(nrow(dd), -0.2, 0.2)
            if (is.null(seed_old)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", seed_old, envir = globalenv())
            graficos[[i]] <- graficos[[i]] + geom_point(data = dd, aes(x = .x_pt), alpha = 0.7)   # Puntos dispersos
          }

          if (any(is_out)) {
            dd$.id <- as.character(dd[[idvar]])
            dd$.dist <- pmax(lim$lo - v, v - lim$hi)       # distancia al limit
            out <- dd[is_out, , drop = FALSE]
            out <- out[order(-out$.dist), , drop = FALSE]
            out_lab <- utils::head(out, outlier.max)

            graficos[[i]] <- graficos[[i]] +
              geom_point(data = out, aes(x = .x_pt), shape = 21, size = 2.6, colour = "black", fill = NA) +
              ggrepel::geom_text_repel(data = out_lab, aes(x = .x_pt, label = .id), colour = "black",
                                       size = size.outlier, seed = 1234, min.segment.length = 0,
                                       max.overlaps = Inf, show.legend = FALSE)

            list_out[[namevar[i]]] <- data.frame(variable = namevar[i], group = as.character(gr[is_out]),
                                                 id = dd[[idvar]][is_out], value = v[is_out],
                                                 lower = lim$lo[is_out], upper = lim$hi[is_out],
                                                 method = outlier.method, k = outlier.k,
                                                 stringsAsFactors = FALSE)
          }

          ## peu: criteri i nombre d'outliers (etiquetats / total)
          txt_crit <- switch(outlier.method,
                             "sd" = paste0("media \u00b1 ", outlier.k, " SD"),
                             "iqr" = paste0("Q1 - ", outlier.k, " IQR, Q3 + ", outlier.k, " IQR"),
                             "mad" = paste0("mediana \u00b1 ", outlier.k, " MAD"))
          graficos[[i]] <- graficos[[i]] +
            labs(caption = paste0("Outliers (", txt_crit, " por grupo): ", sum(is_out),
                                  if (sum(is_out) > outlier.max) paste0(", etiquetados los ", outlier.max, " m\u00e1s extremos") else ""))
        }

        if(show.n){
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = Inf, y = Inf,
                     label = paste("n =", nrow(dat %>% dplyr::select(any_of(c(namevar[i],y))) %>% na.omit())), hjust = 1.2, vjust = 1.5, size = size.n)
        }

      }

    }

  }
  graficos <- graficos[!sapply(graficos, is.null)]
  if (!is.null(idvar)) attr(graficos, "outliers") <- if (length(list_out) > 0) do.call("rbind", unname(list_out)) else NULL

  if (topdf) {
    ggplot_to_pdf(graficos, row = rowcol[1], col = rowcol[2], name.file = nameFile)
  } else if (list.plots){
    return(graficos)
  } else {
    return(walk(graficos, print))
  }
}
