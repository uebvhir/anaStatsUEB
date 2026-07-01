#' Gráficos descriptivos automáticos para variables categóricas y numéricas
#'
#' Esta función genera gráficos descriptivos (barras, histogramas, boxplots) para un conjunto de variables,
#' de forma univariada o bivariada, con opciones para personalizar estilo, leyendas, etiquetas, tamaños de texto, etc.
#'
#' @param dat Data frame que contiene las variables a analizar.
#' @param covariates Vector de nombres de variables a graficar. Si se usa \code{frml}, se sobreescribe.
#' @param frml Fórmula del tipo \code{y ~ x1 + x2 + ...} para definir la variable dependiente y las independientes.
#' @param y Variable de agrupación (dependiente) para análisis bivariado. Ignorado si se usa \code{frml}.
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
#' @param ... Argumentos adicionales (no usados actualmente).
#'
#' @return Por defecto imprime los gráficos. Si \code{list.plots = TRUE}, retorna una lista de objetos ggplot.
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
                        ...) {

  graficos <- list()

  ## en el cas de que hi hagi formula seleccionem el grup i les covariates
  if (!is.null(frml)) {
    covariates <- rhs.vars(frml)
    if (!is.null(lhs.vars(frml))) {y <- lhs.vars(frml)}
  }

  ## en el cas de que seleccionem variables a analitzar reduim bbdd a variables necesaies
  if (!is.null(covariates)) {
    dat %<>% dplyr::select(any_of(c(covariates,y)))
  }

  # if (sum(!is.na(dat[, y])) > bw.n.max) {
  # bw <- FALSE
  # }

  ## eliminem columnes buides
  dat <- remove_empty(dat, which = c("cols"))

  ## Labels i names  de les a gràficar excepte la que crea grup.
  lbls <- Hmisc::label(dat[!names(dat) %in% y])
  lbls[lbls == ""] <- names(dat)[!names(dat) %in% y][lbls == ""]
  namevar <- names(lbls)

  lbl_y <- ifelse(Hmisc::label(dat[,y]) == "",y, Hmisc::label(dat[,y]))



  common_theme <- theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = size.title),
      panel.grid.major = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )

  # Especificamos orientacion de las etiquetas del eje X en el boxplot (de momento solo boxplot, TODO añadir en barplot)
  angle_settings <- switch(
    angle_x,
    "horizontal" = element_text(angle = 0, hjust = 0.5, vjust = 1),
    "diagonal"   = element_text(angle = 45, hjust = 1, vjust = 1),
    "vertical"   = element_text(angle = 90, hjust = 1, vjust = 0.5),
    element_text(angle = 0, hjust = 0.5, vjust = 1)  # valor por defecto si no coincide
  )



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

        # Agregar p-valor si es necesario
        if (show.pval) {
          info_test <- test_categoricas(data = dd, factor1 = y, factor2 = namevar[i])
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = 1, y = 1.06,
                     label = paste(info_test$test, "p:", format.pval(info_test$pvalor, digits = 3, eps = 0.001)),
                     hjust = 0, size = size.pval)
        }
      }
      ##### variables caracter
    } else if (inherits(dat[[namevar[i]]], "character")) {
      message(paste("La variable",namevar[i], "es tipo caracter y no se ha realizado gráfico"))

      ##### variables dates
    } else if (inherits(dat[[namevar[i]]], "Date") |
               inherits(dat[[namevar[i]]], "POSIXt")){
      library(ggplot2)
      # Gráfico de histograma para visualizar la frecuencia de fechas
      ggplot(dat, aes_string(x = namevar[i])) +
        geom_histogram(binwidth = 1, fill = "steelblue", color = "black", linewidth = 0.3) +
        labs(title = lbls[namevar[i]], x = "Fecha", y = "Frecuencia") +
        scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "5 days") +
        theme_minimal() +
        common_theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dat %>% dplyr::select(namevar[i])))),
                 hjust = 1.2, vjust = 1.5, size = size.n)
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
          # geom_rug(sides = "b") +  # Agregar rayitas (rug plot) en la base
          labs(title = lbls[namevar[i]], x = NULL, y = "Frequency") +
          annotate("text", x = Inf, y = Inf, label = paste0("n = ", sum(complete.cases(dd %>% dplyr::select(namevar[i])))),
                   hjust = 1.2, vjust = 1.5, size = size.n) +
          common_theme

      } else {

        dd <- if (show.na) dat else dat %>% dplyr::select(any_of(c(namevar[i], y))) %>% na.omit()
        ######## BIVARIANT
        # Gráfico con ggplot2
        if(show.pval)       info_test <- test_numericas(factor_col = y,numerica_col = namevar[i] ,data = dat,parametrico = FALSE)

        if(bw) outliers <- NA else outliers <- 1.5 # Si mostramos los puntos dispersos, quitamos los outliers del boxplot para no duplicar puntos
        # sino, estos tendran el valor por defecto de este parametro en la funcion geom_boxplot

        graficos[[i]] <- ggplot(dd, aes_string(x = y, y = namevar[i], color = y)) +
          geom_boxplot(fill = "gray80", alpha = 0.5, outlier.shape = outliers) +  # Boxplot con o sin outliers (segun bw)
          labs(title = ifelse(show.pval, paste(lbls[namevar[i]],".",info_test$test,  "p:", format.pval(info_test$pvalor, digits = 3, eps = 0.001)),
                              lbls[namevar[i]]),  # Título con etiqueta de Hmisc
               x = lbl_y,
               y = "") +
          common_theme +
          theme(legend.position = "none",
                axis.text.x = angle_settings # Orientacion de las etiquetas del eje X
                )

        if(bw) graficos[[i]] <- graficos[[i]] + geom_jitter(width = 0.2, height = 0, alpha = 0.7)   # Puntos dispersos

        if(show.n){
          graficos[[i]] <- graficos[[i]] +
            annotate("text", x = Inf, y = Inf,
                     label = paste("n =", nrow(dat %>% dplyr::select(any_of(c(namevar[i],y))) %>% na.omit())), hjust = 1.2, vjust = 1.5, size = size.n)
        }

      }

    }

  }
  graficos <- graficos[!sapply(graficos, is.null)]

  if (topdf) {
    ggplot_to_pdf(graficos, row = rowcol[1], col = rowcol[2], name.file = nameFile)
  } else if (list.plots){
    return(graficos)
  } else {
    return(walk(graficos, print))
  }
}

