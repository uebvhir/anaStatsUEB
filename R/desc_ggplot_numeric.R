#' A desc_ggplot_numeric Function
#'
#' Genera gráficos exploratorios para una o varias variables numéricas respecto a
#' un conjunto de variables explicativas. Para variables categóricas crea
#' boxplots mediante \code{ggplot2} y opcionalmente añade puntos individuales,
#' tamaño muestral y resultado del test estadístico. Para variables numéricas
#' calcula la correlación utilizando \code{quickCor()}.
#'
#' Si se especifican varias variables respuesta en \code{y}, la función genera
#' todos los gráficos correspondientes para cada combinación entre las variables
#' respuesta y las variables explicativas.
#'
#' @param y character vector con el nombre de una o varias variables respuesta numéricas.
#' @param covariates character vector con los nombres de las variables explicativas a evaluar.
#' @param dat data frame que contiene las variables a representar.
#' @param rowcol vector \code{c(nrows, ncols)} para definir la disposición de los gráficos.
#' Actualmente no se utiliza internamente.
#' @param bw TRUE o FALSE indicando si se añaden los puntos individuales (jitter).
#' Cuando es TRUE también se eliminan los outliers del boxplot. Por defecto FALSE.
#' @param show.n TRUE o FALSE indicando si se muestra el tamaño muestral en el gráfico.
#' Por defecto FALSE.
#' @param size.title tamaño del título del gráfico. Por defecto 10.
#' @param size.pval tamaño del texto asociado al p-valor. Por defecto 3.
#' Actualmente este argumento no tiene efecto sobre el gráfico.
#' @param show.pval TRUE o FALSE indicando si se muestra el resultado del test
#' estadístico y el p-valor en el título. Por defecto FALSE.
#' @param angle_x orientación de las etiquetas del eje X. Valores posibles:
#' \code{"horizontal"}, \code{"diagonal"} o \code{"vertical"}.
#' @param size.n tamaño del texto utilizado para mostrar el tamaño muestral.
#' Por defecto 3.
#' @param flip TRUE o FALSE indicando si los boxplots se muestran girados mediante
#' \code{coord_flip()}. No afecta a los gráficos de correlación. Por defecto FALSE.
#'
#' @return Devuelve invisiblemente una lista con los objetos \code{ggplot}
#' generados y, además, los imprime en pantalla.
#'
#' @export desc_ggplot_numeric
#' @import ggplot2 dplyr purrr Hmisc
#' @author Àlex Martí Barrera, Miriam Mota \email{mmota.foix@@gmail.com}
#'
#' @examples
#' set.seed(1111)
#'
#' df <- data.frame(
#'   grupo = factor(sample(c("A","B"), 100, replace = TRUE)),
#'   tratamiento = factor(sample(c("Sí","No"), 100, replace = TRUE)),
#'   edad = rnorm(100, 65, 10),
#'   peso = rnorm(100, 75, 12),
#'   altura = runif(100, 1.5, 2)
#' )
#'
#' # Boxplot básico
#' desc_ggplot_numeric(
#'   y = "edad",
#'   covariates = "grupo",
#'   dat = df
#' )
#'
#' # Mostrando p-valor y tamaño muestral
#' desc_ggplot_numeric(
#'   y = "edad",
#'   covariates = "grupo",
#'   dat = df,
#'   show.pval = TRUE,
#'   show.n = TRUE
#' )
#'
#' # Añadiendo puntos individuales y girando el gráfico
#' desc_ggplot_numeric(
#'   y = "edad",
#'   covariates = c("grupo","tratamiento"),
#'   dat = df,
#'   bw = TRUE,
#'   flip = TRUE
#' )
#'
#' # Varias variables respuesta
#' desc_ggplot_numeric(
#'   y = c("edad","peso"),
#'   covariates = c("grupo","tratamiento"),
#'   dat = df
#' )
#'
#' # Gráfico de correlación
#' desc_ggplot_numeric(
#'   y = "edad",
#'   covariates = "altura",
#'   dat = df
#' )
#'
#' @keywords plots ggplot boxplot descriptive

desc_ggplot_numeric <- function(
    y,
    covariates,
    dat,
    rowcol = c(1,1),
    bw = FALSE,
    show.n = FALSE,
    size.title = 10,
    size.pval = 3,
    show.pval = FALSE,
    angle_x = "horizontal",
    size.n = 3,
    flip = TRUE){

  plt_list <- list()
  k <- 1

  for (yy in y){

    for (cov in covariates){

      if (is.factor(dat[[cov]])) {

        angle_settings <- switch(
          angle_x,
          horizontal = element_text(angle = 0, hjust = 0.5, vjust = 1),
          diagonal   = element_text(angle = 45, hjust = 1, vjust = 1),
          vertical   = element_text(angle = 90, hjust = 1, vjust = 0.5),
          element_text(angle = 0, hjust = 0.5, vjust = 1)
        )

        lbl_y <- ifelse(
          Hmisc::label(dat[[yy]]) == "",
          yy,
          Hmisc::label(dat[[yy]])
        )

        common_theme <-
          theme_minimal() +
          theme(
            plot.title = element_text(
              hjust = 0.5,
              size = size.title
            ),
            panel.grid.major = element_blank(),
            plot.margin = margin(10,10,10,10)
          )

        outliers <- if (bw) NA else 1.5

        if (show.pval) {
          info_test <- test_numericas(
            factor_col = cov,
            numerica_col = yy,
            data = dat,
            parametrico = FALSE
          )
        }

        name <- ifelse(
          Hmisc::label(dat[[cov]]) == "",
          cov,
          Hmisc::label(dat[[cov]])
        )

        p <- ggplot(
          dat,
          aes_string(
            x = yy,
            y = cov,
            colour = yy
          )
        ) +
          geom_boxplot(
            fill = "grey80",
            alpha = 0.5,
            outlier.shape = outliers
          ) +
          labs(
            title =
              if (show.pval) {
                paste0(
                  name,
                  ". ",
                  info_test$test,
                  " p: ",
                  format.pval(info_test$pvalor,
                              digits = 3,
                              eps = 0.001)
                )
              } else {
                name
              },
            x = lbl_y,
            y = ""
          ) +
          common_theme +
          theme(
            legend.position = "none",
            axis.text.x = angle_settings
          )

        if (bw) {
          p <- p +
            geom_jitter(
              width = 0.2,
              height = 0,
              alpha = 0.7
            )
        }

        if (show.n) {
          p <- p +
            annotate(
              "text",
              x = Inf,
              y = Inf,
              label = paste(
                "n =",
                nrow(
                  dat |>
                    dplyr::select(dplyr::any_of(c(cov, yy))) |>
                    tidyr::drop_na()
                )
              ),
              hjust = 1.2,
              vjust = 1.5,
              size = size.n
            )
        }

        if (flip) {
          p <- p + coord_flip()
        }
        plt_list[[k]] <- p
        k <- k + 1

      } else {
        plt_list[[k]] <-
          quickCor(
            dat = dat,
            x = cov,
            y = yy,
            show.res = FALSE
          )
        k <- k + 1
      }
    }
  }

  plt_list <- Filter(Negate(is.null), plt_list)

  purrr::walk(plt_list, print)

  # invisible(plt_list)

}
