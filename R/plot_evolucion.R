#' A plot_evolucion Function
#'
#' Genera gráficos de evolución longitudinal para una o varias variables numéricas
#' a lo largo de diferentes visitas o momentos temporales. Cuando se analiza una
#' única variable, además ajusta un modelo lineal mixto para evaluar la evolución
#' temporal y muestra tanto la tabla de resultados como el gráfico de efectos del modelo.
#'
#' @param df data frame que contiene las variables a analizar.
#' @param vars_y vector de caracteres con los nombres de las variables numéricas
#' cuya evolución se desea representar.
#' @param vars_x variable temporal o de visitas utilizada en el eje X.
#' Por defecto \code{"redcap_event_name"}.
#' @param var_id identificador único de sujeto. Por defecto \code{"id"}.
#' @param test_name texto utilizado en los títulos de los gráficos.
#' Por defecto \code{"Results"}.
#' @param show.title TRUE o FALSE indicando si se muestra el título con la etiqueta
#' de la variable antes de los resultados. Por defecto TRUE.
#'
#' @details
#' Cuando \code{vars_y} contiene una única variable:
#' \itemize{
#'   \item Se representa la media en cada visita.
#'   \item Se muestran barras de error correspondientes al error estándar.
#'   \item Se ajusta un modelo lineal mixto con intercepto aleatorio.
#'   \item Si existen suficientes sujetos, también se ajusta un modelo con pendiente
#'   aleatoria y se selecciona automáticamente el mejor modelo mediante comparación
#'   de verosimilitud.
#'   \item Se muestran los coeficientes estimados y un gráfico de efectos.
#' }
#'
#' Cuando \code{vars_y} contiene varias variables:
#' \itemize{
#'   \item Se genera un único gráfico combinado.
#'   \item Cada variable se representa con un color diferente.
#'   \item Se muestran medias y errores estándar para cada visita.
#'   \item No se ajustan modelos estadísticos.
#' }
#'
#' @return
#' La función genera gráficos y resultados estadísticos directamente en la salida.
#' No devuelve explícitamente ningún objeto.
#'
#' @export plot_evolucion
#' @import dplyr tidyr ggplot2 Hmisc nlme sjPlot RColorBrewer magrittr
#'
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#'
#' @examples
#' \dontrun{
#' data_long <- data.frame(
#'   id = rep(1:20, each = 3),
#'   visita = factor(rep(c("Basal", "Mes1", "Mes3"), 20)),
#'   score = rnorm(60, 50, 10),
#'   score2 = rnorm(60, 70, 15)
#' )
#'
#' Hmisc::label(data_long$score) <- "Test score"
#' Hmisc::label(data_long$score2) <- "Secondary score"
#'
#' # Evolución de una única variable
#' plot_evolucion(
#'   df = data_long,
#'   vars_y = "score",
#'   vars_x = "visita",
#'   var_id = "id"
#' )
#'
#' # Evolución conjunta de varias variables
#' plot_evolucion(
#'   df = data_long,
#'   vars_y = c("score", "score2"),
#'   vars_x = "visita",
#'   var_id = "id",
#'   test_name = "Clinical outcomes"
#' )
#' }
#'
#' @keywords plots longitudinal mixed-model repeated-measures evolution

plot_evolucion <- function(df, vars_y, vars_x = "redcap_event_name", var_id = "id", test_name = "Results", show.title = T) {

  require(nlme,RColorBrewer,sjPlot)

  if (length(vars_y) == 1) {

    # Eliminamos NAs
    df %<>% filter(!is.na(.data[[vars_y]]), is.finite(.data[[vars_y]]))

    # Plot evolucion (solo la media y el error estandar)
    if(show.title) {
      cat(" \n####", Hmisc::label(df[[vars_y]]), " \n")
    }

    print(ggplot(df, aes_string(x = vars_x, y = vars_y, group = 1)) +
            stat_summary(fun = mean, geom = "line", size = 1.2, color = "#0072B2") +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "#0072B2") +
            labs(title = paste(test_name, "evolution"),
                 x = "Visit", y = "Puntuation") +
            theme_minimal())

    cat("\n")

    ### MODEL
    ctrl <- lmeControl(opt = 'optim')
    frml <- as.formula(paste0(vars_y, "~", vars_x))

    model1 <- lme(frml, random = ~1| id, data = df[which(complete.cases(df[,vars_y])),],
                  method = "REML", control = ctrl)

    # El modelo 2 falla si hay pocas muestras
    # PENDIENTE ARREGLAR
    # Para hacerlo simplemente hay que asignar automáticamente el model a model1, sin generar model2 en el caso de que de error

    if (length(unique(df$id)) >= 3) {

      model2 <- lme(frml, random = as.formula(paste0("~", vars_x, "| id")),
                    data = df[which(complete.cases(df[,vars_y])),], method = "REML", control = ctrl)

      if (na.omit(anova(model1, model2)$"p-value") < 0.05) {
        model <- model2
      } else {
        model <- model1
      }

    } else {
      model <- model1
    }
    cat("\n")
    cat(tab_model(model,show.df = F)$knitr,"\n--------\n")
    cat("\n")
    print(plot_model(model, colors = "#8d35cc",show.values = T, title = Hmisc::label(df[[vars_y]])))
    cat("\n")

  } else {
    # Plot combinado (sin modelo)
    df_long <- df %>%
      select(all_of(c(var_id, vars_x, vars_y))) %>%
      mutate(across(all_of(vars_y), ~ as.numeric(.))) %>%
      pivot_longer(cols = all_of(vars_y), names_to = "variable", values_to = "puntuation") %>%
      filter(!is.na(puntuation))

    # Crear etiquetas bonitas a partir de los labels
    label_vector <- sapply(vars_y, function(var) Hmisc::label(df[[var]]))
    names(label_vector) <- vars_y

    # Reetiquetar los niveles de df_long$variable con los nombres bonitos
    df_long <- df_long %>%
      mutate(variable = factor(variable, levels = vars_y, labels = label_vector))

    # Generar una paleta de colores automáticamente en función del número de variables
    num_vars <- length(vars_y)
    color_palette <-
      brewer.pal(min(num_vars, 12), "Dark2")  # 'Dark2' es una paleta de colores para 8 o menos variables
    # Asignar colores de manera dinámica a las variables
    color_mapping <- setNames(color_palette, label_vector)

    print(ggplot(df_long, aes_string(x = vars_x, y = "puntuation", color = "variable", group = "variable")) +
            stat_summary(fun = mean, geom = "line", size = 1.2) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
            labs(title = paste(test_name, "combined"), x = "Visit", y = "Puntuation") +
            theme_minimal() +
            scale_color_manual(values = color_mapping))
    cat("\n")

  }
}
