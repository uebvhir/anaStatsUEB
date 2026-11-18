#' @title Gráficos de evolución longitudinal de variables numéricas
#'
#' @description
#' Genera gráficos de evolución longitudinal para una o varias variables
#' numéricas a lo largo de distintas visitas o momentos temporales. Con una sola
#' variable, además ajusta un modelo lineal mixto para evaluar la evolución
#' temporal y muestra la tabla de resultados y el gráfico de efectos del modelo.
#'
#' @param df Data frame que contiene las variables a analizar.
#' @param vars_y Vector de caracteres con los nombres de las variables numéricas
#'   cuya evolución se desea representar.
#' @param vars_x Variable temporal o de visitas utilizada en el eje X. Por
#'   defecto, \code{"redcap_event_name"}.
#' @param var_id Identificador único de sujeto. Por defecto, \code{"id"}. Solo se
#'   utiliza cuando \code{vars_y} tiene varias variables: con una única variable,
#'   la función usa siempre la columna llamada \code{id}.
#' @param test_name Texto utilizado en los títulos de los gráficos
#'   (\code{"<test_name> evolution"} o \code{"<test_name> combined"}). Por
#'   defecto, \code{"Results"}.
#' @param show.title Lógico. Si es \code{TRUE} (por defecto), antes de los
#'   resultados se muestra un título con la etiqueta de la variable. Solo se aplica
#'   con una única variable.
#'
#' @return
#' \code{NULL}, de forma invisible. Los gráficos y resultados se muestran
#' directamente en la salida.
#'
#' @details
#' Con una única variable en \code{vars_y}:
#' \itemize{
#'   \item se representa la media en cada visita, con barras de error del error
#'     estándar;
#'   \item se ajusta un modelo lineal mixto (\code{nlme::lme()}, REML) con
#'     intercepto aleatorio por \code{id};
#'   \item si hay al menos 3 valores distintos de \code{id}, se ajusta también un
#'     modelo con pendiente aleatoria y se elige el mejor mediante el contraste de
#'     razón de verosimilitudes (se usa el de pendiente aleatoria si p < 0.05);
#'   \item se muestran la tabla de coeficientes (\code{sjPlot::tab_model()}) y el
#'     gráfico de efectos (\code{sjPlot::plot_model()}).
#' }
#'
#' Con varias variables en \code{vars_y}:
#' \itemize{
#'   \item se genera un único gráfico combinado, con un color por variable
#'     (paleta \code{"Dark2"}, de 8 colores como máximo);
#'   \item se muestran medias y errores estándar por visita, usando las
#'     etiquetas (\code{Hmisc::label}) de las variables;
#'   \item no se ajustan modelos.
#' }
#'
#' \code{vars_x} debe ser una variable categórica (por ejemplo, un factor de
#' visitas). La salida está pensada para un informe de R Markdown con
#' \code{results = 'asis'}. Los ejes tienen etiquetas fijas en inglés
#' (\code{"Visit"} y \code{"Puntuation"}). Si el modelo con pendiente aleatoria no
#' converge con pocos datos, el error no se controla.
#'
#' @seealso \code{\link[nlme]{lme}}, \code{\link[sjPlot]{tab_model}},
#'   \code{\link[sjPlot]{plot_model}}, \code{\link[ggplot2]{stat_summary}},
#'   \code{\link{desc_group_rep}}
#'
#' @examples
#' \dontrun{
#' set.seed(1)
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
#' # Evolucion de una unica variable
#' plot_evolucion(
#'   df = data_long,
#'   vars_y = "score",
#'   vars_x = "visita",
#'   var_id = "id"
#' )
#'
#' # Evolucion conjunta de varias variables
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
#'
#' @import dplyr tidyr ggplot2 Hmisc nlme sjPlot RColorBrewer magrittr
#'
#' @author
#' Miriam Mota \email{miriam.mota@@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
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
