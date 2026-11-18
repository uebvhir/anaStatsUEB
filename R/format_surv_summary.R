#' @title Tabla resumen de un modelo de supervivencia en tiempos concretos
#'
#' @description
#' Genera una tabla resumen de un modelo de supervivencia ajustado con
#' \code{survival::survfit()}, que muestra el número de sujetos en riesgo, los
#' eventos, la estimación de supervivencia, el error estándar y el intervalo de
#' confianza en los tiempos indicados. Si el modelo tiene estratos, los
#' resultados se presentan agrupados por estrato.
#'
#' @param fit Objeto de clase \code{survfit} generado con
#'   \code{survival::survfit()}.
#' @param times Vector numérico con los tiempos en los que se desea obtener el
#'   resumen de supervivencia.
#'
#' @return Una tabla con formato (objeto \code{kableExtra}) con las columnas:
#' \itemize{
#'   \item Tiempo.
#'   \item N en riesgo (\code{n.risk}).
#'   \item N evento (\code{n.event}).
#'   \item Supervivencia estimada.
#'   \item Error estándar.
#'   \item IC inferior.
#'   \item IC superior.
#' }
#'
#' @details
#' La función no tiene ningún argumento para indicar la estratificación: se
#' detecta automáticamente a partir del objeto \code{fit}.
#'
#' Si \code{fit} no tiene estratos (por ejemplo, \code{Surv(time, status) ~ 1}),
#' se genera una única tabla con los resultados globales.
#'
#' Si \code{fit} tiene estratos (por ejemplo, \code{Surv(time, status) ~ sex}):
#' \itemize{
#'   \item el nombre del grupo se extrae de la etiqueta del estrato, conservando
#'     la parte posterior al último signo \code{=};
#'   \item los resultados se ordenan por grupo y tiempo;
#'   \item las filas de cada estrato se agrupan visualmente con
#'     \code{kableExtra::pack_rows()}.
#' }
#'
#' Los valores se calculan con \code{summary(fit, times = times)}. Por tanto, el
#' nivel de los intervalos de confianza es el indicado en \code{survfit()}
#' (95\% por defecto, argumento \code{conf.int}), y los tiempos posteriores al
#' máximo seguimiento no aparecen en la tabla. La supervivencia, el error
#' estándar y los límites se redondean a tres decimales.
#'
#' Limitación: con más de un factor de estratificación (por ejemplo,
#' \code{~ sex + ph}) la etiqueta del grupo conserva solo el valor del último
#' factor, por lo que estratos distintos pueden recibir el mismo nombre.
#'
#' @seealso \code{\link[survival]{survfit}}, \code{\link[survival]{summary.survfit}},
#'   \code{\link{kable_ueb}}, \code{\link{coxPretty}}
#'
#' @examples
#' library(survival)
#'
#' # Modelo de supervivencia simple
#' fit <- survfit(Surv(time, status) ~ 1, data = lung)
#' format_surv_summary(fit = fit, times = c(100, 200, 300))
#'
#' # Modelo estratificado (se detecta automaticamente)
#' fit_strat <- survfit(Surv(time, status) ~ sex, data = lung)
#' format_surv_summary(fit = fit_strat, times = c(100, 200, 300))
#'
#' @keywords survival survfit Kaplan-Meier summary table
#'
#' @import dplyr tibble survival kableExtra
#'
#' @author
#' Alba García Zarzosa
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
format_surv_summary <- function(fit, times) {

  s <- summary(fit, times = times)

  # Tabla base
  df <- tibble(
    grupo = if (!is.null(s$strata)) gsub(".*=", "", s$strata) else NA_character_,
    time = s$time,
    n.risk = s$n.risk,
    n.event = s$n.event,
    survival = s$surv,
    std_err = s$std.err,
    lower_95_CI = s$lower,
    upper_95_CI = s$upper
  ) %>%
    mutate(
      across(
        -c(grupo, time, n.risk, n.event),
        ~ round(., 3)
      )
    )

  col_names <- c(
    "Tiempo",
    "N en riesgo",
    "N evento",
    "Supervivencia",
    "Error estándar",
    "IC inferior",
    "IC superior"
  )

  # Caso no estratificado
  if (all(is.na(df$grupo))) {

    return(
      kable_ueb(
        df %>% select(-grupo),
        col.names = col_names
      )
    )

  }

  # Caso estratificado
  df <- df %>%
    arrange(grupo, time)

  idx <- rle(df$grupo)

  index_pack <- idx$lengths
  names(index_pack) <- idx$values

  kable_ueb(
    df %>% select(-grupo),
    col.names = col_names
  ) %>%
    kableExtra::pack_rows(
      index = index_pack,
      background = "#E6E6FA"
    )

}
