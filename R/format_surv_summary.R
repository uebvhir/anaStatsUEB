#' A format_surv_summary Function
#'
#' Genera una tabla resumen de un modelo de supervivencia ajustado mediante
#' \code{survfit()}, mostrando el número de sujetos en riesgo, eventos,
#' estimaciones de supervivencia e intervalos de confianza en tiempos
#' específicos. Opcionalmente puede presentar los resultados estratificados
#' por grupos.
#'
#' @param fit objeto de clase \code{survfit} generado mediante la función
#' \code{survival::survfit()}.
#' @param times vector numérico con los tiempos en los que se desea obtener
#' el resumen de supervivencia.
#' @param stratified TRUE o FALSE indicando si el modelo contiene estratos y
#' los resultados deben mostrarse agrupados por grupo. Por defecto FALSE.
#'
#' @return Una tabla formateada mediante \code{kableExtra} que incluye:
#' \itemize{
#'   \item Tiempo.
#'   \item Número de sujetos en riesgo (\code{n.risk}).
#'   \item Número de eventos (\code{n.event}).
#'   \item Probabilidad de supervivencia estimada.
#'   \item Error estándar.
#'   \item Límite inferior del intervalo de confianza del 95\%.
#'   \item Límite superior del intervalo de confianza del 95\%.
#' }
#'
#' @details
#' Cuando \code{stratified = FALSE}, la función genera una única tabla con los
#' resultados globales del modelo de supervivencia.
#'
#' Cuando \code{stratified = TRUE}:
#' \itemize{
#'   \item Se extraen automáticamente los nombres de los grupos a partir de los estratos.
#'   \item Los resultados se ordenan por grupo y tiempo.
#'   \item La tabla se presenta agrupando visualmente las filas correspondientes
#'   a cada estrato mediante \code{kableExtra::pack_rows()}.
#' }
#'
#' Todas las estimaciones de supervivencia, errores estándar e intervalos de
#' confianza se redondean a tres decimales.
#'
#' @export format_surv_summary
#' @import dplyr tibble survival kableExtra
#'
#' @author Alba García Zarzosa
#'
#' @examples
#' \dontrun{
#' library(survival)
#'
#' # Modelo de supervivencia simple
#' fit <- survfit(Surv(time, status) ~ 1, data = lung)
#'
#' format_surv_summary(
#'   fit = fit,
#'   times = c(100, 200, 300)
#' )
#'
#' # Modelo estratificado
#' fit_strat <- survfit(Surv(time, status) ~ sex, data = lung)
#'
#' format_surv_summary(
#'   fit = fit_strat,
#'   times = c(100, 200, 300),
#'   stratified = TRUE
#' )
#' }
#'
#' @keywords survival survfit Kaplan-Meier summary table
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
