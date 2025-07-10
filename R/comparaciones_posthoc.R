#' Comparaciones post-hoc entre grupos
#'
#' Realiza comparaciones post-hoc para un conjunto de variables numéricas o categóricas, según una variable de agrupación con más de dos niveles.
#'
#' - Para variables numéricas, se aplica el test de Dunn con corrección de Bonferroni.
#' - Para variables categóricas, se aplica el test de Tukey HSD tras un ANOVA.
#'
#' El resultado incluye una tabla con los p-valores de cada comparación y una versión formateada para informes en HTML.
#'
#' @param covariates Vector de nombres de variables a comparar.
#' @param data Dataframe con las variables.
#' @param group Nombre de la variable de agrupación (factor con más de 2 niveles).
#' @param caption Título de la tabla formateada (por defecto: "Comparaciones post-hoc").
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{\code{tabla}}{Dataframe con los p-valores sin formato.}
#'   \item{\code{kable}}{Tabla HTML formateada con `kableExtra`.}
#' }
#'
#' @details
#' Las variables numéricas se identifican con <sub>1</sub> y las categóricas con <sub>2</sub> en la tabla resultante.
#'
#' @export comparaciones_posthoc
#' @importFrom dunn.test dunn.test
#' @importFrom Hmisc label
#' @importFrom kableExtra kable kable_styling row_spec column_spec footnote cell_spec
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows mutate across
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   grupo = factor(rep(c("A", "B", "C"), each = 10)),
#'   x = rnorm(30),
#'   y = factor(sample(c("sí", "no", "quizás"), 30, replace = TRUE))
#' )
#' comparaciones_posthoc(covariates = c("x", "y"), data = dat, group = "grupo")$kable
#' }
comparaciones_posthoc <- function(covariates, data, group, caption = "Comparaciones post-hoc") {
  if (!group %in% names(data)) {
    stop("La variable de agrupación no está en el dataframe.")
  }

  grupo <- data[[group]]
  if (!is.factor(grupo)) grupo <- as.factor(grupo)
  if (nlevels(grupo) <= 2) {
    stop("La variable de agrupación debe tener más de 2 niveles.")
  }

  ordenar_comparacion <- function(nombre) {
    partes <- unlist(strsplit(nombre, " ?- ?"))
    if (length(partes) != 2) return(nombre)
    partes_ordenadas <- sort(partes)
    paste(partes_ordenadas, collapse = " vs ")
  }

  resultado_lista <- list()
  tipo_variable <- c()

  for (var in covariates) {
    x <- data[[var]]
    df_temp <- data[!is.na(x) & !is.na(grupo), , drop = FALSE]
    x <- df_temp[[var]]
    g <- df_temp[[group]]
    label_var <- Hmisc::label(data[[var]])
    if (is.null(label_var) || label_var == "") label_var <- var

    if (is.numeric(x)) {
      dt <- dunn.test::dunn.test(as.numeric(x), g, method = "bonferroni", kw = FALSE)
      comparaciones <- sapply(dt$comparisons, ordenar_comparacion)
      tabla <- data.frame(Comparacion = comparaciones, P = dt$P.adjusted)
      tipo_variable[label_var] <- "<sub>1</sub>"
    } else if (is.factor(x)) {
      modelo <- aov(as.numeric(x) ~ g)
      tuk <- TukeyHSD(modelo)
      comparaciones <- sapply(rownames(tuk$g), ordenar_comparacion)
      tabla <- data.frame(Comparacion = comparaciones, P = tuk$g[, "p adj"])
      tipo_variable[label_var] <- "<sub>2</sub>"
    } else {
      next
    }

    tabla$Variable <- label_var
    resultado_lista[[label_var]] <- tabla
  }

  if (length(resultado_lista) == 0) {
    stop("Ninguna variable válida para comparación post-hoc.")
  }

  resultado_df <- bind_rows(resultado_lista)
  resultado_df$Variable <- paste0(resultado_df$Variable, tipo_variable[resultado_df$Variable])

  tabla_pivot <- resultado_df %>%
    pivot_wider(names_from = Comparacion, values_from = P)

  tabla_orig <- tabla_pivot

  tabla_formateada <- tabla_pivot %>%
    mutate(across(
      -Variable,
      ~ ifelse(
        is.na(.), NA,
        ifelse(
          . < 0.001,
          cell_spec("<0.001", bold = TRUE),
          ifelse(. < 0.05,
                 cell_spec(sprintf("%.3f", .), bold = TRUE),
                 sprintf("%.3f", .)
          )
        )
      )
    ))

  tabla_kable <- kable(tabla_formateada, format = "html", escape = FALSE,
                       caption = caption,
                       digits = 3, row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center", font_size = 13, full_width = F) %>%
    row_spec(0, bold = TRUE, background = "#993489", color = "white") %>%
    column_spec(1, bold = TRUE) %>%
    footnote(
      general = "<sub>1</sub> Variable numérica (test de Dunn); <sub>2</sub> Variable categórica (Tukey HSD)",
      general_title = "Nota: ",
      footnote_as_chunk = TRUE,
      escape = FALSE
    )

  return(list(
    tabla = tabla_orig,
    kable = tabla_kable
  ))
}
