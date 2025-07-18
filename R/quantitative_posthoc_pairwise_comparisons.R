#' Comparaciones post-hoc para variables cuantitativas
#'
#' Realiza comparaciones post-hoc entre grupos definidos por una variable categórica con más de dos niveles,
#' aplicando el test de Dunn (no paramétrico) o el test de Tukey HSD (paramétrico), únicamente para variables numéricas.
#'
#' El resultado incluye una tabla con los p-valores de cada comparación y una versión formateada para informes en HTML.
#'
#' @param covariates Vector de nombres de variables numéricas a comparar.
#' @param data Dataframe que contiene las variables.
#' @param group Nombre (carácter) de la variable de agrupación (factor con más de dos niveles).
#' @param method Método de comparación: \code{"no-param"} (por defecto, test de Dunn) o \code{"param"} (test de Tukey HSD).
#' @param caption Título de la tabla formateada (por defecto: "Comparaciones post-hoc").
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{\code{tabla}}{Dataframe con los p-valores sin formato (ancho: variables en filas, comparaciones en columnas).}
#'   \item{\code{kable}}{Tabla HTML formateada con `kableExtra`.}
#' }
#'
#' @details
#' - Las variables deben ser numéricas. Las variables categóricas o no numéricas serán ignoradas.
#' - Los p-valores < 0.05 se muestran resaltados. Valores < 0.001 aparecen como "<0.001".
#'
#' @export quantitative_posthoc_pairwise_comparisons
#' @importFrom dunn.test dunn.test
#' @importFrom Hmisc label
#' @importFrom kableExtra kable kable_styling row_spec column_spec cell_spec
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr bind_rows mutate across
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   grupo = factor(rep(c("A", "B", "C"), each = 10)),
#'   x = rnorm(30),
#'   z = rnorm(30)
#' )
#' quantitative_posthoc_pairwise_comparisons(covariates = c("x", "z"), data = dat, group = "grupo")$kable
#' }
quantitative_posthoc_pairwise_comparisons <- function(covariates, data, group, method = "no-param", caption = "Comparaciones post-hoc") {
  # Verificar si la variable de agrupación es factor con más de 2 niveles
  if (!group %in% names(data)) stop("La variable de agrupación no está en el dataframe.")
  grupo <- data[[group]]
  if (!is.factor(grupo)) grupo <- as.factor(grupo)
  if (nlevels(grupo) <= 2) stop("La variable de agrupación debe tener más de 2 niveles.")

  # Función para ordenar las comparaciones y uniformar el nombre
  ordenar_comparacion <- function(nombre) {
    partes <- unlist(strsplit(nombre, " ?- ?"))
    if (length(partes) != 2) return(nombre)
    partes_ordenadas <- sort(partes)
    paste(partes_ordenadas, collapse = " vs ")
  }

  resultado_lista <- list()
  # tipo_variable <- c()  # Para guardar el tipo

  for (var in covariates) {
    x <- data[[var]]
    df_temp <- data[!is.na(x) & !is.na(grupo), , drop = FALSE]
    x <- df_temp[[var]]
    g <- df_temp[[group]]
    label_var <- Hmisc::label(data[[var]])
    if (is.null(label_var) || label_var == "") label_var <- var

    if (is.numeric(x)) {
      if (method == "param") {
        modelo <- aov(x ~ g)
        tuk <- TukeyHSD(modelo)
        comparaciones <- sapply(rownames(tuk$g), ordenar_comparacion)
        tabla <- data.frame(Comparacion = comparaciones, P = tuk$g[, "p adj"])
        tipo_variable[label_var] <- "<sub>2</sub>"
      } else if (method == "no-param") {
        dt <- dunn.test::dunn.test(as.numeric(x), g, method = "bonferroni", kw = FALSE)
        comparaciones <- sapply(dt$comparisons, ordenar_comparacion)
        tabla <- data.frame(Comparacion = comparaciones, P = dt$P.adjusted)
        # tipo_variable[label_var] <- "<sub>1</sub>"
      } else {
        stop("El argumento 'method' debe ser 'param' o 'no-param'.")
      }
    } else if (is.factor(x)) {
      warning(paste0("La variable ", var, "es un factor y no será analizada. Utiliza la función para factores"))
      # tabla_cont <- table(x, g)
      # test_global <- suppressWarnings(chisq.test(tabla_cont))  # suppressWarnings para cuando hay celdas vacías
      # usar_fisher <- any(test_global$expected < 5) # Decidir si usar Fisher o Chi igual que en summary.quali
      #
      # pares <- combn(levels(g), 2, simplify = FALSE)
      # comparaciones <- sapply(pares, function(par) ordenar_comparacion(paste(par, collapse = "-")))
      # pvals <- c()
      #
      # for (par in pares) {
      #   subdata <- df_temp[g %in% par, , drop = FALSE]
      #
      #   g_sub <- droplevels(subdata[[group]])
      #   x_sub <- table(subdata[[var]], g_sub)
      #
      #   if (usar_fisher) {
      #     test <- fisher.test(x_sub)
      #   } else {
      #     test <- suppressWarnings(chisq.test(x_sub, correct = FALSE))
      #   }
      #   pvals <- c(pvals, test$p.value)
      # }
      #
      # pvals_adj <- p.adjust(pvals, method = "bonferroni")
      # tabla <- data.frame(Comparacion = comparaciones, P = pvals_adj)
      #
      # tipo_variable[label_var] <- if (usar_fisher) "<sub>3</sub>" else "<sub>4</sub>"
    } else {
      warning(paste0("La variable ", var, "no es cuantitativa y no será analizada."))
      next
    }

    tabla$Variable <- label_var
    resultado_lista[[label_var]] <- tabla
  }

  if (length(resultado_lista) == 0) {
    stop("Ninguna variable válida para comparación post-hoc.")
  }

  # Unir todas las tablas
  resultado_df <- bind_rows(resultado_lista)

  # Reemplazar los nombres de variable con superíndices
  # resultado_df$Variable <- paste0(resultado_df$Variable, tipo_variable[resultado_df$Variable])
  caption_final <- paste0(
    caption,
    if (method == "param") " (Tukey HSD test)" else " (Dunn test)"
  )

  # Pivotar
  tabla_pivot <- resultado_df %>%
    pivot_wider(names_from = Comparacion, values_from = P)

  # Guardar versión sin formato por si se quiere analizar aparte
  tabla_orig <- tabla_pivot

  # Formatear los p-valores para presentación
  tabla_formateada <- tabla_pivot %>%
    mutate(across(
      -Variable,
      ~ ifelse(
        is.na(.), NA,
        ifelse(
          . < 0.001,
          cell_spec("<0.001", bold = TRUE, background = "#ebe0e9"),
          ifelse(. < 0.05,
                 cell_spec(sprintf("%.3f", .), bold = TRUE, background = "#ebe0e9"),
                 sprintf("%.3f", .)
          )
        )
      )
    ))

  # nota <- paste0(
  #   "<sub>1</sub> Kruskal-Wallis + Dunn test; \n",
  #   "<sub>2</sub> ANOVA + Tukey HSD; \n",
  #   "<sub>3</sub> Fisher + pairwise comparisons\n",
  #   "<sub>4</sub> Chi² + pairwise comparisons; "
  # )

  # Formato de tabla
  tabla_kable <- kable(tabla_formateada, format = "html", escape = FALSE,
                       caption = caption_final,
                       digits = 3, row.names = FALSE) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), position = "center", font_size = 13, full_width = F) %>%
    row_spec(0, bold = TRUE, background = "#993489", color = "white") %>%
    column_spec(1, bold = TRUE)

  return(list(
    tabla = tabla_orig,
    kable = tabla_kable
  ))
}
