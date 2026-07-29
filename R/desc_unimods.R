#' Descripción de modelos univariados (regresión logística o lineal), con soporte para efectos aleatorios
#'
#' Esta función ajusta modelos univariados para una variable dependiente 'y' y un conjunto
#' de variables explicativas `var2test`. Opcionalmente, cada modelo puede ajustarse por un
#' conjunto de covariables especificadas mediante `adjust_vars`. Puede trabajar con modelos
#' lineales, logísticos y modelos mixtos con efectos aleatorios.
#'
#' Incluye la opción de calcular p-valores globales para variables categóricas (solo para modelos sin efectos aleatorios),
#' generar tablas con resultados formateados en HTML y remarcar variables con p-valores significativos.
#'
#' @param y Character. Nombre de la variable dependiente en data.
#' @param var2test Character vector. Nombres de las variables explicativas a testear univariadamente.
#' @param data Data.frame. Conjunto de datos con las variables.
#' @param type Character. Tipo de modelo: "logistic" para regresión logística binaria, "linear" para regresión lineal.
#' @param adjust_vars Character vector o `NULL`. Variables de ajuste que se incluirán en todos los modelos además de cada variable de `var2test`. Por defecto `NULL`, en cuyo caso se ajustan modelos estrictamente univariados.
#' @param size Numeric. Tamaño de fuente para la tabla HTML generada (por defecto 8.5).
#' @param format Character. Formato de salida para la tabla (por defecto "html").
#' @param caption Character. Texto para el título (caption) de la tabla. Si 'NULL', se genera automáticamente.
#' @param show.n Logical. Indica si mostrar el número de observaciones (no implementado en la función actual).
#' @param show.p.global Logical. Indica si se calcula y muestra el p-valor global para variables categóricas (solo si no hay efectos aleatorios).
#' @param group Logical. Si `TRUE`, agrupa las filas de la tabla por variable explicativa.
#' @param random_effect Character or NULL. Nombre de la variable para el efecto aleatorio (si se desea un modelo mixto). Si 'NULL', se ajusta modelo clásico.
#'
#' @details
#' - Si `adjust_vars` no es `NULL`, cada modelo incluye la variable de interés y todas las covariables de ajuste especificadas.
#' - Cuando `show.p.global = TRUE` y existen variables de ajuste, el p-valor global de la variable de interés se obtiene comparando el modelo completo frente a un modelo reducido que contiene únicamente las covariables de ajuste (test de razón de verosimilitudes para modelos logísticos).
#' - Si `random_effect` no es `NULL` y `show.p.global = TRUE`, la función produce un error porque el p-valor global no está implementado para modelos mixtos.
#' - El p-valor global se añade solo a la primera fila de cada variable para evitar redundancia en variables con múltiples niveles.
#' - Se remarcan todas las filas de las variables que tengan al menos un nivel con p-valor significativo (≤ 0.05).
#' - Se soportan modelos ajustados con `glm`, `lm`, `glmer` o `lmer` según los parámetros especificados.
#'
#' @return Lista con tres elementos:
#' \describe{
#'   \item{unimod_ci_df}{Data.frame con los resultados de los modelos univariados (estimates, IC, p-valores, etc.).}
#'   \item{xtab}{Objeto 'kableExtra' con la tabla HTML formateada.}
#'   \item{mods}{Lista con los modelos ajustados para cada variable en 'var2test'.}
#' }
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' data(iris)
#'
#' iris$Species_bin <- ifelse(iris$Species == "setosa", "setosa", "others")
#' iris$Species_bin <- factor(iris$Species_bin)
#'
#' # Modelo logístico simple
#' res <- desc_unimods(
#'   y = "Species_bin",
#'   var2test = c("Sepal.Length", "Petal.Width"),
#'   data = iris,
#'   type = "logistic"
#' )
#'
#' # Modelo logístico ajustado por covariables
#' res_adj <- desc_unimods(
#'   y = "Species_bin",
#'   var2test = c("Petal.Width"),
#'   adjust_vars = c("Sepal.Length", "Sepal.Width"),
#'   data = iris,
#'   type = "logistic"
#' )
#'
#' # Modelo lineal mixto con efecto aleatorio
#' # Suponiendo que "Group" es el efecto aleatorio
#' # res_mix <- desc_unimods(
#' #   y = "Sepal.Length",
#' #   var2test = c("Petal.Width"),
#' #   adjust_vars = c("Sepal.Width"),
#' #   data = iris,
#' #   type = "linear",
#' #   random_effect = "Group"
#' # )
#' }
#'
#' @importFrom stats glm lm anova
#' @importFrom lme4 glmer lmer
#' @importFrom Hmisc label
#' @importFrom dplyr %>%
#' @import kableExtra
#'
#' @export
desc_unimods <- function(y, var2test, data, type = NULL,
                         adjust_vars = NULL,
                         size = 8.5,
                         format = "html",
                         caption = NULL,
                         show.n = TRUE,
                         show.p.global = FALSE,
                         group = TRUE,
                         random_effect = NULL){
  if (!is.factor(data[[y]]) & type == "logistic" ) {
    stop("variable 'y' must be factor")
  }

  if (is.null(type)) stop("model 'type' is needed ")
  if (length(levels(data[[y]])) != 2 & type == "logistic") stop("variable 'y' must have two levels")
  if (is.null(caption)) {
    caption <- paste0(
      "Univariate ", type, " regression (", get_lab_nam(data,y), ").",
      if (type %in% c("logistic", "multinomial"))
        paste("Reference level:", levels(data[[y]])[1])
      else ""
    )
  }
  if (!is.null(random_effect) && show.p.global) {
    stop("Cálculo de p-valor global no está implementado para modelos mixtos (con efectos aleatorios).")
  }

  data_or <- data # Guardamos data original porque se modifica en el p-global
  unimod_df <- NULL
  mod <- list()
  global_pvals <- c()

  for (i in seq_along(var2test)) {
    data <- data_or # cada iteración con los datos originales

    # Por si hay variables de ajuste
    rhs <- c(var2test[i], adjust_vars)
    rhs <- rhs[!is.na(rhs)]
    rhs <- unique(rhs)

    # Construir fórmula con o sin efecto aleatorio
    if (is.null(random_effect)) {
      frml <- as.formula(
        paste0(y, " ~ ", paste(rhs, collapse = " + "))
      )
    } else {
      frml <- as.formula(
        paste0(
          y,
          " ~ ",
          paste(rhs, collapse = " + "),
          " + (1 | ",
          random_effect,
          ")"
        )
      )
    }

    mod[[var2test[i]]] <- switch(type,
                                 "logistic" = if (is.null(random_effect)) {
                                   glm(frml, data = data, family = "binomial")
                                 } else {
                                   glmer(frml, data = data, family = "binomial")
                                 },
                                 "linear" = if (is.null(random_effect)) {
                                   lm(frml, data = data)
                                 } else {
                                   lmer(frml, data = data)
                                 },
                                 stop("Unsupported model type")
    )

    if (show.p.global){

      vars <- all.vars(frml)
      data <- data[complete.cases(data[, vars]), ]
      var_label <- get_lab_nam(data[[var2test[i]]], var2test[i])

      # Calcular p-valor global
      if(type == "linear"){
        global_pvals[[var_label]] <- anova(mod[[var2test[i]]])$`Pr(>F)`[1]
      } else if(type == "logistic"){

        if(is.null(adjust_vars)){ # si es univariado, comparamos con el modelo sin covars
          mod_null <- glm(
            as.formula(paste0(y, " ~ 1")),
            data = data,
            family = "binomial"
          )
        } else { # Si hay adjust_vars, el pvalor global correcto es el comparado con el modelo sin la var2test pero con las covars de ajuste
          mod_null <- glm(
            as.formula(
              paste0(
                y,
                " ~ ",
                paste(adjust_vars, collapse = " + ")
              )
            ),
            data = data,
            family = "binomial"
          )
        }

        global_pvals[[var_label]] <- anova(mod_null, mod[[var2test[i]]], test = "LRT")$`Pr(>Chi)`[2]
      }

    }

    # Aqui nos quedamos solo con el resultado de la var2test, por si hay vars de ajuste
    tmp <- desc_mod(mod[[var2test[i]]], show.pretty = TRUE)
    tmp <- tmp[tmp$vars == get_lab_nam(data,var2test[i]), ]
    unimod_df <- rbind(unimod_df, tmp)

  }

  # Si mostramos show.p.global, añadimos la columna
  if (show.p.global) {
    unimod_df$p.global <- NA  # Inicializamos la columna

    for (var in names(global_pvals)) {
      idx_var <- which(unimod_df$vars_label == var)
      if (length(idx_var) > 0) {
        first_idx <- idx_var[1]
        unimod_df$p.global[first_idx] <- paste0(formatC(global_pvals[[var]], digits = 3, format = "f"), "<sub>1</sub>")
      }
    }
  }

  xtab <- kable_ueb(unimod_df[, !names(unimod_df) %in% "vars_label"], row.names = F, digits = 3,font_size = size,
                    caption = caption)

  if (group) {
    xtab <- xtab %>% kableExtra::group_rows(index = table(unimod_df$vars_label)[unique(as.character(unimod_df$vars_label))])
  }

  # Remarcar filas con p-valor significativo en color
  if(type == "linear"){
    sig_rows <- which(unimod_df$`Pr(>|t|)` <= 0.05)
  } else if (type == "logistic"){
    sig_rows <- which(unimod_df$`Pr(>|z|)` <= 0.05)
  } else{
    sig_rows <- c(integer(0))
  }
  if (length(sig_rows) > 0) {
    # Variables con algún nivel significativo
    vars_sig <- unique(unimod_df$vars_label[sig_rows])

    # Todas las filas que correspondan a esas variables
    rows_to_paint <- which(unimod_df$vars_label %in% vars_sig)

    for (r in rows_to_paint) {
      xtab <- xtab %>% kableExtra::row_spec(r, background = "#ebe0e9")
    }
  }

  if (show.p.global) {
    xtab <- xtab %>%
      kableExtra::footnote(
        general = "1: Overall p-value",
        general_title = "",
        escape = FALSE
      )
  }

  return(list( unimod_ci_df = unimod_df, xtab = xtab, mods = mod) )
}
