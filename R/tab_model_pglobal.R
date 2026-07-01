#' A tab_model_pglobal Function
#'
#' Genera una tabla resumen de un modelo de regresión logística o lineal,
#' mostrando los coeficientes estimados, intervalos de confianza y el
#' p-valor global de cada predictor obtenido mediante \code{car::Anova()}.
#' Las variables con asociación global significativa se resaltan visualmente
#' en la tabla final.
#'
#' @param model objeto modelo ajustado mediante funciones compatibles con
#' \code{broom::tidy()} y \code{car::Anova()}.
#' @param type tipo de modelo. Puede ser \code{"logistic"} para modelos
#' logísticos o \code{"linear"} para modelos lineales. Por defecto
#' \code{"logistic"}.
#'
#' @return Una tabla formateada mediante \code{kableExtra} que incluye:
#' \itemize{
#'   \item Nombre de los predictores.
#'   \item Odds Ratio o estimación del coeficiente.
#'   \item Intervalo de confianza del 95\%.
#'   \item P-valor global de cada variable.
#'   \item Estadísticos globales del modelo.
#' }
#'
#' @details
#' Para modelos logísticos:
#' \itemize{
#'   \item Se muestran Odds Ratios e intervalos de confianza exponenciados.
#'   \item Se añade el número de observaciones.
#'   \item Se añade el coeficiente de determinación de Tjur (\code{R² Tjur}).
#' }
#'
#' Para modelos lineales:
#' \itemize{
#'   \item Se muestran los coeficientes estimados e intervalos de confianza.
#'   \item Se añade el número de observaciones.
#'   \item Se añade una medida de pseudo-\eqn{R^2}.
#' }
#'
#' Los p-valores globales se obtienen mediante \code{car::Anova(type = 3)}.
#' Cuando el p-valor global de una variable es inferior a 0.05, todas las filas
#' correspondientes a dicha variable se resaltan con un color de fondo.
#'
#' @export tab_model_pglobal
#' @import dplyr broom tibble car performance kableExtra
#'
#' @author Alba García Zarzosa
#'
#' @examples
#' ## Modelo logístico
#' data(mtcars)
#' mtcars$am <- factor(mtcars$am)
#'
#' mod_log <- glm(
#'   am ~ wt + hp + factor(cyl),
#'   data = mtcars,
#'   family = binomial()
#' )
#'
#' tab_model_pglobal(
#'   model = mod_log,
#'   type = "logistic"
#' )
#'
#' ## Modelo lineal
#' mod_lin <- lm(
#'   mpg ~ wt + hp + factor(cyl),
#'   data = mtcars
#' )
#'
#' tab_model_pglobal(
#'   model = mod_lin,
#'   type = "linear"
#' )
#'
#' @keywords regression logistic linear anova p-value table summary

tab_model_pglobal<-function(model,type="logistic",caption="Modelo multivariante con P global"){

  # Extract estimates/odds,ratio
  if (type == "logistic") {
    coeff<-tidy(model,exponentiate=T,conf.int=T)

  } else if (type == "linear") {
    coeff<-tidy(model,exponentiate=F,conf.int=T)
  } else {
    stop("type must be 'logistic' or 'linear'")
  }

  # Calculate p global
  # NOTA sobre car::Anova
  # type = 2: ajusta cada término por los demás del modelo, pero excluye las interacciones
  # type = 3: ajusta por todos los términos, incluidas las interacciones

  tab_global <- car::Anova(model, type = 3) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("variable")

  vars <-  tab_global$variable

  coeff <- coeff %>%
    mutate(
      variable = case_when(
        term == "(Intercept)" ~ NA_character_,
        TRUE ~ sapply(term, function(x) {
          vars[which.max(startsWith(x, vars))]
        })
      )
    )

  if (type =="logistic") {
    p_format<-"Pr(>Chisq)"

  }else{
    p_format<-"Pr(>F)"
  }
  # Join p global to coeff table
  tab_final <- coeff %>%
    left_join(
      tab_global %>%
        select(variable, global_p =p_format ),
      by = "variable")%>%
    group_by(variable) %>%
    mutate(
      global_p = ifelse(row_number() == 1,
                        round(global_p, 10),
                        NA)
    ) %>%
    ungroup()

  # Build table
  tab_resumen <- tab_final %>%
    dplyr::mutate(
      Effect = round(estimate, 2),
      CI = paste0(" (", round(conf.low, 2), "–", round(conf.high, 2), ")"),
      `P global` = ifelse(is.na(global_p), "", signif(global_p, 3))
    ) %>%
    dplyr::rename(Predictors = term) %>%
    dplyr::mutate(Effect = as.character(Effect))

  tab_resumen <- tab_resumen %>%
    dplyr::select(Predictors, Effect, CI, `P global`)

  # Rename depending on model type and select variables
  if (type == "logistic") {
    names(tab_resumen)[names(tab_resumen) == "Effect"] <- "Odds Ratio"

  } else if (type == "linear") {
    names(tab_resumen)[names(tab_resumen) == "Effect"] <- "Estimate"
  } else {
    stop("type must be 'logistic' or 'linear'")
  }

  # Add model stats

  stats_row <- if (type == "logistic") {
    tibble(
      Predictors = c("Observations", "R² Tjur"),
      `Odds Ratio` = c(as.character(nobs(model)), round(performance::r2_tjur(model), 3)),
      CI = c("", ""),
      `P global` = c("", "")
    )
  } else {
    tibble(
      Predictors = c("Observations", "R²"),
      Estimate = c(as.character(nobs(model)), round(performance::r2(model)[[1]], 3)),
      # pendiente incluir ajustado
      CI = c("", ""),
      `P global` = c("", "")
    )
  }

  # Ensure column consistency before bind
  tab_resumen <- bind_rows(tab_resumen, stats_row)


  # print(kable_ueb(tab_resumen))

  # Filas donde aparece un p global
  idx_p <- which(tab_resumen$`P global` != "")

  # Filas con p g# Filas con p g# Filas con p global significativo
  pvals <- as.numeric(tab_resumen$`P global`[idx_p])

  idx_sig <- idx_p[!is.na(pvals) & pvals < 0.05]

  # Construir vector de filas a colorear
  rows_to_color <- c()

  for(i in seq_along(idx_sig)){

    start_row <- idx_sig[i]

    # siguiente fila que contiene un p global
    next_p <- idx_p[idx_p > start_row]

    end_row <- if(length(next_p) > 0) {
      min(next_p) - 1
    } else {
      nrow(tab_resumen) - 2  # excluir filas de estadísticas finales
    }

    rows_to_color <- c(rows_to_color, start_row:end_row)
  }


  tabla <- kable_ueb(tab_resumen,caption=caption)

  # Aplicar color a los bloques significativos
  if(length(rows_to_color) > 0){
    tabla <- tabla %>%
      row_spec(
        rows_to_color,
        background = "#ebe0e9"
      )
  }

  # Línea separadora antes de las estadísticas
  tabla <- tabla %>%
    row_spec(
      nrow(tab_resumen)-1,
      extra_css = "border-top: 2px solid black;"
    )

  return(tabla)

}
