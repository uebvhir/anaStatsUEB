#' @title Tabla de un modelo de regresión con el p-valor global de cada predictor
#'
#' @description
#' Genera una tabla resumen de un modelo de regresión logística o lineal con los
#' coeficientes estimados, los intervalos de confianza al 95 \% y el p-valor global
#' de cada predictor, obtenido con \code{car::Anova()}. Las filas de las variables
#' con asociación global significativa (p < 0.05) se resaltan en la tabla final.
#'
#' @param model Modelo ajustado: un \code{glm} binomial si \code{type = "logistic"}
#'   o un \code{lm} si \code{type = "linear"}. Debe ser compatible con
#'   \code{broom::tidy()} y \code{car::Anova()}.
#' @param type Tipo de modelo: \code{"logistic"} (por defecto) o \code{"linear"}.
#'   Debe corresponder a la clase del modelo; véase \sQuote{Details}.
#' @param caption Título de la tabla (por defecto,
#'   \code{"Modelo multivariante con P global"}).
#'
#' @return Un objeto \code{kableExtra} (creado con \code{\link{kable_ueb}}) con las
#' columnas \code{Predictors}, \code{Odds Ratio} (logístico) o \code{Estimate}
#' (lineal), \code{CI} (intervalo de confianza al 95 \%, con los valores
#' redondeados a 2 decimales) y \code{P global}. Se añaden dos filas finales de
#' estadísticos: \code{Observations} y \code{R² Tjur} (logístico) o \code{R²}
#' (lineal).
#'
#' @details
#' \itemize{
#'   \item Modelos logísticos: se muestran las odds ratio y sus intervalos de
#'     confianza exponenciados, el número de observaciones y el R² de Tjur
#'     (\code{performance::r2_tjur()}).
#'   \item Modelos lineales: se muestran los coeficientes y sus intervalos de
#'     confianza, el número de observaciones y el R² (\code{performance::r2()}, sin
#'     ajustar).
#'   \item Los p-valores globales se obtienen con \code{car::Anova(type = 3)}: test
#'     F para \code{lm} y test de razón de verosimilitudes para \code{glm}. Se
#'     muestran redondeados a 3 cifras significativas, solo en la primera fila de
#'     cada variable.
#'   \item Si el p-valor global de una variable es inferior a 0.05, se resaltan con
#'     un color de fondo todas las filas de esa variable. Una línea separa las
#'     filas de estadísticos del resto de la tabla.
#'   \item Nota: el p global se asigna a cada término mediante \code{startsWith()},
#'     por lo que los términos de interacción (por ejemplo, \code{wt:hp}) y las
#'     variables cuyo nombre empieza igual que otra (\code{hp} y \code{hp2}) no
#'     muestran su propio p global. \code{type} debe coincidir con la clase del
#'     modelo; en caso contrario se produce un error poco claro. Los valores muy
#'     grandes o muy pequeños se muestran sin formato especial.
#' }
#'
#' @seealso \code{\link{kable_ueb}}, \code{\link[car]{Anova}},
#'   \code{\link[broom]{tidy}}, \code{\link[performance]{r2_tjur}},
#'   \code{\link[performance]{r2}}, \code{\link{tab_model_segmented}}
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(x1 = rnorm(100),
#'                   grupo = factor(sample(c("A", "B", "C"), 100, replace = TRUE)))
#' dat$y <- factor(rbinom(100, 1, plogis(1.5 * dat$x1)))
#'
#' # Modelo logístico
#' mod_log <- glm(y ~ x1 + grupo, data = dat, family = binomial())
#' tab_model_pglobal(model = mod_log, type = "logistic")
#'
#' # Modelo lineal
#' mod_lin <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
#' tab_model_pglobal(model = mod_lin, type = "linear", caption = "Modelo lineal")
#'
#' @keywords regression logistic linear anova p-value table summary
#'
#' @importFrom broom tidy
#' @importFrom dplyr %>% mutate case_when left_join select group_by ungroup
#' @importFrom dplyr row_number rename bind_rows
#' @importFrom tibble tibble
#' @importFrom kableExtra row_spec
#' @importFrom stats nobs
#'
#' @author
#' Alba García Zarzosa
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
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
