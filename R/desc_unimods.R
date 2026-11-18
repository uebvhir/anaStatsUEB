#' @title Descripción de modelos univariables (logísticos o lineales)
#'
#' @description
#' Ajusta un modelo para cada variable explicativa de \code{var2test} frente a la
#' variable dependiente \code{y}. Opcionalmente, cada modelo se ajusta por un
#' conjunto común de covariables (\code{adjust_vars}) y puede incluir un efecto
#' aleatorio (\code{random_effect}). Admite modelos lineales y logísticos.
#'
#' Devuelve los resultados numéricos, una tabla HTML formateada y los modelos
#' ajustados. Se resaltan las filas de las variables con al menos un p-valor
#' significativo y, de forma opcional, se añade el p-valor global de las
#' variables categóricas (solo para modelos sin efectos aleatorios).
#'
#' @param y Cadena de texto con el nombre de la variable dependiente en
#'   \code{data}. Para \code{type = "logistic"} debe ser un factor de dos niveles.
#' @param var2test Vector de caracteres con los nombres de las variables
#'   explicativas a evaluar de forma univariable.
#' @param data Data frame con las variables del estudio.
#' @param type Cadena de texto con el tipo de modelo: \code{"logistic"} para
#'   regresión logística binaria o \code{"linear"} para regresión lineal. Es
#'   obligatorio: aunque su valor por defecto es \code{NULL}, la función requiere
#'   que se indique.
#' @param adjust_vars Vector de caracteres o \code{NULL}. Variables de ajuste
#'   incluidas en todos los modelos además de cada variable de \code{var2test}.
#'   Por defecto, \code{NULL}, es decir, modelos estrictamente univariables.
#' @param size Valor numérico con el tamaño de fuente de la tabla HTML. Por
#'   defecto, 8.5.
#' @param format Cadena de texto con el formato de salida de la tabla. Por
#'   defecto, \code{"html"}. Actualmente la función no lo utiliza.
#' @param caption Cadena de texto con el título de la tabla. Si es \code{NULL},
#'   se genera automáticamente.
#' @param show.n Lógico. Reservado para mostrar el número de observaciones.
#'   Actualmente la función no lo utiliza.
#' @param show.p.global Lógico. Si es \code{TRUE}, se calcula y muestra el
#'   p-valor global de cada variable explicativa (solo sin efectos aleatorios).
#'   Por defecto, \code{FALSE}.
#' @param group Lógico. Si es \code{TRUE} (por defecto), las filas de la tabla
#'   se agrupan por variable explicativa.
#' @param random_effect Cadena de texto o \code{NULL}. Nombre de la variable
#'   usada como efecto aleatorio (intercepto aleatorio) en un modelo mixto. Si
#'   es \code{NULL} (por defecto), se ajusta un modelo clásico.
#'
#' @return Lista con tres elementos:
#' \describe{
#'   \item{unimod_ci_df}{Data frame con los resultados de los modelos
#'     univariables: etiqueta de la variable (\code{vars_label}), nivel
#'     (\code{levs}), estimador (por ejemplo \code{Odds Ratio} en modelos
#'     logísticos), límites del intervalo de confianza, p-valor y \code{N}. Si
#'     \code{show.p.global = TRUE} incluye además \code{p.global}.}
#'   \item{xtab}{Objeto \code{kableExtra} con la tabla HTML formateada.}
#'   \item{mods}{Lista con nombre con los modelos ajustados para cada variable
#'     de \code{var2test}.}
#' }
#'
#' @details
#' \itemize{
#'   \item Si \code{adjust_vars} no es \code{NULL}, cada modelo incluye la
#'     variable de interés y todas las covariables de ajuste.
#'   \item Con \code{show.p.global = TRUE} y \code{type = "logistic"}, el
#'     p-valor global se obtiene comparando el modelo completo con un modelo
#'     reducido (sin covariables, o solo con \code{adjust_vars}) mediante un test
#'     de razón de verosimilitudes. Con \code{type = "linear"} se toma el
#'     p-valor F de la primera fila de \code{anova()} del modelo.
#'   \item Si \code{random_effect} no es \code{NULL} y \code{show.p.global =
#'     TRUE}, la función produce un error, porque el p-valor global no está
#'     implementado para modelos mixtos.
#'   \item El p-valor global se añade solo a la primera fila de cada variable
#'     para evitar redundancia en variables con varios niveles.
#'   \item Se resaltan todas las filas de las variables con al menos un nivel
#'     con p-valor menor o igual que 0.05 (umbral fijo).
#'   \item Los modelos se ajustan con \code{glm} o \code{lm}, o con
#'     \code{glmer} o \code{lmer} (paquete \pkg{lme4}) si se indica
#'     \code{random_effect}.
#' }
#'
#' @seealso \code{\link{desc_mod}}, \code{\link{kable_ueb}},
#'   \code{\link{desc_unimods_multi}}, \code{\link{desc_multimod}},
#'   \code{\link{plot_forest_unimods}}
#'
#' @examples
#' iris$Species_bin <- factor(ifelse(iris$Species == "setosa", "setosa", "others"))
#'
#' # Modelos logisticos univariables
#' res <- desc_unimods(
#'   y = "Species_bin",
#'   var2test = c("Sepal.Length", "Petal.Width"),
#'   data = iris,
#'   type = "logistic"
#' )
#' res$xtab
#'
#' # Modelo logistico ajustado por covariables
#' res_adj <- desc_unimods(
#'   y = "Species_bin",
#'   var2test = "Petal.Width",
#'   adjust_vars = c("Sepal.Length", "Sepal.Width"),
#'   data = iris,
#'   type = "logistic"
#' )
#'
#' \dontrun{
#' # Modelo lineal mixto con efecto aleatorio (grupo ficticio)
#' set.seed(1)
#' iris$Group <- factor(sample(letters[1:5], nrow(iris), replace = TRUE))
#' res_mix <- desc_unimods(
#'   y = "Sepal.Length",
#'   var2test = "Petal.Width",
#'   adjust_vars = "Sepal.Width",
#'   data = iris,
#'   type = "linear",
#'   random_effect = "Group"
#' )
#' }
#'
#' @keywords regression logistic linear univariate mixed-model
#'
#' @importFrom stats glm lm anova
#' @importFrom lme4 glmer lmer
#' @importFrom Hmisc label
#' @importFrom dplyr %>%
#' @import kableExtra
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
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
