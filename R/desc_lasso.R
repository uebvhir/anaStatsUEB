#' @title Regularización Lasso: gráfico de la trayectoria y tabla de coeficientes para modelos binomiales y multinomiales
#'
#' @description
#' Ajusta de forma automática un modelo de regresión penalizada Lasso (L1) con
#' validación cruzada mediante el paquete \code{glmnet}. Se adapta a respuestas
#' binomiales y multinomiales, y genera el gráfico de la trayectoria de regularización
#' y una tabla HTML con estilo \code{kableExtra}. El título de la tabla muestra
#' dinámicamente la N inicial, la N final analizada, el porcentaje retenido y el
#' desglose por grupos (n y \%).
#'
#' @param var_out Cadena de caracteres. Nombre de la variable respuesta categórica (Y).
#' @param var_comp Vector de caracteres. Nombres de las variables predictoras (X).
#' @param data Data frame que contiene todas las variables especificadas.
#' @param na_perc Lógico. Si \code{TRUE}, descarta las variables predictoras con un
#'   porcentaje elevado de valores perdidos. El umbral se define con
#'   \code{complete_obs}. Por defecto \code{FALSE}.
#' @param complete_obs Numérico. Porcentaje mínimo de datos completos para conservar una
#'   variable; internamente se pasa a \code{NAperc()} como \code{100 - complete_obs}.
#'   Por defecto 90.
#' @param frml Fórmula opcional. Solo se utiliza la parte derecha para definir los
#'   predictores (\code{var_comp}); la parte izquierda no se lee, por lo que
#'   \code{var_out} debe indicarse igualmente. Por defecto \code{NULL}.
#' @param lambda_fixed Numérico. Valor de \eqn{\lambda} fijado manualmente. Si se indica,
#'   sustituye al criterio de \code{lambda_criterion}. Por defecto \code{NULL}.
#' @param lambda_criterion Cadena de caracteres. Criterio de selección de \eqn{\lambda}:
#'   \code{"lambda.min"} o \code{"lambda.1se"}. Por defecto \code{"lambda.min"}.
#' @param col.background Cadena de caracteres. Color hexadecimal del fondo de la cabecera
#'   de la tabla. Por defecto \code{"#993489"}.
#' @param font_size Numérico. Tamaño de letra de la tabla HTML. Por defecto 12.
#' @param show.plot Lógico. Si \code{TRUE}, muestra el gráfico de la trayectoria de
#'   regularización. Por defecto \code{TRUE}.
#' @param plot_type Cadena de caracteres. Métrica de pérdida de la validación cruzada
#'   (\code{type.measure} de \code{cv.glmnet}): \code{"deviance"}, \code{"class"},
#'   \code{"auc"} o \code{"mae"}. Por defecto \code{"deviance"}.
#' @param nresponse Numérico o carácter. En modelos multinomiales, categoría de respuesta
#'   cuyas trayectorias se representan. Por defecto 1.
#'
#' @return Una lista con:
#' \describe{
#'   \item{table}{Objeto \code{kableExtra} con la tabla HTML de coeficientes y estado de selección de las variables.}
#'   \item{plot}{Gráfico de la trayectoria de regularización (capturado con \code{recordPlot()}).}
#'   \item{lambda}{Valor numérico de \eqn{\lambda} utilizado para la selección.}
#'   \item{model}{Modelo \code{glmnet} ajustado.}
#'   \item{cv_model}{Objeto de validación cruzada de clase \code{cv.glmnet}.}
#'   \item{outVar}{Variables descartadas por exceso de valores perdidos (si \code{na_perc = TRUE}).}
#'   \item{selVar}{Variables seleccionadas por el modelo.}
#' }
#'
#' @details
#' El flujo de trabajo interno es el siguiente:
#' \enumerate{
#'   \item \strong{Filtrado de datos:} selecciona solo los casos completos (sin \code{NA})
#'   en las variables implicadas, requisito imprescindible de \code{glmnet}.
#'   \item \strong{Familia del modelo:} evalúa el número de niveles de la variable
#'   respuesta. Con 2 niveles ajusta un modelo \code{"binomial"}; con más de 2, uno
#'   \code{"multinomial"}.
#'   \item \strong{Matriz de diseño:} construye la matriz \code{X} con \code{model.matrix},
#'   que convierte automáticamente los factores en variables ficticias (dummies), y
#'   elimina manualmente la columna del intercepto.
#'   \item \strong{Validación cruzada:} usa una semilla fija (\code{set.seed(123)}) para
#'   evaluar el error (la métrica indicada en \code{plot_type}) para cada valor de
#'   penalización \eqn{\lambda}.
#'   \item \strong{Gráfico:} dibuja la trayectoria de regularización con el paquete
#'   \code{plotmo} y marca el \eqn{\log(\lambda)} seleccionado con una línea vertical
#'   discontinua.
#'   \item \strong{Extracción de coeficientes:} extrae los coeficientes para el
#'   \eqn{\lambda} seleccionado. En el caso multinomial, una variable se considera
#'   seleccionada si su coeficiente es distinto de cero en \emph{alguna} de las categorías
#'   de la respuesta.
#' }
#'
#' @seealso \code{\link{desc_plot_lasso}}, \code{\link{extract_names}}, \code{\link{NAperc}}
#'
#' @importFrom glmnet cv.glmnet glmnet
#' @importFrom plotmo plot_glmnet
#' @importFrom dplyr select filter mutate arrange bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom kableExtra kable kable_styling row_spec column_spec
#'
#' @examples
#' \dontrun{
#' # Ejemplo 1: modelo binomial (respuesta con 2 niveles)
#' bin_data <- mtcars
#' bin_data$vs <- as.factor(bin_data$vs) # Respuesta binomial
#'
#' bin_res <- desc_lasso(
#'   var_out = "vs",
#'   var_comp = c("mpg", "disp", "hp", "drat", "wt", "qsec"),
#'   data = bin_data,
#'   lambda_criterion = "lambda.min"
#' )
#' bin_res$table # Tabla HTML
#'
#' # Ejemplo 2: modelo multinomial (respuesta con 3 niveles)
#' multi_data <- iris
#' # "Species" tiene 3 niveles: setosa, versicolor, virginica
#'
#' multi_res <- desc_lasso(
#'   var_out = "Species",
#'   var_comp = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'   data = multi_data,
#'   lambda_criterion = "lambda.1se", # Modelo más restrictivo y parsimonioso
#'   nresponse = 2 # Dibuja las curvas de la segunda categoría (versicolor)
#' )
#' multi_res$table
#' }
#'
#' @keywords lasso glmnet regularization variable-selection
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
desc_lasso <- function(
    var_out = NULL,
    var_comp = NULL,
    data,
    na_perc = FALSE,
    complete_obs = 90,
    frml = NULL,
    lambda_fixed = NULL,
    lambda_criterion = "lambda.min",
    col.background = "#993489",
    font_size = 12,
    show.plot = TRUE,
    plot_type = "deviance",
    nresponse = 1)
{
### --- 0. Management of formula versus variables ---
  if (!is.null(frml)) {

    var_comp <- formula.tools::rhs.vars(frml)

  }

### --- 0. NA cleaning ---
### The maximum of NAperc is the inverse of percentatge of NA equal to 100.
  vars_out <- character(0)

if (isTRUE(na_perc)) {

    max_na_threshold <- 100 - complete_obs

    # 1. Limitem l'anàlisi de NAs NOMÉS a les variables predictores (var_comp)
    # Així evitem que ens tregui varsOut que ni tan sols anaven a entrar al model
    data_for_na <- data[, colnames(data) %in% var_comp, drop = FALSE]

    res_na <- NAperc(
      df = data_for_na,
      maxNA = max_na_threshold)

    vars_out <- res_na[['var']]

    # 2. Actualitzem var_comp traient les descartades
    var_comp <- setdiff(var_comp, vars_out)

  }

### --- 1. Sample N calculation and data cleanning ---
  n_inicial <- nrow(data)

  vars_all <- c(var_out, var_comp)
  df_clean <- data[, colnames(data) %in% vars_all, drop = FALSE]
  df_clean <- na.omit(df_clean)

  n_final   <- nrow(df_clean)
  pct_final <- round((n_final / n_inicial) * 100, 1)

### --- 2. Group distribution for the outcome variable 'var_out' (final N) ---
  y_counts <- table(df_clean[[var_out]])
  y_levels <- names(y_counts)

  dist_parts <- sapply(y_levels, function(lvl) {
    n_lvl <- y_counts[[lvl]]
    pct_lvl <- round((n_lvl / n_final) * 100, 2)
    paste0("<b>", lvl, "</b>: ", n_lvl, " (", pct_lvl, "%)")
  })

  dist_text <- paste(dist_parts, collapse = " | ")

### --- 3. Configuration of the outcome variable 'var_out' ---
### Choose between 'binomial' or 'multinomial' outcome
  y_vec <- df_clean[[var_out]]

  if (!is.factor(y_vec)) y_vec <- as.factor(y_vec)

    n_levels <- nlevels(y_vec)

  if (n_levels < 2) {

    stop("The response variable must have at least 2 levels.")

  } else if (n_levels == 2) {

    family_type <- "binomial"

  } else {

    family_type <- "multinomial"

  }

### --- 4. Matrix of the independent variable ---
  formula_x <- as.formula(paste(' ~' , paste(var_comp, collapse = " + ")))
  x_matrix  <- model.matrix(formula_x, data = df_clean)

  if ("(Intercept)" %in% colnames(x_matrix)) {

    x_matrix <- x_matrix[, colnames(x_matrix) != "(Intercept)", drop = FALSE]

  }

### --- 4b. Rename columns and labels for the LASSO plot ---
  colnames_new <- sapply(colnames(x_matrix), function(col_name) {
    sorted_vars <- var_comp[order(nchar(var_comp), decreasing = TRUE)]
    matched_var <- NULL

    for (v in sorted_vars) {
      if (startsWith(col_name, v)) {
        matched_var <- v
        break
      }
    }

    if (is.null(matched_var)) return(col_name)

    var_label <- tryCatch({
      lbl <- Hmisc::label(df_clean[[matched_var]])
      if (is.null(lbl) || lbl == "") matched_var else as.character(lbl)

    }, error = function(e) matched_var)

    level_part <- sub(paste0("^", matched_var), "", col_name)

    if (level_part == "") {

      return(var_label)

    } else {

      return(paste0(var_label, ": ", level_part))

    }
  })

  colnames(x_matrix) <- colnames_new

### --- 5. Fit and cross validation model ---
  set.seed(123)

  fit <- glmnet::glmnet(
    x_matrix,
    y_vec,
    family = family_type,
    alpha = 1)

  cv_fit <- glmnet::cv.glmnet(
    x_matrix,
    y_vec,
    family = family_type,
    alpha = 1,
    type.measure = plot_type)

### --- 6. Lambda value selection ---
  if (!is.null(lambda_fixed) && is.numeric(lambda_fixed)) {

    best_lambda <- lambda_fixed
    lambda_label <- "fixed"

  } else {

    best_lambda <- cv_fit[[lambda_criterion]]
    lambda_label <- lambda_criterion

  }

### --- 7. LASSO Plot ---
if(show.plot) {
  tryCatch({ grDevices::dev.control("enable") }, error = function(e) NULL)

  if (!requireNamespace("plotmo", quietly = TRUE)) {

    message("The 'plotmo' package is not installed. Using base glmnet plot.")

    plot(
      fit,
      xvar = "lambda",
      label = TRUE)

  } else {

    if (family_type == "multinomial") {
      plotmo::plot_glmnet(
        fit,
        xvar = "lambda",
        label = TRUE,
        s = best_lambda,
        nresponse = nresponse,
        cex = 1.2)

    } else {
      plotmo::plot_glmnet(
        fit,
        xvar = "lambda",
        label = TRUE,
        s = best_lambda,
        cex = 1.2)

    }
  }

  abline(v = log(best_lambda), col = "#993489", lty = 2, lwd = 1)
  plot_captured <- tryCatch({ grDevices::recordPlot() }, error = function(e) NULL)
}

### --- 8. Wraped function to process names()
  parse_names <- function(names_vec) {

    has_colon <- grepl(": ", names_vec, fixed = TRUE)
    var_part <- ifelse(has_colon, sub(": .*", "", names_vec), names_vec)
    level_part <- ifelse(has_colon, sub("^.*?: ", "", names_vec), "")

    data.frame(
      Variable = var_part,
      Nivel = level_part,
      stringsAsFactors = FALSE)
  }

### --- 9. Coeficients extraction ---
  coefs <- coef(cv_fit, s = best_lambda)

  if (family_type == "binomial") {

    coef_mat <- as.matrix(coefs)
    parsed <- parse_names(rownames(coef_mat))

    df_coefs <- data.frame(
        Variable_Raw = parsed$Variable,
        Variable = parsed$Variable,
        Level = parsed$Nivel,
        Coefficient = round(coef_mat[, 1], 4),
        stringsAsFactors = FALSE) |>
      dplyr::filter(Variable_Raw != "(Intercept)") |>
      dplyr::mutate(Selected = ifelse(Coefficient == 0, "No", "Si"))

    active_vars <- df_coefs |>
      dplyr::group_by(Variable_Raw) |>
      dplyr::summarise(Any_Active = any(Selected == "Si"), .groups = "drop")

    df_coefs <- df_coefs |> dplyr::left_join(active_vars, by = "Variable_Raw")

    labels_order <- sapply(var_comp, function(v) {

      lbl <- Hmisc::label(data[[v]])
      if (is.null(lbl) || lbl == "") v else as.character(lbl)

    })

    ### Extract varsSel
      vars_sel_labels <- df_coefs |>
        dplyr::filter(Selected == 'Si') |>
        dplyr::pull(Variable_Raw) |>
        unique()

    df_coefs$Var_Order <- match(df_coefs$Variable_Raw, labels_order)
    df_coefs <- df_coefs |> dplyr::arrange(desc(Any_Active), Var_Order, Level)

    sig_rows <- which(df_coefs$Selected == "Si")
    df_coefs$Variable[duplicated(df_coefs$Variable_Raw)] <- ""
    df_tabla <- df_coefs |> dplyr::select(Variable, Level, Coefficient, Selected)

  } else {

    list_df <- lapply(names(coefs), function(lvl) {
      c_mat <- as.matrix(coefs[[lvl]])
      parsed <- parse_names(rownames(c_mat))
      data.frame(
        Variable_Raw = parsed$Variable,
        Variable = parsed$Variable,
        Level = parsed$Nivel,
        Coef = round(c_mat[, 1], 4),
        Nivel_Out = lvl,
        stringsAsFactors = FALSE)

    })

    df_all_mult <- dplyr::bind_rows(list_df) |> dplyr::filter(Variable_Raw != "(Intercept)")
    df_tabla <- df_all_mult |> tidyr::pivot_wider(names_from = Nivel_Out, values_from = Coef)

    v_columns <- setdiff(names(df_tabla), c("Variable_Raw", "Variable", "Level"))

    df_tabla$Selected <- apply(df_tabla[v_columns], 1, function(row) {

      if (any(row != 0)) "Si" else "No"

    })

    active_vars <- df_tabla |>
      dplyr::group_by(Variable_Raw) |>
      dplyr::summarise(Any_Active = any(Selected == "Si"), .groups = "drop")

    df_tabla <- df_tabla |> dplyr::left_join(active_vars, by = "Variable_Raw")

    labels_order <- sapply(var_comp, function(v) {

      lbl <- Hmisc::label(data[[v]])
      if (is.null(lbl) || lbl == "") v else as.character(lbl)

    })

    ### Extract varsSel
      vars_sel_labels <- df_tabla |>
        dplyr::filter(Selected == 'Si') |>
        dplyr::pull(Variable_Raw) |>
        unique()

    df_tabla$Var_Order <- match(df_tabla$Variable_Raw, labels_order)
    df_tabla <- df_tabla |> dplyr::arrange(desc(Any_Active), Var_Order, Level)

    sig_rows <- which(df_tabla$Selected == "Si")
    df_tabla$Variable[duplicated(df_tabla$Variable_Raw)] <- ""
    df_tabla <- df_tabla |> dplyr::select(-Variable_Raw, -Any_Active, -Var_Order)

  }

### --- 10. Dinamic caption with methodological info ---
  full_caption <- paste0(
    "<b>Coeficients LASSO</b> (Family: ", family_type, " | Lambda: ", round(best_lambda, 4), ")<br>",
    "<span style='font-size: 0.9em; font-weight: normal; color: #333333;'>",
    "N inicial = ", n_inicial, " | N final analitzada = ", n_final, " (", pct_final, "% de la mostra inicial)<br>",
    "<b>Distribució de grups analitzats:</b> ", dist_text,
    "</span>")

### --- 11. Table cration with 'html' (kableExtra)
  taula_html <- df_tabla |>
    kableExtra::kable(
      escape = FALSE,
      row.names = FALSE,
      align = "c",
      caption = full_caption,
      format = "html") |>
    kableExtra::kable_styling(
      latex_options = c("striped", "hold_position"),
      font_size = font_size,
      full_width = FALSE) |>
    kableExtra::row_spec(0, background = col.background, color = "white", bold = TRUE) |>
    kableExtra::column_spec(1, bold = TRUE) |>
    kableExtra::column_spec(2, italic = TRUE)

  if (length(sig_rows) > 0) {

    taula_html <- taula_html |>
      kableExtra::row_spec(sig_rows, background = "#ebe0e9")

  }

  taula_html <- taula_html |>
    kableExtra::footnote(
      general = paste0("Note: The penalization threshold (lambda) used is ", round(best_lambda, 4), " (", lambda_label, "). Shaded rows indicate coefficients selected by the model."),
      general_title = "",
      escape = FALSE)

### --- 12. Return name of columns ---
  diccionari_nom_col <- setNames(var_comp, sapply(var_comp, function(v) {

    lbl <- Hmisc::label(data[[v]])

      if (is.null(lbl) || lbl == "") v else as.character(lbl)

  }))

  vars_sel <- sapply(vars_sel_labels, function(lbl) {

    if (lbl %in% names(diccionari_nom_col)) {
      return(diccionari_nom_col[[lbl]])

    } else {

      return(lbl)

    }
  }) |> unname()

  vars_sel <- vars_sel[!is.na(vars_sel)]

### --- 13. Return of the results ---
  return(
    list(
      table = taula_html,
      plot = plot_captured,
      lambda = best_lambda,
      model = fit,
      cv_model = cv_fit,
      outVar = vars_out,
      selVar = vars_sel))
}
