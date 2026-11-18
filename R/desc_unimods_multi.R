#' @title Modelos multinomiales univariables y ajustados para múltiples predictores
#'
#' @description
#' Ajusta una serie de modelos de regresión logística multinomial con
#' \code{\link[nnet]{multinom}} y resume los resultados en una tabla HTML con
#' formato. Cada predictor de \code{var_comp} se evalúa en un modelo distinto y,
#' opcionalmente, se ajusta por un conjunto común de covariables indicado en
#' \code{adjust_vars}. Se calculan los riesgos relativos (RRR), sus intervalos de
#' confianza y los p-valores de cada categoría del outcome respecto a la
#' categoría de referencia. Las asociaciones significativas pueden resaltarse a
#' nivel de fila o directamente en las celdas de p-valor.
#'
#' La función devuelve tanto una tabla lista para informes como los resultados
#' numéricos subyacentes, incluidos los modelos ajustados.
#'
#' @param var_out Cadena de texto con el nombre de la variable respuesta
#'   multinomial.
#' @param var_comp Vector de caracteres con las variables predictoras a evaluar.
#'   Se ajusta un modelo multinomial para cada predictor.
#' @param adjust_vars Vector de caracteres o \code{NULL}. Covariables incluidas
#'   en todos los modelos además de cada predictor de \code{var_comp}. Por
#'   defecto, \code{NULL}, es decir, modelos estrictamente univariables.
#' @param data Data frame con las variables del estudio.
#' @param col.background Cadena de texto con el color de fondo de las cabeceras
#'   de la tabla. Por defecto, \code{"#993489"}.
#' @param align Vector de caracteres con la alineación de las columnas de la
#'   tabla. Si es \code{NULL}, la columna de niveles se alinea a la izquierda y
#'   el resto se centran.
#' @param pval_cut Valor numérico con el umbral de significación utilizado para
#'   el resaltado. Por defecto, 0.05.
#' @param col.varsel Cadena de texto con el color de fondo para resaltar las
#'   filas de los predictores con al menos un p-valor significativo. Con
#'   \code{NULL} se desactiva el resaltado de filas. Por defecto,
#'   \code{"#ebe0e9"}.
#' @param col.varsel_pval Especificación opcional del resaltado de las celdas de
#'   p-valor significativas. Puede ser:
#'   \itemize{
#'     \item \code{NULL}: sin resaltado de celdas.
#'     \item Un único color: aplica un color de fondo fijo.
#'     \item \code{"gradient"}: aplica un gradiente de color por defecto según
#'       la significación.
#'     \item Un vector de dos colores: define un gradiente personalizado.
#'   }
#' @param font_size Valor numérico con el tamaño de fuente de la tabla. Por
#'   defecto, 12.
#' @param width_lev Valor numérico conservado por compatibilidad con versiones
#'   anteriores de la función. Actualmente no se utiliza.
#' @param conf_level Valor numérico con el nivel de confianza de los intervalos.
#'   Por defecto, 0.95.
#' @param group Lógico. Si es \code{TRUE} (por defecto), las filas se agrupan por
#'   variable con \code{\link[kableExtra]{group_rows}}: la etiqueta de cada
#'   variable aparece como fila de cabecera de grupo y la primera columna
#'   contiene solo el nivel, como en \code{\link{desc_unimods}}. Si es
#'   \code{FALSE}, no se añaden cabeceras de grupo y solo se muestra la columna
#'   de niveles.
#' @param fixed_thead Lógico. Se pasa a \code{\link[kableExtra]{kable_styling}}.
#'   Por defecto, \code{FALSE}: como la tabla tiene dos filas de cabecera (las
#'   categorías del outcome, añadidas con \code{add_header_above()}, y los
#'   nombres de columna), con \code{TRUE} kableExtra fija ambas con
#'   \code{position: sticky; top: 0}, de modo que se superponen al desplazarse y
#'   la fila del outcome queda oculta tras la de nombres de columna. Úsese
#'   \code{TRUE} solo si la tabla es lo bastante corta para no desplazarse.
#' @param footnote Vector de caracteres opcional con notas al pie de la tabla.
#' @param caption Cadena de texto con el título de la tabla. Si es \code{NULL},
#'   se genera un título por defecto.
#'
#' @return Lista con los elementos:
#' \describe{
#'   \item{table}{Tabla HTML con formato que resume los resultados de los
#'     modelos multinomiales.}
#'   \item{results}{Data frame en formato largo, con una fila por predictor (o
#'     nivel) y categoría del outcome, y las columnas \code{Outcome},
#'     \code{Level}, \code{Relative Risk Ratio}, \code{SE}, \code{Lower95},
#'     \code{Upper95}, \code{P.value} y \code{Variable}.}
#'   \item{models}{Lista con nombre de los modelos multinomiales ajustados, uno
#'     por predictor.}
#'   \item{lbls_vars}{Vector de caracteres con nombre con la etiqueta de cada
#'     predictor.}
#'   \item{lbls_levels}{Lista con nombre con los niveles observados de cada
#'     predictor factor (\code{NULL} para los no factores).}
#' }
#'
#' @details
#' Para cada predictor de \code{var_comp} se ajusta un modelo de regresión
#' logística multinomial. Si \code{adjust_vars} es \code{NULL}, el modelo es:
#'
#' \deqn{Outcome \sim Predictor}
#'
#' En caso contrario, cada modelo incluye el predictor de interés junto con
#' todas las covariables de ajuste:
#'
#' \deqn{Outcome \sim Predictor + Covariates}
#'
#' Los riesgos relativos (RRR) se obtienen exponenciando los coeficientes de
#' regresión. Los intervalos de confianza se calculan con la aproximación
#' normal:
#'
#' \deqn{\exp(\beta \pm z_{\alpha/2} \times SE)}
#'
#' donde \eqn{SE} es el error estándar del coeficiente. Las columnas de límites
#' del data frame \code{results} se llaman siempre \code{Lower95} y
#' \code{Upper95}, aunque se modifique \code{conf_level}.
#'
#' \strong{Diseño de la tabla.} La tabla sigue la misma estructura de filas que
#' \code{\link{desc_unimods}}: la primera columna (\code{levs}) contiene solo el
#' nivel del predictor, precedido de \code{": "}, y cada variable aparece como
#' fila de cabecera de grupo (véase \code{group}). Los predictores continuos
#' tienen la celda de nivel vacía. Sobre las columnas, hay una cabecera agrupada
#' por categoría del outcome con:
#' \itemize{
#'   \item el riesgo relativo (RRR) con su intervalo de confianza;
#'   \item el p-valor.
#' }
#' y una columna final \code{N} con el tamaño muestral de cada modelo, mostrada
#' una vez por variable.
#'
#' \strong{Resaltado.} Los predictores significativos se resaltan según
#' \code{pval_cut}. Si se indica \code{col.varsel_pval} y \code{col.varsel}
#' conserva su valor por defecto (\code{"#ebe0e9"}), el resaltado de filas se
#' desactiva y solo se resaltan las celdas de p-valor. Si \code{col.varsel} se
#' cambia a otro color, ambos resaltados se aplican a la vez.
#'
#' @seealso \code{\link{desc_unimods}}, \code{\link{plot_forest_multimods}},
#'   \code{\link{desc_multimod}}, \code{\link[nnet]{multinom}}
#'
#' @examples
#' set.seed(123)
#' n <- 300
#'
#' mydata <- data.frame(
#'   outcome = factor(sample(c("Bajo", "Medio", "Alto"), n, replace = TRUE,
#'                           prob = c(0.3, 0.4, 0.3))),
#'   age = round(rnorm(n, mean = 55, sd = 12)),
#'   bmi = round(rnorm(n, mean = 27, sd = 4), 1),
#'   sex = factor(sample(c("Hombre", "Mujer"), n, replace = TRUE)),
#'   smoker = factor(sample(c("No", "Si"), n, replace = TRUE, prob = c(0.7, 0.3)))
#' )
#'
#' # Etiquetas opcionales
#' Hmisc::label(mydata$age) <- "Edad"
#' Hmisc::label(mydata$bmi) <- "Indice de masa corporal"
#' Hmisc::label(mydata$sex) <- "Sexo"
#'
#' # Modelos univariables
#' result <- desc_unimods_multi(
#'   var_out = "outcome",
#'   var_comp = c("age", "sex", "bmi"),
#'   data = mydata
#' )
#' result$table
#' head(result$results)
#' names(result$models)
#'
#' # Modelos ajustados por tabaquismo, con gradiente en los p-valores
#' result_adj <- desc_unimods_multi(
#'   var_out = "outcome",
#'   var_comp = c("age", "sex"),
#'   adjust_vars = "smoker",
#'   data = mydata,
#'   col.varsel_pval = "gradient"
#' )
#' result_adj$table
#'
#' @keywords multinomial regression logistic univariate relative-risk-ratio
#'
#' @importFrom nnet multinom
#' @importFrom stats qnorm pnorm as.formula
#' @importFrom Hmisc label
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr bind_rows mutate filter select
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom purrr map
#' @importFrom kableExtra kable_styling add_header_above row_spec column_spec
#'   add_footnote cell_spec group_rows
#'
#' @author
#' Àlex Martí Barrera, Alba García Zarzoso, Miriam Mota
#'
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Modificaciones y mantenimiento:
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
desc_unimods_multi <- function(
    var_out,
    var_comp,
    adjust_vars = NULL,
    data,
    col.background = "#993489",
    align = NULL,
    pval_cut = 0.05,
    col.varsel = "#ebe0e9",
    col.varsel_pval = NULL,
    font_size = 12,
    width_lev = 25,
    conf_level = 0.95,
    group = TRUE,
    fixed_thead = FALSE,
    footnote = NULL,
    caption = NULL
){

  if (!requireNamespace("nnet", quietly = TRUE)) {
    stop("El paquete 'nnet' es necesario para ajustar modelos multinomiales.",
         call. = FALSE)
  }

  # Lògica d'interacció: si s'activa color a la cel·la, es desactiva la fila
  if (!is.null(col.varsel_pval) && !is.null(col.varsel) && col.varsel == "#ebe0e9") {
    col.varsel <- NULL
  }


  # Funciones auxiliares

  get_var_label <- function(var, data){

    lab <- tryCatch(label(data[[var]]), error = function(e) NULL)
    if(is.null(lab) || length(lab) == 0){ return(var) }
    as.character(lab)
  }

  process_df <- function(df){
    df <- df |> tibble::rownames_to_column("Grupo")
    df_long <- df |>
      pivot_longer(-Grupo, names_to = "Metric", values_to = "Value") |>
      unite("col", Grupo, Metric)
    df_long |> pivot_wider(names_from = col, values_from = Value)
  }


  # Ajuste de modelos

  coef_list <- list()
  p_list    <- list()
  ici_list  <- list()
  ics_list  <- list()
  df_list   <- list()

  z_value <- qnorm(1 - (1 - conf_level) / 2)
  results_df <- data.frame()
  model_list<-list()
  n_list<-list()

  for(i in seq_along(var_comp)){
    vars_model <- unique(c(var_comp[i], adjust_vars))
    frm <- reformulate(
      termlabels = vars_model,
      response = var_out
    )

    mod <- nnet::multinom(frm, data = data, trace = FALSE)
    model_list[[var_comp[i]]] <- mod
    n_list[[var_comp[i]]] <- nrow(model.frame(mod))
    sm <- summary(mod)

    coef_names <- colnames(sm$coefficients)
    keep <- startsWith(coef_names, var_comp[i])
    coef_mat <- sm$coefficients[, keep, drop = FALSE]
    se_mat   <- sm$standard.errors[, keep, drop = FALSE]


    z_mat <- coef_mat / se_mat
    p_mat <- 2 * (1 - pnorm(abs(z_mat)))

    ici_mat <- coef_mat - z_value * se_mat
    ics_mat <- coef_mat + z_value * se_mat

    # Guardar resultados en formato largo


    coef_long <- as.data.frame(as.table(exp(coef_mat)))
    names(coef_long) <- c("Outcome", "Level", "Relative Risk Ratio")

    coef_long$SE <- as.vector(exp(se_mat))
    coef_long$Lower95 <- as.vector(exp(ici_mat))
    coef_long$Upper95 <- as.vector(exp(ics_mat))
    coef_long$P.value <- as.vector(p_mat)
    coef_long$Variable <- var_comp[i]

    results_df <- dplyr::bind_rows(results_df, coef_long)


    # Exponenciamos coeficientes e intervalos: RRR e IC95% de la RRR

    coef_list[[i]] <- round(exp(coef_mat), 3)
    p_list[[i]]    <- round(p_mat, 3)
    ici_list[[i]]  <- round(exp(ici_mat), 3)
    ics_list[[i]]  <- round(exp(ics_mat), 3)

    est <- as.matrix(coef_list[[i]])
    p   <- as.matrix(p_list[[i]])
    ici <- as.matrix(ici_list[[i]])
    ics <- as.matrix(ics_list[[i]])

    out <- as.data.frame(
      do.call(cbind, lapply(seq_len(ncol(est)), function(j){
        cbind(
          `Relative Risk Ratio` = paste0(est[, j], " (", ici[, j], ", ", ics[, j], ")"),
          p.value = p[, j]
        )
      }))
    )

    colnames(out) <- as.vector(rbind(paste0("Relative Risk Ratio", colnames(est)), paste0("p.value", colnames(est))))
    df_list[[i]] <- out
  }


  # Transformación resultados

  df_processed <- purrr::map(df_list, process_df)
  df_combined <- dplyr::bind_rows(df_processed)
  df_combined <- df_combined |> dplyr::mutate(Variable = var_comp, .before = 1)

  df_clean <- df_combined |>
    dplyr::mutate(across(everything(), ~ na_if(as.character(.), "NULL"))) |>
    dplyr::mutate(row_id = row_number()) |>
    tidyr::pivot_longer(
      cols = -c(Variable, row_id),
      names_to = c("Grupo", "Tipo", "Nivel"),
      names_pattern = "^(.*?)_(Relative Risk Ratio|p.value)(.*)$",
      values_to = "valor"
    ) |>
    dplyr::filter(!is.na(valor), valor != "NA") |>
    dplyr::mutate(Nivel = ifelse(Nivel == "", "base", Nivel)) |>
    tidyr::pivot_wider(
      id_cols = c(Variable, row_id, Nivel),
      names_from = c(Grupo, Tipo),
      names_glue = "{Grupo}_{Tipo}",
      values_from = "valor"
    ) |>
    dplyr::select(-row_id)


  # Limpieza niveles
  # El nombre del coeficiente es 'variable + nivel': se elimina el prefijo de
  # la variable para quedarse solo con el nivel. En variables continuas el
  # nombre del coeficiente coincide con la variable, por lo que el nivel
  # resultante queda vacio.

  df_clean$Nivel <- ifelse(
    df_clean$Nivel == "base",
    df_clean$Nivel,
    mapply(function(var, niv){ sub(paste0("^", var), "", niv) }, df_clean$Variable, df_clean$Nivel)
  )

  idx_base <- which(df_clean$Nivel == "base")
  if(length(idx_base) > 0){
    for(i in idx_base){
      var_i <- df_clean$Variable[i]
      if(is.factor(data[[var_i]]) && nlevels(data[[var_i]]) >= 2){
        df_clean$Nivel[i] <- levels(data[[var_i]])[2]
      }
    }
  }


  # Etiquetas variables
  # 'N' se muestra una sola vez por variable (primera fila)

  df_clean$N <- unlist(n_list[df_clean$Variable], use.names = FALSE)
  df_clean$N[duplicated(df_clean$Variable)] <- ""
  df_clean$Variable <- sapply(df_clean$Variable, get_var_label, data = data)

  # Formato p-values

  p_cols <- grep("_p.value$", names(df_clean), value = TRUE)
  df_clean <- df_clean |>
    dplyr::mutate(across(all_of(p_cols), ~ case_when(. == 0 ~ "<0.001", TRUE ~ as.character(.))))


  # Resaltado p-values i cerca de fileres per Variable completa

  df_tabla <- df_clean
  cols_p <- grep("value$", names(df_tabla))

  sig_vars <- c()
  for(i in cols_p){
    vals_num <- suppressWarnings(as.numeric(df_tabla[[i]]))
    idx <- which(vals_num < pval_cut | df_tabla[[i]] == "<0.001")

    if(length(idx) > 0){
      sig_vars <- union(sig_vars, df_tabla$Variable[idx])

      if(!is.null(col.varsel_pval)){
        # COMPROVACIÓ SEGURA DE TIPUS DE GRADIENT (Evita l'error de longitud)
        es_gradient <- (length(col.varsel_pval) == 1 && col.varsel_pval == "gradient") || length(col.varsel_pval) == 2

        if(es_gradient){
          # Si l'usuari passa 2 colors, els usem. Si posa "gradient", usem els pastels base.
          colors_escala <- if(length(col.varsel_pval) == 2) col.varsel_pval else c("#FFB3B3", "#B3D1FF")
          col_ramp <- colorRamp(colors_escala)

          for(row_idx in idx){
            p_str <- df_tabla[[i]][row_idx]
            p_num <- if(p_str == "<0.001") { 0 } else { suppressWarnings(as.numeric(p_str)) }
            p_num <- ifelse(is.na(p_num), pval_cut, p_num)

            val_norm <- min(max(p_num / pval_cut, 0), 1)

            rgb_matrix <- col_ramp(val_norm)
            cell_color <- rgb(rgb_matrix[1,1], rgb_matrix[1,2], rgb_matrix[1,3], maxColorValue = 255)

            df_tabla[[i]][row_idx] <- cell_spec(df_tabla[[i]][row_idx], bold = TRUE, background = cell_color)
          }
        } else if (length(col.varsel_pval) == 1) {
          # Color fix independent
          df_tabla[[i]][idx] <- cell_spec(df_tabla[[i]][idx], bold = TRUE, background = col.varsel_pval)
        }
      } else {
        df_tabla[[i]][idx] <- cell_spec(df_tabla[[i]][idx], bold = TRUE)
      }
    }
  }

  sig_rows <- which(df_clean$Variable %in% sig_vars)


  # Estructura de filas al estilo 'desc_unimods()'
  # - Columna 'levs': solo el nivel, precedido de ': ' (vacia si es continua)
  # - Etiqueta de variable: cabecera de grupo ('group_rows'), no columna

  vars_label_rows <- as.character(df_clean$Variable)

  levs_col <- ifelse(
    is.na(df_tabla$Nivel) | !nzchar(df_tabla$Nivel),
    "",
    paste0(": ", df_tabla$Nivel)
  )

  cols_data <- setdiff(names(df_tabla), c("Variable", "Nivel"))
  cols_stat <- setdiff(cols_data, "N")

  df_tabla <- data.frame(
    levs = levs_col,
    df_tabla[, cols_data, drop = FALSE],
    check.names = FALSE,
    stringsAsFactors = FALSE
  )


  # Cabecera agrupada: 1 columna de nivel + 2 columnas por categoria + N

  strat_names <- sub("_.*$", "", cols_stat)
  tab_header <- table(factor(strat_names, levels = unique(strat_names)))

  header_vector <- c(" " = 1, as.numeric(tab_header))
  names(header_vector)[-1] <- names(tab_header)
  if("N" %in% cols_data){
    header_vector <- c(header_vector, " " = 1)
  }

  names(df_tabla) <- c("levs", sub("^[^_]+_", "", cols_data))


  # Alineación: nivel a la izquierda, resultados centrados
  if(is.null(align)){ align <- c("l", rep("c", ncol(df_tabla) - 1)) }

  # Caption
  if (is.null(caption)) {
    caption <- paste0(
      "Univariate multinomial regression (", get_lab_nam(data, var_out), ")."
    )
  }

  # Tabla final
  tabla <- df_tabla |>
    kable(
      escape = FALSE,
      row.names = FALSE,
      align = align,
      caption = caption,
      format = "html",
      table.attr = 'class="table-with-group-header"'
    )

  # Cabecera de grupo por variable (mismo comportamiento que 'desc_unimods()')
  if(isTRUE(group)){
    tabla <- tabla |>
      kableExtra::group_rows(
        index = table(factor(vars_label_rows, levels = unique(vars_label_rows)))
      )
  }

  if(!is.null(col.varsel) && length(sig_rows) > 0){
    tabla <- tabla |> row_spec(sig_rows, background = col.varsel)
  }

  # 'fixed_thead' y cabeceras de dos filas
  # Con 'add_header_above()' la tabla tiene DOS filas de cabecera. Si
  # 'fixed_thead = TRUE', kableExtra fija ambas con 'position: sticky;
  # top: 0', de modo que al desplazarse se superponen en la misma posicion
  # y la fila de categorias del outcome queda tapada por la de nombres de
  # columna. Por eso el valor por defecto es FALSE.

  tabla <- tabla |>
    add_header_above(header_vector, background = col.background, color = "white", bold = TRUE) |>
    kable_styling(
      latex_options = c("striped", "hold_position", "repeat_header"),
      font_size = font_size,
      full_width = FALSE,
      fixed_thead = fixed_thead
    ) |>
    row_spec(0, background = col.background, color = "white") |>
    add_footnote(footnote, escape = FALSE, notation = "symbol")

  # Coger etiquetas de variables y niveles de variables factor
    var_labels <- sapply(var_comp, get_var_label, data = data, simplify = TRUE)

    level_labels <- purrr::map(var_comp, function(v){
      if (is.factor(data[[v]])) {
        # droplevels() garanteix que només es guarden els nivells amb
        # observacions realment presents a 'data' (ex.: després d'aplicar
        # filter_sparse_levels()), independentment de si l'atribut
        # levels() del factor ja s'havia netejat aigües amunt o no.
        lv <- levels(droplevels(data[[v]]))
        stats::setNames(lv, lv)
      } else {
        NULL
      }
    })

    names(level_labels) <- var_comp
    names(var_labels) <- var_comp

    return(list(
      table = tabla,
      results = results_df,
      models = model_list,
      lbls_vars = var_labels,
      lbls_levels = level_labels
    ))
  }
