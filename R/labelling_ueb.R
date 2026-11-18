#' @title Completar, actualizar y derivar etiquetas de variables
#'
#' @description
#' Rellena con el nombre de la variable las etiquetas vacías de un data frame,
#' actualiza las etiquetas si se proporcionan y, de forma opcional, deriva
#' etiquetas y niveles categóricos a partir de nombres de variable con el formato:
#'
#' \code{"Variable (0=No; 1=Si; 2=Otro)"}.
#'
#' Las correspondencias de categorías extraídas se guardan en el atributo
#' \code{"var_levels"} del data frame, para su uso posterior.
#'
#' @param df Data frame o tibble.
#' @param lbls Vector o lista con nombres de etiquetas (opcional). Los nombres
#'   deben coincidir con los de las columnas de \code{df}.
#' @param warn_missing Lógico. Si es \code{TRUE} (por defecto), avisa cuando
#'   variables de \code{lbls} no están en \code{df}. Solo se aplica con
#'   \code{parse_names = FALSE}.
#' @param store Lógico. Si es \code{TRUE} (por defecto), guarda las etiquetas
#'   finales en el atributo \code{"var_labels"} de \code{df}.
#' @param parse_names Lógico. Si es \code{TRUE}, extrae etiquetas y niveles
#'   categóricos de los nombres de columna (\code{names(df)}). Por defecto,
#'   \code{FALSE}.
#'
#' @return Lista con dos elementos:
#' \describe{
#'   \item{data}{Data frame con las etiquetas actualizadas (\code{Hmisc::label}).
#'     Con \code{store = TRUE} incluye el atributo \code{"var_labels"} y, con
#'     \code{parse_names = TRUE}, también \code{"var_levels"}.}
#'   \item{labels}{Vector de caracteres con nombres (las columnas de \code{df}) con
#'     las etiquetas finales.}
#' }
#'
#' @details
#' Con \code{parse_names = FALSE}, las etiquetas vacías se sustituyen por el
#' nombre de la variable y, si se indica \code{lbls}, se actualizan las etiquetas
#' de las variables correspondientes. Un \code{lbls} sin nombres produce un error,
#' y las variables de \code{lbls} que no están en \code{df} se ignoran (con un
#' aviso si \code{warn_missing = TRUE}).
#'
#' Con \code{parse_names = TRUE}, la etiqueta de cada variable es el nombre de la
#' columna sin el texto entre paréntesis final, y las correspondencias
#' \code{codigo=etiqueta} separadas por \code{;} se guardan en
#' \code{attr(df, "var_levels")}: una lista con un elemento por columna, que es un
#' vector con nombres (código y etiqueta) o \code{NULL} si el nombre no tiene
#' paréntesis. En este modo se descartan las etiquetas existentes y solo prevalecen
#' las de \code{lbls}, que deben corresponder a columnas de \code{df}: si no, se
#' produce un error y no se avisa.
#'
#' \code{labelling_ueb()} guarda \code{"var_levels"} pero ninguna otra función del
#' paquete lo utiliza actualmente.
#'
#' @seealso \code{\link[Hmisc]{label}}, \code{\link{factor_ueb}},
#'   \code{\link{get_lab_nam}}
#'
#' @examples
#' df <- data.frame(id = 1:3, edad = c(50, 60, 70), sexo = c(0, 1, 1))
#' Hmisc::label(df$edad) <- "Edad (anos)"
#'
#' # Rellenar etiquetas vacias y actualizar alguna
#' res <- labelling_ueb(df, lbls = c(id = "Identificador"))
#' res$labels
#'
#' # Derivar etiquetas y niveles a partir de los nombres de columna
#' df2 <- data.frame(`Sexo (0=Hombre; 1=Mujer)` = c(0, 1, 1), check.names = FALSE)
#' res2 <- labelling_ueb(df2, parse_names = TRUE)
#' res2$labels
#' attr(res2$data, "var_levels")
#'
#' @keywords labels variables data-frame Hmisc
#'
#' @importFrom Hmisc label
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
labelling_ueb <- function(
  df,
  lbls = NULL,
  warn_missing = TRUE,
  store = TRUE,
  parse_names = FALSE)
{

  if (!is.data.frame(df)) {
    stop("'df' must be a data.frame or tibble.", call. = FALSE)
  }

  # --- 1. Extract current labels ---
  current_lbls <- Hmisc::label(df, self = FALSE)

  # --- 2. Fill empty labels with variable names ---
  empty_lbls <- is.na(current_lbls) | current_lbls == ""
  current_lbls[empty_lbls] <- names(df)[empty_lbls]

  # --- 3. Parse labels from names (optional) ---
  if (isTRUE(parse_names)) {

    raw_names <- names(df)

    parse_var <- function(x) {

      # no parentheses → only label
      if (!grepl("\\(", x)) {
        return(list(
          label = trimws(x),
          levels = NULL
        ))
      }

      # extract label
      label <- trimws(sub("\\s*\\(.*\\)$", "", x))

      # extract inside ()
      inside <- sub(".*\\((.*)\\).*", "\\1", x)
      parts <- unlist(strsplit(inside, ";"))

      lev_list <- lapply(parts, function(p) {
        kv <- strsplit(p, "=")[[1]]
        if (length(kv) == 2) {
          list(
            name = trimws(kv[1]),
            value = trimws(kv[2])
          )
        } else NULL
      })

      lev_list <- lev_list[!sapply(lev_list, is.null)]

      levs <- NULL
      if (length(lev_list) > 0) {
        levs <- setNames(
          sapply(lev_list, `[[`, "value"),
          sapply(lev_list, `[[`, "name")
        )
      }

      list(
        label = label,
        levels = levs
      )
    }

    parsed <- lapply(raw_names, parse_var)

    auto_labels <- sapply(parsed, `[[`, "label")
    names(auto_labels) <- names(df)

    auto_levels <- lapply(parsed, `[[`, "levels")
    names(auto_levels) <- names(df)

    # Merge with existing labels (manual has priority)
    current_lbls <- auto_labels
    if (!is.null(lbls)) {
      current_lbls[names(lbls)] <- as.character(unlist(lbls))
    }

    # store levels
    attr(df, "var_levels") <- auto_levels
  }

  # --- 4. Apply manual labels (if provided, and not parse_names mode) ---
  if (!is.null(lbls) && !parse_names) {

    if (is.list(lbls)) {
      lbls <- unlist(lbls, use.names = TRUE)
    }

    if (is.null(names(lbls)) || any(names(lbls) == "")) {
      stop("'lbls' must be a named vector or named list.", call. = FALSE)
    }

    vars_ok <- intersect(names(lbls), names(df))
    vars_missing <- setdiff(names(lbls), names(df))

    if (warn_missing && length(vars_missing) > 0) {
      warning(
        "Variables not found in 'df': ",
        paste(vars_missing, collapse = ", "),
        call. = FALSE
      )
    }

    current_lbls[vars_ok] <- as.character(lbls[vars_ok])
  }

  # --- 5. Apply labels to dataframe ---
  Hmisc::label(df, self = FALSE) <- current_lbls

  # --- 6. Store labels ---
  if (store) {
    attr(df, "var_labels") <- current_lbls
  }

  # --- 7. Return ---
  return(list(
    data = df,
    labels = current_lbls
  ))
}