#' Fill, Update, and Optionally Derive Variable Labels
#'
#' @description
#' Fills empty variable labels with variable names, updates labels if provided,
#' and optionally derives labels and categorical levels from variable names
#' formatted as:
#'
#' \code{"Variable (0=No; 1=Yes; 2=Other)"}.
#'
#' Parsed category mappings are stored as an attribute (\code{"var_levels"})
#' for downstream use (e.g., with \code{factor_ueb()}).
#'
#' @param df A data.frame or tibble.
#' @param lbls Named vector or list of labels (optional).
#' @param warn_missing Logical. Warn if variables in \code{lbls} are not in \code{df}.
#' @param store Logical. Store labels in attribute \code{"var_labels"}.
#' @param parse_names Logical. If TRUE, extract labels and categorical levels from \code{names(df)}.
#'
#' @return A list with:
#' \describe{
#'   \item{data}{Data frame with updated labels}
#'   \item{labels}{Final label vector}
#' }
#'
#' @importFrom Hmisc label
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