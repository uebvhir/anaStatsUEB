#' @title Detección, análisis y eliminación opcional de registros duplicados por ID
#'
#' @description
#' Recorre una lista de data frames (o un único data frame), identifica los
#' identificadores (ID) duplicados en cada uno, contabiliza los casos, extrae las
#' filas afectadas para su inspección visual y, de forma opcional, elimina los
#' registros de los IDs duplicados. Además, genera una matriz global de
#' presencia/ausencia de los IDs a lo largo de todos los datasets
#' (\code{matriz_id}).
#'
#' @param l_df Una lista con nombres de data frames/tibbles, o un único data
#'   frame/tibble (que se trata como un dataset llamado \code{"dataset"}). Cada
#'   uno debe contener la columna indicada en \code{id_col}; los que no la tengan
#'   se omiten con un mensaje.
#' @param rm_duplicates Lógico. Si es \code{TRUE}, \code{df_cleaned} excluye
#'   \emph{todas} las filas de los IDs que aparecen duplicados (incluida su
#'   primera aparición). Si es \code{FALSE} (por defecto), \code{df_cleaned}
#'   contiene el data frame original sin cambios.
#' @param id_col Carácter. Nombre de la columna que actúa como identificador
#'   (numérico o de texto). Por defecto, \code{"id"}.
#' @param overlap_sets Lista opcional con nombres (por defecto, \code{NULL}) para
#'   identificar solapamientos de IDs. Cada elemento es un vector de caracteres
#'   con nombres de datasets presentes en \code{l_df} (y, por tanto, en
#'   \code{matriz_id}). Para cada elemento \code{overlap_sets[["nom"]] <- c("ds1",
#'   "ds2", ...)} se crean dos columnas lógicas en \code{matriz_id}:
#'   \itemize{
#'     \item \code{nom_any}: el ID está presente en ALGUNO de los datasets del
#'       conjunto (OR).
#'     \item \code{nom_all}: el ID está presente en TODOS los datasets del
#'       conjunto (AND).
#'   }
#'   Los datasets del conjunto que no existen en \code{l_df} se descartan con un
#'   mensaje, y un conjunto sin ningún dataset válido se omite.
#'
#' @return Una lista con un elemento por cada dataset analizado y un elemento
#'   final \code{matriz_id}:
#'   \itemize{
#'     \item \code{[nombre_dataset]}: sublista con \code{n_duplicados} (número de
#'       filas sobrantes), \code{id_duplicados} (IDs repetidos),
#'       \code{id_data_duplicated} (todas las filas de esos IDs, ordenadas por ID)
#'       y \code{df_cleaned} (datos tras la eliminación opcional).
#'     \item \code{matriz_id}: tibble con todos los IDs únicos analizados, con su
#'       presencia (lógica) en cada dataset, el número de datasets donde aparece
#'       (\code{total_datasets}), si está en todos (\code{en_todos}) y la
#'       combinación exacta de datasets (\code{variant_patron}). Es un tibble
#'       vacío si ningún dataset tiene la columna \code{id_col}.
#'   }
#'
#' @details
#' El argumento \code{overlap_sets} permite definir agrupaciones personalizadas de
#' datasets para evaluar solapamientos de identificadores (IDs).
#'
#' Por ejemplo:
#'
#' \preformatted{
#' overlap_sets = list(
#'   iris = c(
#'     "df_ori_iris",
#'     "df_small_iris"),
#'   completos = c(
#'     "df_ori_mtcars",
#'     "df_ori_iris",
#'     "df_small_iris"))
#' }
#'
#' En este caso se crearán cuatro columnas adicionales en \code{matriz_id}:
#'
#' \itemize{
#'   \item \code{iris_any}: el ID está presente en al menos uno de los datasets
#'     \code{df_ori_iris} o \code{df_small_iris}.
#'   \item \code{iris_all}: el ID está presente simultáneamente en
#'     \code{df_ori_iris} y \code{df_small_iris}.
#'   \item \code{completos_any}: el ID está presente en al menos uno de los
#'     datasets \code{df_ori_mtcars}, \code{df_ori_iris} o \code{df_small_iris}.
#'   \item \code{completos_all}: el ID está presente en los tres datasets
#'     simultáneamente.
#' }
#'
#' El sufijo \code{_any} corresponde a una condición lógica OR (presente en
#' cualquiera de los datasets definidos), y el sufijo \code{_all} a una condición
#' AND (presente en todos los datasets definidos).
#'
#' Los avisos (datasets sin la columna \code{id_col}, nombres inexistentes en
#' \code{overlap_sets}) se emiten con \code{message()} y no detienen la
#' ejecución.
#'
#' @seealso \code{\link[base]{duplicated}}, \code{\link[dplyr]{full_join}}
#'
#' @examples
#' # Crear datasets de ejemplo con una columna identificadora
#' mtcars_ex <- mtcars
#' mtcars_ex$id <- paste0("id_", seq_len(nrow(mtcars_ex)))
#'
#' iris_ex <- iris
#' iris_ex$id <- paste0("id_", seq_len(nrow(iris_ex)))
#'
#' # Dataset con solapamiento parcial de IDs
#' iris_small <- iris_ex[c(1:20, 30:40), ]
#'
#' # Introducir algunos duplicados artificiales
#' mtcars_ex <- rbind(mtcars_ex, mtcars_ex[1:2, ])
#' iris_ex <- rbind(iris_ex, iris_ex[1, ])
#'
#' # Lista de datasets
#' l_df <- list(
#'   df_ori_mtcars = mtcars_ex,
#'   df_ori_iris = iris_ex,
#'   df_small_iris = iris_small)
#'
#' # Identificar duplicados, eliminarlos y calcular solapamientos
#' res <- check_ids_overlap_ueb(
#'   l_df = l_df,
#'   id_col = "id",
#'   rm_duplicates = TRUE,
#'   overlap_sets = list(
#'     iris = c("df_ori_iris", "df_small_iris"),
#'     completos = c("df_ori_mtcars", "df_ori_iris", "df_small_iris")))
#'
#' # Numero de registros duplicados detectados
#' res$df_ori_mtcars$n_duplicados
#' res$df_ori_iris$n_duplicados
#'
#' # Registros duplicados encontrados
#' head(res$df_ori_mtcars$id_data_duplicated)
#'
#' # Dataset sin los IDs duplicados
#' head(res$df_ori_mtcars$df_cleaned)
#'
#' # Matriz global de presencia de IDs
#' head(res$matriz_id)
#'
#' # Variables generadas a partir de overlap_sets
#' names(res$matriz_id)
#'
#' @keywords duplicates id overlap datasets data-quality
#'
#' @importFrom dplyr filter arrange select distinct mutate
#' @importFrom dplyr all_of across full_join as_tibble
#' @importFrom purrr reduce
#' @importFrom tibble tibble
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#' 
#' @include check_ids_overlap_ueb.R
#' @export
check_ids_overlap_ueb <- function(
    l_df, 
    rm_duplicates = FALSE,
    id_col = 'id',
    overlap_sets = NULL) {
  
  if (is.data.frame(l_df)) {
    l_df <- list(dataset = l_df)
  }
  
  resultat_global <- list()
  llista_presencia <- list() 
  
  for (nom_dataset in names(l_df)) {
    df <- l_df[[nom_dataset]]
    
    if (!id_col %in% names(df)) {
      message(paste0("Aviso: El dataset '", nom_dataset, "' no contiene la columna '", id_col, "'. Saltado."))
      next
    }
    
    ids_afectats <- df[[id_col]][duplicated(df[[id_col]])] |> unique()
    n_duplicats <- sum(duplicated(df[[id_col]]))
    
    if (length(ids_afectats) > 0) {
      id_data_duplicated <- df |> 
        dplyr::filter(.data[[id_col]] %in% ids_afectats) |> 
        dplyr::arrange(.data[[id_col]])
    } else {
      id_data_duplicated <- df[0, , drop = FALSE]
    }
    
    if (isTRUE(rm_duplicates)) {
      df_cleaned <- df |> dplyr::filter(!(.data[[id_col]] %in% ids_afectats))
    } else {
      df_cleaned <- df
    }
    
    llista_presencia[[nom_dataset]] <- df |> 
      dplyr::select(dplyr::all_of(id_col)) |> 
      dplyr::distinct() |> 
      dplyr::mutate(!!nom_dataset := TRUE)
    
    resultat_global[[nom_dataset]] <- list(
      n_duplicados = n_duplicats,
      id_duplicados = ids_afectats,
      id_data_duplicated = id_data_duplicated,
      df_cleaned = df_cleaned
    )
  }
  
  # === CONSTRUCCIÓ I ENRIQUIMENT DE 'matriz_id' ===
  if (length(llista_presencia) > 0) {
    noms_ds <- names(llista_presencia)
 
    matriz_id <- llista_presencia |>
      purrr::reduce(dplyr::full_join, by = id_col) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(noms_ds), ~ !is.na(.x)))
 
    # Matriu lògica calculada UNA sola vegada (evita repetir across() a cada pas)
    mat_logica <- as.matrix(matriz_id[, noms_ds, drop = FALSE])
 
    matriz_id <- matriz_id |>
      dplyr::mutate(
        total_datasets = rowSums(mat_logica),
        en_todos = total_datasets == length(noms_ds),
        variant_patron = apply(mat_logica, 1, function(fila) {
          paste(noms_ds[fila], collapse = " + ")
        })
      )
 
    # --- COLUMNES DE SOLAPAMENT GENERALITZADES (overlap_sets) ---
    if (!is.null(overlap_sets) && length(overlap_sets) > 0) {
      for (nom_combo in names(overlap_sets)) {
        ds_combo <- overlap_sets[[nom_combo]]
        ds_trobats <- intersect(ds_combo, noms_ds)
        ds_no_trobats <- setdiff(ds_combo, noms_ds)
 
        if (length(ds_no_trobats) > 0) {
          message(paste0(
            "Aviso: overlap_sets[['", nom_combo, "']] conté dataset(s) inexistent(s) a l_df: ",
            paste(ds_no_trobats, collapse = ", "), ". S'ignoren."
          ))
        }
 
        if (length(ds_trobats) == 0) {
          message(paste0("Aviso: overlap_sets[['", nom_combo, "']] no té cap dataset vàlid. Saltat."))
          next
        }
 
        sub_mat <- mat_logica[, ds_trobats, drop = FALSE]
        matriz_id[[paste0(nom_combo, "_any")]] <- apply(sub_mat, 1, any)
        matriz_id[[paste0(nom_combo, "_all")]]  <- apply(sub_mat, 1, all)
      }
    }
 
    matriz_id <- matriz_id |> dplyr::as_tibble()
    
  } else {

    matriz_id <- dplyr::tibble()
  }
 
  resultat_global[["matriz_id"]] <- matriz_id
 
  return(resultat_global)

}