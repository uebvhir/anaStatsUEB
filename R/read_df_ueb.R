#' @title Lectura de datos desde múltiples formatos de archivo
#'
#' @description
#' Lee una lista de archivos y devuelve su contenido como una lista de data frames.
#' Admite Excel (\code{.xls}, \code{.xlsx}), CSV y SPSS (\code{.sav}). El tipo de
#' archivo debe indicarse con \code{type_file}.
#'
#' En los archivos Excel se leen todas las hojas y cada una se devuelve como un
#' elemento distinto. En los CSV y SAV, cada archivo se lee como un data frame.
#'
#' @param l_files Vector de caracteres (o lista) con las rutas de los archivos, por
#'   ejemplo el resultado de \code{\link{list_files_ueb}}.
#' @param type_file Tipo de archivo a leer: \code{"excel"}, \code{"csv"} o
#'   \code{"sav"}. Es obligatorio; véase \sQuote{Details}.
#' @param sep Separador de campos para CSV (por defecto, \code{NULL}). En la versión
#'   actual \code{NULL} no autodetecta el separador; hay que indicarlo.
#' @param dec Separador decimal para CSV (por defecto, \code{NULL}). En la versión
#'   actual hay que indicarlo (\code{"."} o \code{","}), porque \code{NULL} produce
#'   un error.
#' @param skip_rows Número de filas que se omiten al principio del archivo (por
#'   defecto, 0). Se aplica a Excel y CSV; en SAV no tiene efecto.
#' @param user_na Lógico. Solo para SPSS: si es \code{TRUE} (por defecto), las
#'   variables con valores perdidos definidos por el usuario se devuelven como
#'   \code{labelled_spss()}; si es \code{FALSE}, esos valores se convierten en
#'   \code{NA}.
#' @param na_strings Vector de caracteres con los valores que se interpretan como
#'   perdidos, para Excel y CSV (no se aplica a SAV). Si es \code{NULL} (por
#'   defecto), en Excel solo se consideran perdidas las celdas vacías y en CSV
#'   \code{""} y \code{"NA"}.
#' @param clean_names Lógico. Si es \code{TRUE} (por defecto), los nombres de las
#'   columnas se convierten a \code{snake_case} con
#'   \code{\link[janitor]{clean_names}} y se eliminan las columnas vacías.
#' @param rm_x_cols Lógico. Si es \code{TRUE}, se eliminan las columnas cuyo nombre
#'   empieza por \code{"..."} (columnas sin nombre en \pkg{readxl}) o es de la forma
#'   \code{x<número>} (sin distinguir mayúsculas), por ejemplo \code{x1} o
#'   \code{X2}. Por defecto, \code{FALSE}.
#' @param id_col Vector de caracteres con el nombre de la columna de identificador
#'   (por defecto, \code{NULL}). Se busca sin distinguir mayúsculas y se usa la
#'   primera coincidencia de cada data frame. Véase \sQuote{Details}.
#' @param id_sheet Lógico. Si es \code{TRUE}, homogeneiza el identificador entre los
#'   elementos de la lista (por ejemplo, las hojas de Excel): al identificador del
#'   elemento \code{i} se le suma \code{i * id_step} si es menor que ese valor, y el
#'   nombre del elemento se añade al identificador (\code{id_<hoja>_<id>_<NN>}).
#'   Crea las columnas \code{id_orig}, \code{id_num} e \code{id_sheet}. Requiere
#'   \code{id_col}. Por defecto, \code{FALSE}.
#' @param id_step Numérico. Salto usado cuando \code{id_sheet = TRUE} (por defecto,
#'   100: primer elemento, 101...; segundo elemento, 201...).
#'
#' @return
#' Una lista de data frames. En Excel, cada hoja es un elemento con el nombre de la
#' hoja; en CSV (\code{data.table}) y SAV (tibble) los elementos no tienen nombre. Si
#' \code{l_files} está vacío, devuelve \code{NULL}.
#'
#' @details
#' \itemize{
#'   \item Los archivos Excel se leen con \code{readxl::read_excel()}, los CSV con
#'     \code{data.table::fread()} y los SAV con \code{haven::read_sav()}, que
#'     conserva las etiquetas.
#'   \item Con \code{clean_names = TRUE}, además de pasar los nombres a
#'     \code{snake_case}, se eliminan las columnas vacías con
#'     \code{janitor::remove_empty()}.
#'   \item Con \code{id_col}, se crean las columnas \code{id}, \code{id_ori} y
#'     \code{event} (situadas al principio). \code{id} tiene el formato
#'     \code{id_<valor>_<NN>}, donde \code{NN} es el número de orden dentro de cada
#'     valor del identificador (útil si se repite) y el valor se rellena con ceros
#'     según el máximo. Si el identificador no es numérico, aparece un aviso de
#'     coerción. Los data frames sin la columna \code{id_col} no se modifican.
#'   \item Nota: en la versión actual, \code{type_file} no se valida: si falta o es
#'     inválido se produce un error poco claro. Si \code{l_files} es una lista,
#'     se muestra un mensaje engañoso (\dQuote{No s'han trobat llistat d'arxius}),
#'     aunque la lectura continúa.
#' }
#'
#' @seealso \code{\link{list_files_ueb}}, \code{\link[readxl]{read_excel}},
#'   \code{\link[data.table]{fread}}, \code{\link[haven]{read_sav}},
#'   \code{\link[janitor]{clean_names}}
#'
#' @examples
#' tmp <- tempfile("read_df_ueb_")
#' dir.create(tmp)
#' write.csv(data.frame(ID = 1:3, Edad = c(20, 30, 40)),
#'           file.path(tmp, "a.csv"), row.names = FALSE)
#' write.csv(data.frame(ID = c(1, 2, 2), Peso = c(70, 80, 90)),
#'           file.path(tmp, "b.csv"), row.names = FALSE)
#' files <- list.files(tmp, pattern = "csv$", full.names = TRUE)
#'
#' dfs <- read_df_ueb(files, type_file = "csv", sep = ",", dec = ".")
#' dfs[[1]]
#'
#' # Creación de la columna de identificador
#' dfs_id <- read_df_ueb(files, type_file = "csv", sep = ",", dec = ".",
#'                       id_col = "id")
#' dfs_id[[2]]
#' unlink(tmp, recursive = TRUE)
#'
#' @keywords read import excel csv sav
#'
#' @importFrom readxl excel_sheets read_excel
#' @importFrom haven read_sav
#' @importFrom data.table fread
#' @importFrom janitor clean_names remove_empty make_clean_names
#' @importFrom stats na.omit
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
read_df_ueb <- function(
  l_files,
  type_file = c('excel', 'csv', 'sav'),
  sep = NULL,
  dec = NULL,
  skip_rows = 0,
  user_na = TRUE,
  na_strings = NULL,
  clean_names = TRUE,
  rm_x_cols = FALSE,
  id_col = NULL,
  id_sheet = FALSE,
  id_step = 100)
{

### 1st double check
  if (!is.character(l_files) || length(l_files) == 0) {
    
    message("No s'han trobat llistat d'arxius: ", l_files)
  
  }

### list
  l_df <- list()

### loop #01
  for(i in seq_along(l_files)) {

  ### argument 'type_file'
    if(type_file == 'excel') {
      
      ### Obtenint info sobre totes les fulles i arxius 'excel'
        l_sheet <- readxl::excel_sheets(l_files[[i]])

        df <- lapply(l_sheet, 
          function(x) readxl::read_excel(l_files[[i]], sheet = x, skip = skip_rows,
            na = if(is.null(na_strings)) '' else na_strings))

      ### Assigna noms a les matrius
        names(df) <- l_sheet

    } else if(type_file == 'csv') {
    
    ### Argumnent per als NA
      if (is.null(na_strings)) {
        csv_na_strings <- c('', 'NA')
      } else {
        csv_na_strings <- na_strings
      }

    ### getting info about all 'csv' files

      df <- lapply(l_files[[i]],
        data.table::fread,
          sep = sep,
          dec = dec,
          skip = skip_rows,
          na.strings = csv_na_strings)

    } else if(type_file == 'sav') {

    ### reading 'sav' files
      df <- lapply(
        l_files[[i]],
        haven::read_sav,
        user_na = user_na)
    }

  ### save list
    l_df[[length(l_df) + 1]] = df

  }

### unlist()
   l_df <- unlist(l_df, recursive = FALSE)


### apply 'clean_names'
### apply 'remove_empty_cols'
  if (clean_names == TRUE && length(l_df) > 0) {
    
    for(i in seq_along(l_df)){

      l_df[[i]] <- l_df[[i]] |>
        janitor::clean_names(case = 'snake') |> 
        janitor::remove_empty(which = 'cols')
    }
  }

### apply 'rm_x_cols'
  if(isTRUE(rm_x_cols)) {

    for(i in seq_along(l_df)){

      l_df[[i]] <- l_df[[i]] |>
        select(
          -dplyr::starts_with('...', ignore.case = TRUE),
          -dplyr::matches('^x[0-9]+$', ignore.case = TRUE))
    }
  }

### apply 'id_sheet'
  if(isTRUE(id_sheet)) {

    if(is.null(id_col)) stop("'id_sheet = TRUE' necessita 'id_col'.")

  ### noms de fulla (fallback per a csv/sav, que no en tenen)
    sheet_nm <- names(l_df)
    if(is.null(sheet_nm)) sheet_nm <- rep('', length(l_df))
    sheet_nm[sheet_nm == ''] <- paste0('df', seq_along(l_df))[sheet_nm == '']
    sheet_nm <- janitor::make_clean_names(sheet_nm)

    for(i in seq_along(l_df)){

      col_found <- match(tolower(id_col), tolower(names(l_df[[i]])))
      first_idx <- na.omit(col_found)[1]
      col_found <- names(l_df[[i]])[first_idx]

      if(!is.na(col_found)) {

        offset <- id_step * i
        id_num <- suppressWarnings(as.numeric(l_df[[i]][[col_found]]))

        l_df[[i]] <- l_df[[i]] |>
          dplyr::mutate(
            id_orig  = .data[[col_found]],
            id_num   = ifelse(id_num < offset, id_num + offset, id_num),
            id_sheet = sheet_nm[i])
      }
    }
  }

### apply 'id_cols'
  if(!is.null(id_col)) {

    for(i in seq_along(l_df)){

      col_found <- match(tolower(id_col), tolower(names(l_df[[i]])))
      first_idx <- na.omit(col_found)[1]
      col_found <- names(l_df[[i]])[first_idx]

      if(!is.na(col_found)) {

      ### amb 'id_sheet' es fa servir l'id homogeneïtzat i el nom de fulla
        col_use <- if(isTRUE(id_sheet)) 'id_num' else col_found
        prefix  <- if(isTRUE(id_sheet)) paste0('id_', sheet_nm[i], '_') else 'id_'

        max_value  <- max(as.numeric(l_df[[i]][[col_use]]), na.rm = TRUE) 
        n_digits <- if(is.infinite(max_value) || is.na(max_value)) 3 else nchar(as.character(max_value))

      l_df[[i]] <- l_df[[i]] |> 
        dplyr::group_by(
          .data[[col_use]]) |>
        dplyr::mutate(
          .sufix = stringi::stri_pad(as.character(dplyr::row_number()), width = 2, side = 'left', pad = '0'),
        id = paste0(
          prefix, stringi::stri_pad(as.character(.data[[col_use]]), width = n_digits, side = 'left', pad = '0'), '_', .sufix)) |> 
        dplyr::ungroup() |> 
        dplyr::select(-.sufix) |> 
        tidyr::separate(
            col = id,
            into = c('id_ori', 'event'),
            sep = '_(?=[^_]+$)',
            remove = FALSE,
            extra = 'drop',
            fill = 'right') |> 
        dplyr::relocate(starts_with('id'), event)
      }
    }
  }

### return
  return(l_df)
}