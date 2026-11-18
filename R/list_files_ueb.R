#' @title Listar archivos de varias carpetas de datos
#'
#' @description
#' Lista los archivos de datos (por extensión) de las carpetas que hay dentro de
#' una carpeta de partida (\code{home_folder}) y los devuelve organizados por
#' carpeta.
#'
#' @param home_folder Cadena de texto con la ruta de la carpeta de datos. Debe
#'   existir; si no, la función produce un error.
#' @param recursive Lógico. Si es \code{TRUE} (por defecto), se listan todas las
#'   carpetas de forma recursiva, incluida la propia \code{home_folder}. Si es
#'   \code{FALSE}, solo se listan las subcarpetas directas (véase
#'   \sQuote{Details}).
#' @param pattern Vector de caracteres con las extensiones de archivo a buscar,
#'   sin punto. Por defecto, \code{c("xlsx", "xls", "csv", "sav")}. Si alguna
#'   extensión no es una de esas cuatro, se emite un aviso, pero se utiliza igual.
#'
#' @return
#' Lista con nombre (el nombre de cada carpeta) de vectores de caracteres con la
#' ruta completa de los archivos encontrados. Una carpeta sin archivos da
#' \code{character(0)}. Si no se encuentra ninguna carpeta, devuelve una lista
#' vacía y muestra un mensaje.
#'
#' @details
#' Para cada carpeta se buscan los archivos con \code{list.files(recursive =
#' TRUE)}. Por ello:
#' \itemize{
#'   \item con \code{recursive = TRUE}, el primer elemento de la lista corresponde
#'     a \code{home_folder} y contiene todos los archivos, y cada archivo aparece
#'     también en todas las carpetas que lo contienen (por ejemplo, un archivo de
#'     \code{A/sub} aparece en \code{home_folder}, en \code{A} y en \code{sub};
#'   \item con \code{recursive = FALSE}, solo se listan las subcarpetas directas
#'     (cada una con todos sus archivos, incluidos los de sus subcarpetas) y los
#'     archivos que están directamente en \code{home_folder} no aparecen;
#'   \item los nombres de la lista son \code{basename()} de cada carpeta, por lo
#'     que pueden repetirse si hay carpetas con el mismo nombre en rutas
#'     distintas.
#' }
#' Los mensajes y errores de la función están en catalán.
#'
#' @seealso \code{\link[base]{list.files}}, \code{\link[base]{list.dirs}}
#'
#' @examples
#' root <- file.path(tempdir(), "datos")
#' dir.create(file.path(root, "A"), recursive = TRUE)
#' dir.create(file.path(root, "B"))
#' file.create(file.path(root, c("A/a1.xlsx", "A/a2.txt", "B/b1.csv")))
#'
#' # Todas las carpetas, de forma recursiva
#' list_files_ueb(root)
#'
#' # Solo archivos .csv
#' list_files_ueb(root, pattern = "csv")
#'
#' @keywords files folders list import
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
list_files_ueb <- function(
    home_folder,
    recursive = TRUE,
    pattern = c('xlsx', 'xls', 'csv', 'sav'))
{
### 1st double check
	if (!dir.exists(home_folder)) {
    
		stop("Error: 'home_folder' no es un directori valid o no existeix. Si us plau, verifica la ruta.")
  
	}

### 2nd double check
	valid_patterns <- c('xlsx', 'xls', 'csv', 'sav')

	if (!all(pattern %in% valid_patterns)) {
    
		warning("Advertencia: el 'pattern' especificat no es l'extensio dels arxius. Si us plau, assegura que siguin correctes.")
  
	}

### Llista directoris
  l_dir <- list.dirs(
  	path = home_folder,
  	recursive = recursive,
		full.names = TRUE)

### 3rd double check
	if (length(l_dir) == 0) {

    message("No s'han trobat subdirectoris: ", home_folder)
  
	  return(list())
  
	}

### Crea un patró de cerca regex
  l_pattern <- paste0('\\.(', paste(pattern, collapse = '|'), ')$', sep = '')

### Llista els arxius
	l_files <- list()

	for(i in seq_along(l_dir)) {

  	file <- list.files(
  		path = l_dir[i],
  		pattern = l_pattern,
  		full.names = TRUE,
  		recursive = TRUE)

  	### save in a list
  		l_files[[length(l_files) + 1]] = file

	}

### Nom de la llista
	names(l_files) <- basename(l_dir)

### Rerornar la llista
	return(l_files)
}
