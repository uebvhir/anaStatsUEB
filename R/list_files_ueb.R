#' @title Llista arxius de diferents carpetes de dades.
#' @description Llista arxius de diferents carpetes de dades donat un 'home_folder'.
#' @param home_folder Objecte R amb la ruta a les carpetes de dades.
#' @param pattern Carrega un format d'extensió específic entre 'xlsx', 'xls' i 'csv'.
#' @param recursive Lògic (TRUE per defecte). Llista els arxius de forma recursiva.
#'
#' @return Una llista d'arxius estructurada per 'carpetes' anomenades.
#' 
#' @rdname list_files_ueb
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
