#' @title Read Data from Multiple File Formats
#' 
#' @description
#' Reads a list of files and returns their contents as a list of data frames.
#' Supports Excel (\code{.xls}, \code{.xlsx}), CSV, and SPSS (\code{.sav}) formats.
#' The type of file to read must be specified via \code{type_file}.
#' 
#' For Excel files, all sheets are read and returned as separate elements.
#' For CSV and SAV files, each file is read into a data frame.
#' 
#' @param l_files A character vector or list of file paths obtained from
#'   \code{amphi_list_files}.
#' @param type_file The type of file to read. One of \code{'excel'}, \code{'csv'}, or \code{'sav'}.
#' @param sep Field separator for CSV files. Default is \code{NULL} (auto-detection).
#' @param dec Decimal separator for CSV files. Default is \code{NULL} (auto-detection).
#' @param skip_rows Number of rows to skip at the beginning of the file.
#'   Applicable to Excel, CSV, and SAV.
#' @param user_na Logical. If \code{TRUE}, variables with user-defined missing values
#'   in SPSS files are returned as \code{labelled_spss()} objects. If \code{FALSE},
#'   user-defined missing values are converted to \code{NA}.
#' @param na_strings Character vector of strings to interpret as missing values.
#'   Applies to CSV and optionally to SAV files.
#' @param clean_names Logical. If \code{TRUE}, column names are cleaned using
#'   \code{janitor::clean_names()} and empty columns are removed.
#' 
#' @return
#' A list of data frames. For Excel files, each sheet is returned as a separate element.
#' 
#' @details
#' \itemize{
#'   \item Excel files are read using \code{readxl::read_excel()}.
#'   \item CSV files are read using \code{data.table::fread()}.
#'   \item SAV files are read using \code{haven::read_sav()}, preserving labelled data.
#'   \item When \code{clean_names = TRUE}, column names are standardized to snake_case
#'         and empty columns are removed.
#' }
#' 
#' @importFrom readxl excel_sheets read_excel
#' @importFrom haven read_sav
#' @importFrom data.table fread
#' @importFrom janitor clean_names remove_empty_cols
#' 
#' @examples
#' \dontrun{
#' # Read Excel files
#' read_df(files, type_file = "excel")
#' 
#' # Read CSV files
#' read_df(files, type_file = "csv", sep = ",", dec = ".")
#' 
#' # Read SPSS files
#' read_df(files, type_file = "sav", user_na = TRUE)
#' }
#' 
#' @rdname read_df_ueb
#' @export
read_df_ueb <- function(
  l_files,
  type_file = c('excel', 'csv', 'sav'),
  sep = NULL,
  dec = NULL,
  skip_rows = 0,
	user_na = TRUE,
  na_strings = NULL,
  clean_names = TRUE) {

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
					function(x) readxl::read_excel(l_files[[i]], sheet = x, skip = skip_rows))

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

### return
	return(l_df)
}