#' @title Tabla resumen descriptiva de un data frame
#'
#' @description
#' Genera una tabla resumen de un conjunto de datos con metadatos clave: tipos de
#' variable, etiquetas, número de niveles (para factores), rangos de valores (para
#' numéricas) y datos faltantes. Resalta en rojo las variables problemáticas según
#' el número de niveles o el porcentaje de valores faltantes.
#'
#' @param data Un data frame a resumir.
#' @param format Formato de salida: \code{"html"} o \code{"latex"}. Se usa para los
#'   saltos de línea y el renderizado de la tabla. Por defecto, \code{"html"}. Con
#'   otros valores el comportamiento no está definido.
#' @param maxlev Número máximo de niveles que se muestran para las variables
#'   factor. Los factores con más niveles se resaltan en rojo. Por defecto, 7.
#' @param maxNA Umbral (en \%) para resaltar en rojo las variables con un
#'   porcentaje de valores faltantes estrictamente superior. Por defecto, 80.
#' @param size Tamaño de letra de la tabla (para \code{kable}). Por defecto, 13.
#' @param caption Texto de leyenda. Actualmente no se utiliza: la función siempre
#'   genera la leyenda, con el número de variables y de observaciones, \code{maxlev}
#'   y \code{maxNA}.
#' @param remove_cols Lógico. Actualmente no se utiliza: las columnas vacías
#'   siempre se eliminan antes de procesar (\code{janitor::remove_empty()}). Por
#'   defecto, \code{TRUE}.
#' @param df Lógico. Si es \code{TRUE}, devuelve el resumen como un data frame. Si
#'   es \code{FALSE} (por defecto), devuelve una tabla formateada.
#'
#' @return Una tabla con estilo \code{kable} (objeto \code{kableExtra}) o, si
#'   \code{df = TRUE}, un data frame con las columnas \code{nms}, \code{clss},
#'   \code{lbl} (solo si alguna variable tiene etiqueta), \code{mm_lev} y
#'   \code{compl_mis}.
#'
#' @details
#' La tabla tiene una fila por variable y las columnas: \code{Variable},
#' \code{Type}, \code{Description} (solo si hay etiquetas), \code{[Min,Max] or
#' Levels} y los casos válidos y faltantes, con su porcentaje.
#'
#' El rango se calcula para variables numéricas, enteras y de fecha; para los
#' factores se muestran el número de niveles y los primeros \code{maxlev}; las
#' variables de texto muestran \code{"-"}. La función contempla los tipos
#' \code{logical}, \code{factor}, \code{numeric}, \code{integer}, \code{Date},
#' \code{POSIXt} y \code{character}.
#'
#' @seealso \code{\link{NAperc}}, \code{\link{desc_group}},
#'   \code{\link[janitor]{remove_empty}}
#'
#' @examples
#' desc_data(data = iris, format = "html")
#' desc_data(airquality, format = "html", maxNA = 20)
#'
#' # Como data frame
#' desc_data(airquality, df = TRUE)
#'
#' @keywords descriptive summary missing data-frame
#'
#' @importFrom Hmisc label
#' @importFrom kableExtra cell_spec kable kable_styling
#' @importFrom janitor remove_empty
#'
#' @author
#' Miriam Mota-Foix \email{mmota.foix@gmail.com}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
desc_data <- function(data,
                format = "html",
                maxlev = 7,
                maxNA = 80,
                size = 13,
                caption = NULL,
                remove_cols = TRUE,
                df = FALSE) {

  new_line <- switch(format, "html" = " <br> ", "latex" = " \\\\ " ) #, "R" = " \n ")
  caption <- paste0("Summary Data.", new_line,
                    "N variables: ", ncol(data), ".  N observaciones: ", nrow(data), new_line,
                    "Max level: ", maxlev, ".  Max NA: ", maxNA, "%")

  ## eliminem columnes buides
  data <- remove_empty(data, which = "cols")

  ## creacio de les diferents columnes
  nms <- names(data)

  clss <- sapply(data, function(x) class(x)[length(class(x))])

  lbl <- Hmisc::label(data,self = F)

  mm_lev <- unlist(lapply(data, function(x) {
    class_x <- class(x)[length(class(x))]
    switch(class_x,
           logical = paste(unique(x), collapse = ", "),
           factor = paste("N LEVELS =", length(levels(x)), new_line,
                          paste(1:min(maxlev, length(levels(x))),
                                ":", levels(x)[1:min(maxlev, length(levels(x)))],
                                collapse = new_line)),
           numeric = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           integer = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           Date = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           POSIXt = paste0("[", min(x, na.rm = T), ", ", max(x, na.rm = T), "]"),
           character = "-")
  }))

  compl_mis <- apply(data,2,function(x) paste0(sum(complete.cases(x))," (", round((sum(complete.cases(x))/length(x))*100,3) , "%)", new_line,
                                               sum(is.na(x))," (", round((sum(is.na(x))/length(x))*100,3) , "%)"))

  #informacio necesaria per a resaltar
  nlev <- sapply(data, function(x)length(levels(x) ))
  percNA <- apply(data,2,function(x)sum(is.na(x))/length(x)*100 )


  # Creacio data frame final
  df_res <- as.data.frame( cbind(nms,clss,lbl, mm_lev, compl_mis))
  colnames <- c("Variable",
                      "Type",
                      "Description",
                      "[Min,Max] or Levels",
                      paste0("Valid cases, n (%)",new_line," Missings, n (%)"))


  # Eliminació de columna en cas de que no hi hagi labels
  if (length(table(df_res$lbl)) == 1) {
    df_res <- df_res[,-which(names(df_res) %in% "lbl")]
    colnames <- colnames[!colnames %in% "Description"]
  }

  # Informació Final
  if(!df) {
    df_res  %>%
    mutate(
      mm_lev = cell_spec(mm_lev, color = ifelse(nlev > maxlev, "red", "black"), escape = F),
      compl_mis = cell_spec(compl_mis, color = ifelse(percNA > maxNA, "red", "black"), escape = F)
    ) %>%
    kable(format = format, booktabs = T,  col.names = colnames, caption = caption, longtable = TRUE, escape = F, row.names= FALSE) %>%
    kable_styling(latex_options = c("striped","hold_position", "repeat_header"),
                  font_size = size, full_width = F, position = "left")
  }else{
      return(df_res)
    }
}
