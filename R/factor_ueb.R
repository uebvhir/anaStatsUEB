#' @title Convertir un vector en factor conservando la etiqueta original
#'
#' @description
#' Codifica un vector como factor conservando la etiqueta de la variable
#' (\code{Hmisc::label}). Permite asignar niveles y etiquetas, o reordenar los
#' niveles de un factor existente, y avisa de los valores que se convierten en
#' \code{NA}.
#'
#' @param x Vector de datos (numérico, de texto o factor), normalmente con pocos
#'   valores distintos.
#' @param levs Vector con los valores de \code{x} que se codifican como niveles.
#'   Solo se utiliza si también se indica \code{labs}; si \code{labs} es
#'   \code{NULL}, se ignora.
#' @param labs Vector de caracteres con las etiquetas de los niveles. Con
#'   \code{levs}, se asignan en el mismo orden que \code{levs}. Sin \code{levs},
#'   \code{x} debe ser un factor y \code{labs} debe contener exactamente sus
#'   niveles, en el orden deseado: solo se reordena el factor.
#' @param del.empty.val Lógico. Si es \code{TRUE} (por defecto), elimina los
#'   niveles sin casos.
#' @param name.var Nombre de la variable que se usa en los mensajes. Por
#'   defecto, la parte derecha del \code{$} (por ejemplo, con
#'   \code{mtcars$mpg} se usa \code{"mpg"}).
#' @param write_changes Lógico. Si es \code{TRUE} (por defecto), cuando se indican
#'   \code{levs} y \code{labs} y hay valores de \code{x} que no están en
#'   \code{levs}, se muestra un mensaje con \code{desc_changes()} y se emite un
#'   aviso (\code{warning}), porque esos valores pasan a ser \code{NA}.
#' @param write_recod Lógico. Si es \code{TRUE} (por defecto), cuando se indican
#'   \code{levs} y \code{labs}, se muestra con \code{desc_changes()} la
#'   recodificación de los valores originales a las etiquetas.
#'
#' @return Un factor con la misma longitud que \code{x} y la etiqueta
#'   (\code{Hmisc::label}) original de \code{x}.
#'
#' @details
#' El comportamiento depende de los argumentos indicados:
#' \itemize{
#'   \item \code{levs} y \code{labs}: equivale a
#'     \code{factor(x, levels = levs, labels = labs)}. Los valores de \code{x}
#'     que no están en \code{levs} pasan a \code{NA}.
#'   \item Solo \code{labs}: se reordenan los niveles de \code{x} según
#'     \code{labs}. Se produce un error si \code{x} no es un factor o si
#'     \code{labs} no contiene exactamente los niveles de \code{x}.
#'   \item Ninguno de los dos, o solo \code{levs}: equivale a \code{factor(x)}.
#' }
#' Si \code{del.empty.val} es \code{TRUE}, se aplica \code{droplevels()} al
#' resultado.
#'
#' @seealso \code{\link[base]{factor}}, \code{\link{desc_changes}},
#'   \code{\link{desc_group}}
#'
#' @examples
#' # Crear variable
#' variable <- c(0, 0, 0, 1, 1, 1, 0, "a", 1, 0, "B")
#'
#' # Factor automatico
#' factor_ueb(x = variable)
#'
#' # Asignar niveles y etiquetas
#' (var_factor <- factor_ueb(variable, levs = c(0, 1, 2), labs = c("No", "Si", "NSNC")))
#'
#' # Reordenar un factor
#' factor_ueb(x = var_factor, labs = c("Si", "No"))
#'
#' var_factor2 <- factor(variable)
#' var_factor2[var_factor2 == "B"] <- NA
#' factor_ueb(var_factor2, labs = c("1", "a", "B", "0"))
#'
#' # Otros ejemplos
#' variable <- c(0, 0, 0, 1, 1, 1, 0, 10, 1, 0)
#' factor_ueb(variable)
#' factor_ueb(variable, levs = c(0, 1), labs = c("No", "Si"))
#'
#' variable <- c(0, 0, 0, 1, 1, 1, 0, 1, 1, 0, NA)
#' factor_ueb(variable)
#' factor_ueb(variable, levs = c(0, 1), labs = c("No", "Si"))
#'
#' @keywords factor variable class levels labels
#'
#' @import Hmisc
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
factor_ueb <- function(x, levs = NULL, labs = NULL, del.empty.val = TRUE, name.var = NULL,write_changes = T, write_recod= T){

  lab_var <- Hmisc::label(x)
  name_sep <- strsplit(deparse(substitute(x)), "$",fixed = T)[[1]]
  name.var <- ifelse(is.null(name.var), name_sep[length(name_sep)], name.var)
  unique_noNA <- unique(x)[!unique(x) %in% NA]

  if(!is.null(levs) & !is.null(labs)){
    levels_dif <- unique_noNA  %in% levs
    if (!all(levels_dif)) {
      mss <- paste0("Los individuos con valor '", paste(unique_noNA[!levels_dif], collapse = "', '"),
                    "' para la variable ",name.var," han sido considerados NA \n")
      if(write_changes) {
        desc_changes(mss, col = "#ff6600")
        warning(mss, call. = FALSE)

      }
    }
    if(write_recod) {desc_changes(paste0("Para la variable **",name.var,"**, los valores numéricos originales ('",paste0(levs, collapse = "', '"), "') fueron recodificados como '",
                        paste0(labs, collapse = "', '"), "' respectivamente, para facilitar la interpretación."), col = "#eb9b34")}
    var_factor <- factor(x, levels = levs, labels = labs )
  } else if (is.null(levs) & !is.null(labs)) {
    var_factor <- factor_reorder(x, labs = labs )
  } else {
    var_factor <- factor(x)
  }


  if (del.empty.val) var_factor <- droplevels(var_factor)
  Hmisc::label(var_factor) <- lab_var
  return(var_factor)
}




factor_reorder <- function(x, labs){

  if(length(labs) != length(levels(x))) stop ("There are different number of 'labs' and 'levels(x)'")
  if(!all (levels(x) %in% labs) ) stop ("'levels(x)' and 'labs' are different")

  lab_var <- Hmisc::label(x) ## guardem label
  var_factor <- factor(x, labs) ## reorder factor
  Hmisc::label(var_factor) <- lab_var # assignem de nou la label original
  if(!all.equal(as.character(x), as.character(var_factor)) ) stop("Houston! We have a problem!  Review code 'factor_reorder")
  return(var_factor)
}
