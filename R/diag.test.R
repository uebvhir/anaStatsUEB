#' @title Medidas de clasificación de un test diagnóstico
#'
#' @description
#' Calcula las medidas de rendimiento diagnóstico (exactitud, sensibilidad,
#' especificidad, valores predictivos, razones de verosimilitud y prevalencia),
#' con sus intervalos de confianza al 95\%, comparando una variable predicha con
#' la variable de referencia. Admite una única variable predicha o un data frame
#' con varias.
#'
#' @param pred Factor con la variable predicha, o data frame con varias variables
#'   predichas (una por columna). Los niveles deben estar incluidos en los de
#'   \code{y}.
#' @param y Factor con la variable de referencia (valor real).
#' @param tag.healthy Valor de \code{y} que codifica a los individuos sanos. Por
#'   defecto, el primer nivel de \code{y} (\code{levels(y)[1]}).
#' @param nround Número entero de decimales con los que se redondean los valores
#'   de la tabla resumen. Por defecto, 2.
#'
#' @return Lista con dos elementos:
#' \describe{
#'   \item{variable}{Lista con un elemento por cada variable de \code{pred}.
#'     Cada elemento contiene \code{table} (tabla 2x2 de la predicción frente a
#'     la referencia), \code{positive.class} (nivel considerado resultado
#'     positivo) y un data frame, con el nombre de la variable, con las columnas
#'     \code{Value}, \code{IC low 95\%} e \code{IC up 95\% } y una fila por medida.}
#'   \item{summary}{Lista con \code{positive.class.all} (clase positiva común a
#'     todas las variables) y \code{table} (matriz de texto con el formato
#'     \dQuote{valor (IC inferior; IC superior)} por medida y variable).}
#' }
#'
#' @details
#' Los niveles de \code{pred} e \code{y} se reordenan de modo que
#' \code{tag.healthy} quede en último lugar. Por tanto, la clase positiva es el
#' nivel distinto de \code{tag.healthy}. Las medidas se calculan con
#' \code{epiR::epi.tests()} y se contrastan con \code{caret::confusionMatrix()}.
#'
#' Exactitud, sensibilidad, especificidad, valores predictivos y prevalencia se
#' expresan en porcentaje. Las razones de verosimilitud (\code{LRpositive} y
#' \code{LRnegative}) se muestran sin escalar.
#'
#' Si \code{pred} es un único vector, el nombre de la variable en el resultado es
#' el texto de la expresión pasada al argumento. Si es un data frame, se usan los
#' nombres de sus columnas. La función se detiene con un error si \code{y} no es
#' un factor, si los niveles de \code{pred} no están en \code{y}, o si las
#' variables de \code{pred} tienen distinta clase positiva.
#'
#' @seealso \code{\link{doROC}}, \code{\link[epiR]{epi.tests}},
#'   \code{\link[caret]{confusionMatrix}}
#'
#' @examples
#' aSAH <- pROC::aSAH
#' pred <- factor(ifelse(aSAH$s100b >= 0.3, "Poor", "Good"), levels = levels(aSAH$outcome))
#'
#' # Una variable predicha
#' res <- diag.test(y = aSAH$outcome, pred = pred, tag.healthy = "Good")
#' res$summary$table
#'
#' # Varias variables predichas
#' df_pred <- data.frame(
#'   p1 = pred,
#'   p2 = factor(ifelse(aSAH$s100b >= 0.1, "Poor", "Good"), levels = levels(aSAH$outcome))
#' )
#' diag.test(y = aSAH$outcome, pred = df_pred, nround = 1)$summary$table
#'
#' @keywords roc glm test classification diagnostic sensitivity specificity
#'
#' @import epiR caret e1071
#'
#' @author
#' Miriam Mota \email{mmota.foix@@gmail.com}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
diag.test <- function(pred, y, tag.healthy = levels(y)[1] , nround = 2){
  if (class(y)[length(class(y))] != "factor")
    stop("La variable 'y' debe ser factor")
  n_var <- ifelse(is.null(ncol(pred)), 1, ncol(pred))
  sum_ac_l <- list()
  classification <- list()
  pos.class <- NA
  for (i in 1:n_var) {
    if (n_var == 1) {
      name_var <- deparse(substitute(pred))
      pred_i <- pred
    }
    else {
      name_var <- names(pred)[i]
      pred_i <- pred[, i]
    }
    classification[["variable"]][[name_var]] <- list()
    if (!all(levels(pred_i) %in% levels(y)))
      stop("\n ERROR: los niveles de las variables", name_var,
           " e 'y' deben ser los mismos \n ")
    pred_i <- factor(pred_i, c(levels(pred_i)[levels(pred_i) !=
                                                tag.healthy], tag.healthy))
    y <- factor(y, c(levels(y)[levels(y) != tag.healthy],
                     tag.healthy))
    tab2test <- table(pred_i, y)
    classification[["variable"]][[name_var]][["table"]] <- tab2test
    epiRes <- epi.tests(tab2test)
    if (epiRes$detail %>% dplyr::filter(statistic == "se") %>% dplyr::select(est) == confusionMatrix(table(pred_i,
                                                                                             y))$byClass[["Sensitivity"]]) {
      positive <- confusionMatrix(table(pred_i, y))$positive
      classification[["variable"]][[name_var]][["positive.class"]] <- positive
      pos.class[i] <- positive
    }
    else {
      stop("Error: Problemas calculo diag test! ")
    }
    if (!identical(rownames(tab2test), colnames(tab2test)))
      stop("Error en l'ordre de les variables")
    ll <- list()
    ll[["Accuracy"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "diag.ac") %>% dplyr::select(-statistic))) *
      100
    ll[["Sensitivity"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "se") %>% dplyr::select(-statistic))) *
      100
    ll[["Specificity"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "sp") %>% dplyr::select(-statistic))) *
      100
    ll[["PPV"]] <-  unlist(c(epiRes$detail %>% dplyr::filter(statistic == "pv.pos") %>% dplyr::select(-statistic))) * 100
    ll[["NPV"]] <- c(unlist(c(epiRes$detail %>% dplyr::filter(statistic == "pv.neg") %>% dplyr::select(-statistic)))) * 100
    ll[["LRpositive"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "lr.pos") %>% dplyr::select(-statistic)))
    ll[["LRnegative"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "lr.neg") %>% dplyr::select(-statistic)))
    ll[["Prevalence"]] <- unlist(c(epiRes$detail %>% dplyr::filter(statistic == "tp") %>% dplyr::select(-statistic))) *
      100
    res <- data.frame(matrix(unlist(ll), nrow = length(ll),
                             byrow = T), stringsAsFactors = FALSE)
    rownames(res) <- names(ll)
    colnames(res) <- c("Value", "IC low 95%", "IC up 95% ")
    classification[["variable"]][[name_var]][[name_var]] <- res
    sum_ac_l[[name_var]] <- apply(res, 1, function(x) paste0(round(x[1],
                                                                   nround), " (", round(x[2], nround), "; ", round(x[3],
                                                                                                                   nround), ")"))
  }
  classification[["summary"]] <- list()

  ## comprovem que per a totes les variables la positive.class (es a dir, els 'casos') sigui sempre el mateix
  if (length(unique(pos.class)) == 1) {
    classification[["summary"]][["positive.class.all"]] <- unique(pos.class)
  }
  else {
    stop("Existen distintas 'positive.class'")
  }
  sum_ac <- data.frame(matrix(unlist(sum_ac_l), nrow = length(sum_ac_l),
                              byrow = T), stringsAsFactors = FALSE)
  rownames(sum_ac) <- names(sum_ac_l)
  colnames(sum_ac) <- names(sum_ac_l[[1]])
  sum_ac <- t(sum_ac)
  classification[["summary"]][["table"]] <- sum_ac
  return(classification)
}
