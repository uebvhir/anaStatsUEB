#' @rdname desc_mod
#' @export
tabOR_lr <- function(...) {
  .Deprecated("desc_mod") #include a package argument, too
  desc_mod(...)
}

#' @title Resumen y formato de modelos de regresión (OR, HR y coeficientes)
#'
#' @description
#' Resume y formatea modelos logísticos, lineales, de supervivencia (coxph), binomial
#' negativa y clogit, tanto univariados como multivariados. Devuelve una tabla con la
#' estimación (Odds Ratio, Hazard Ratio o coeficiente según el modelo), su intervalo
#' de confianza y el p-valor, o bien una tabla con formato \code{kable}.
#'
#' @param mod Objeto ajustado de clase \code{glm}, \code{clogit}, \code{coxph},
#'   \code{negbin} o \code{lm}. La estimación mostrada es \emph{Odds Ratio} (glm),
#'   \emph{Hazard Ratio} (clogit, coxph) o \emph{Estimate} (lm, negbin).
#' @param xtab TRUE o FALSE. Si es \code{TRUE}, devuelve una tabla con formato
#'   \code{kable} (mediante \code{kable_ueb()}) en lugar de un data frame. Por defecto
#'   \code{FALSE}.
#' @param title Vector de caracteres con el título o leyenda de la tabla. Solo se usa si
#'   \code{xtab = TRUE}. Por defecto \code{"Model summary"}.
#' @param font_size Tamaño de letra de la tabla. Actualmente no se utiliza. Por defecto 13.
#' @param show.pretty TRUE o FALSE. Si es \code{TRUE}, muestra las etiquetas (\code{label})
#'   de las variables y separa el nombre de la variable de su nivel. Solo funciona para
#'   \code{lm} y \code{glm}. Por defecto \code{FALSE}.
#' @param group_rw TRUE o FALSE. Si es \code{TRUE}, agrupa las filas por variable
#'   (solo con \code{xtab = TRUE}). Por defecto \code{FALSE}.
#' @param show.intcp TRUE o FALSE. Indica si se muestra el intercepto del modelo; en
#'   ambos casos el modelo se ha calculado con intercepto. Por defecto \code{FALSE}.
#' @param show.n TRUE o FALSE. Muestra el total de individuos usados para el ajuste del
#'   modelo (columna \code{N}). Por defecto \code{TRUE}.
#' @param show.aov.pval TRUE o FALSE. Pretende mostrar el p-valor del modelo global
#'   (columna \code{"P-value (Global)"}). Actualmente esa columna no se calcula, por lo
#'   que este argumento no tiene efecto. Por defecto \code{TRUE}.
#' @param row.names TRUE o FALSE. Muestra o no los nombres de fila en la tabla con
#'   formato \code{kable} (\code{xtab = TRUE}). Por defecto \code{FALSE}.
#' @param digits Vector numérico con el número de decimales a mostrar. Se aplica con
#'   \code{show.pretty = TRUE} y, con \code{xtab = TRUE} y \code{group_rw = TRUE}, a la
#'   tabla con formato. Por defecto 2.
#'
#' @return Con \code{xtab = FALSE}, un data frame con las columnas \code{Variable},
#'   la estimación (\code{Odds Ratio}, \code{Hazard Ratio} o \code{Estimate}),
#'   \code{CI (lower)}, \code{CI (upper)}, el p-valor y \code{N}. Con
#'   \code{xtab = TRUE}, una tabla con formato \code{kable} (\code{kable_ueb()}).
#'
#' @details
#' La función \code{tabOR_lr()} está obsoleta (deprecated) y se mantiene solo por
#' compatibilidad: llama a \code{desc_mod()} con los mismos argumentos. Utilice
#' \code{desc_mod()}.
#'
#' @seealso \code{\link{kable_ueb}}, \code{\link{desc_multimod}}, \code{\link{desc_unimods}}
#'
#' @import xtable dplyr papeR
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(
#'   y = factor(rep(c("No", "Si"), each = 20)),
#'   x = rnorm(40),
#'   g = factor(sample(c("A", "B"), 40, replace = TRUE))
#' )
#' mod <- glm(y ~ x + g, data = dat, family = binomial)
#' desc_mod(mod)
#' desc_mod(mod, show.pretty = TRUE)
#'
#' @keywords OR regresion logistica
#'
#' @author
#' Miquel Vázquez-Santiago \email{miquel.vazquez@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
desc_mod <- function(mod,
                     xtab = FALSE,
                     title = "Model summary",
                     font_size = 13,
                     show.pretty = FALSE,
                     group_rw = FALSE,
                     show.intcp = FALSE,
                     show.n = TRUE,
                     show.aov.pval = TRUE,
                     row.names = F, digits = 2) {


  type_mod <-  switch(class(mod)[1],
                      glm = 'Odds Ratio',
                      clogit = 'Hazard Ratio',
                      lm = "Estimate",
                      coxph = "Hazard Ratio",
                      negbin = "Estimate")
  pret_mod <- papeR::prettify(summary(mod))
  names(pret_mod)[names(pret_mod) == " "] <- "Variable"
  res <- pret_mod[, c("Variable", type_mod, "CI (lower)", "CI (upper)", grep("Pr", names(pret_mod), value = T) ) ]
  rownames(res) <- res$Variable




  if (!show.intcp) {
    res <- res %>% dplyr::filter(Variable != "(Intercept)" )
  }

  #P.valueGlobal
  # res[1,"P-value (Global)"] <- na.omit(anova(mod,test = "Chisq")$Pr)[1] ## INCORRECTE
  #N total
  res[1,"N"] <- nobs(mod)
  # colnames(tauORcoef) <- c("OR", "LowerIC", "UpperIC", "P-value", "P-value (Global)", "N")




  if (!show.n) {  res <- res[,!names(res) %in% ("N")]  }

  if (!show.aov.pval) {    res <- res[,!names(res) %in% ("P-value (Global)")]  }


  if (show.pretty) {
    if (class(mod)[1] == "glm" | class(mod)[1] == "lm") {
      vars_mod <- get.vars(alias(mod)$Model)[-1]
      Hmisc::label(mod$model, self = F)[Hmisc::label(mod$model) == ""] <- names(mod$model)[Hmisc::label(mod$model) == ""]
      # label_var <-  Hmisc::label(mod$model[,res$vars_name]) # Hmisc::label(mod$model)[-1]
    }else{
      vars_mod <- attr(terms(mod),"term.labels")
    }

    matches <- stringr::str_c(vars_mod, collapse = "|")
    vars_name <- stringr::str_extract_all(res$Variable, matches, simplify = T)[,1]
    res <- tibble::add_column(res,vars_name,.before = "Variable")

    if (class(mod)[1] == "glm" | class(mod)[1] == "lm") {
      vars_label <-  Hmisc::label(mod$model[,res$vars_name])
      if (show.intcp) vars_label <- c("Intercept",vars_label)
      res <- tibble::add_column(res,vars_label,.before = "Variable")
    }
    levs <- stringr::str_replace_all(res$Variable,vars_name,"")
    res <- tibble::add_column(res,levs,.before = "Variable")

    res <- res %>% dplyr::select(-Variable,-vars_name)
    res <- res %>%
      mutate_if(is.numeric, round,digits)

  }

  if (xtab) {
    if (group_rw) {
      kable_ueb(res[,!names(res) %in% c("var_name", "vars_label")],
                caption = title, row.names = row.names, digits = digits) %>%
        kableExtra::group_rows(index = table(res$vars_label)[unique(as.character(res$vars_label))])
    }else{
      kable_ueb(res, caption = title, row.names = row.names)
    }

  } else {
    return(res)
  }
}



