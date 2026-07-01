#' A freq_table Function
#'
#' Tabla de frecuencias relativas, por fila columnas o global.
#' @param data data frame que contiene las variables a analizar
#' @param var.cat vector con el nombre de las variables categoricas a resumir
#' @param y nombre de la variable factor principal.
#' @param margin index, or vector of indices to generate margin for
#' @param nround integer indicating the number of decimal places (round).
#' @param restab TRUE o FALSE, para obtener tabla en formato .tex or .html
#' @param title titles for the table
#' @param xtab.type A character string. Possible values are latex, html, markdown, pandoc, and rst; this will be automatically determined if the function is called within knitr; it can also be set in the global option knitr.table.format. If format is a function, it must return a character string.
#' @export freq_table
#' @import stargazer
#' @author Miriam Mota \email{mmota.foix@@gmail.com}
#' @examples
#' # freq_table(var.cat = c('cyl','vs','gear', 'carb'), y = 'am', data = mtc_bis, restab = TRUE)
#' @keywords freq tables compare qualitative

freq_table <- function(var.cat, y,
                       data, margin = NULL,
                       nround = 2,
                       restab = TRUE,
                       title = "titles for the table",
                       xtab.type = "html") {

  data[, y] <- factor(data[, y])
  res_freq_all <- NULL
  for (i in 1:length(var.cat)) {
    data[, var.cat[i]] <- factor(data[, var.cat[i]])
    (freq_abs <- table(data[, var.cat[i]], data[, y]))
    (freq_rel <- prop.table(freq_abs, margin = margin) * 100)
    res_freq <- matrix(paste0(freq_abs, " (", round(freq_rel, nround), "%)"),
                       ncol = ncol(freq_abs))
    res_freq <- cbind(c(var.cat[i], rep("", nrow(res_freq) - 1)),
                      rownames(freq_abs),
                      res_freq)
    colnames(res_freq) <- c("Variable", "Levels", colnames(freq_abs))
    res_freq_all <- rbind(res_freq_all, rep("", ncol(res_freq)), res_freq)
  }
  if (restab) {
    kable_ueb(res_freq_all, format = xtab.type, booktabs = T,
          caption = title)
  } else {
    return(res_freq_all)
  }
}
