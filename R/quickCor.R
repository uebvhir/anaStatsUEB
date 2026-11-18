#' @title Correlación de Pearson y/o Spearman entre dos variables numéricas
#'
#' @description
#' Calcula la correlación entre dos variables numéricas de un data frame
#' (Pearson, Spearman o ambas), con su intervalo de confianza al 95 \%, el
#' p-valor y el número de observaciones. Opcionalmente muestra un diagrama de
#' dispersión con la recta de regresión y una tabla formateada.
#'
#' @param dat Data frame que contiene las variables. No se admite un tibble.
#' @param x,y Variables numéricas a correlacionar: nombres de columna de
#'   \code{dat}, con o sin comillas. Ninguna puede ser constante.
#' @param nround Número de decimales con los que se redondean los coeficientes y
#'   los p-valores (por defecto, 3).
#' @param main Título del gráfico. Si es \code{NULL} (por defecto), se usa
#'   \code{"<x> with <y>"}, con la etiqueta de \code{Hmisc::label} si existe.
#' @param xtab Lógico. Si es \code{TRUE} (por defecto), imprime la tabla de
#'   resultados con \code{\link{kable_ueb}}.
#' @param pos Posición de la leyenda del gráfico (por defecto,
#'   \code{"bottomleft"}). Valores: \code{"bottomright"}, \code{"bottom"},
#'   \code{"bottomleft"}, \code{"left"}, \code{"topleft"}, \code{"top"},
#'   \code{"topright"}, \code{"right"} y \code{"center"}.
#' @param sz.xtab Sin efecto en la versión actual: no se usa en el cuerpo de la
#'   función (por defecto, \code{NULL}).
#' @param method Método de correlación: \code{"pearson"} (por defecto),
#'   \code{"spearman"} o \code{"both"}. Véase \sQuote{Details}.
#' @param corplot Lógico. Si es \code{TRUE} (por defecto), dibuja el diagrama de
#'   dispersión.
#' @param cex.txt Factor de expansión del texto con el coeficiente que se escribe
#'   en el gráfico (por defecto, 0.8).
#' @param cex.main Factor de expansión del título del gráfico (por defecto, 0.8);
#'   véase \code{\link[graphics]{title}} y \code{\link[graphics]{par}}.
#' @param xtab.type Sin efecto en la versión actual: no se usa en el cuerpo de la
#'   función (por defecto, \code{"html"}).
#' @param sub Subtítulo del gráfico. También se añade a la leyenda de la tabla
#'   (por defecto, \code{NULL}).
#' @param lm.fit Lógico. Si es \code{TRUE} (por defecto), dibuja la recta de
#'   regresión lineal.
#' @param show.res Lógico. Si es \code{TRUE} (por defecto), devuelve la lista de
#'   resultados; si es \code{FALSE}, no devuelve nada (\code{NULL}).
#' @param pos.text Línea del margen (contando desde 0 hacia fuera) en la que se
#'   escribe el texto con el coeficiente (por defecto, -1.8).
#' @param show.pval Lógico. Si es \code{TRUE} (por defecto), incluye el p-valor
#'   en la tabla y en el gráfico.
#' @param prep.tab Lógico. Si es \code{TRUE}, añade a la salida el elemento
#'   \code{df_prep_tab}, pensado para las tablas de \code{desc_group_numeric}
#'   (por defecto, \code{FALSE}).
#' @param cor_cut Valor numérico a partir del cual la correlación se considera
#'   relevante (por defecto, 0.7). Véase \sQuote{Details}.
#'
#' @return Una lista con los elementos:
#' \describe{
#'   \item{\code{coeff}}{Objeto \code{summary.lm} de la regresión lineal de
#'     \code{y} sobre \code{x}.}
#'   \item{\code{methods}}{Texto \code{"Correlation coefficient"}.}
#'   \item{\code{result}}{Matriz de caracteres con una fila por método
#'     (\code{Pearson}, \code{Spearman}) y las columnas \code{rho}, \code{IC},
#'     \code{p-value} y \code{n}. Los p-valores menores de 0.001 se muestran como
#'     \code{"<0.001"}.}
#'   \item{\code{df_prep_tab}}{Solo si \code{prep.tab = TRUE}: data frame de una
#'     fila con \code{variable}, \code{levels}, \code{summary}, \code{p.value} y
#'     \code{n}.}
#'   \item{\code{select}}{Solo si \code{method = "both"} y el valor absoluto de
#'     alguna de las dos correlaciones supera \code{cor_cut}: nombre (o etiqueta)
#'     de \code{x}.}
#' }
#' Si \code{show.res = FALSE}, no se devuelve nada.
#'
#' @details
#' \itemize{
#'   \item El intervalo de confianza de Pearson procede de
#'     \code{\link[stats]{cor.test}}; el de Spearman se calcula con
#'     \code{\link[mada]{CIrho}}.
#'   \item Para \code{n} se usan los pares de observaciones sin valores perdidos.
#'   \item Se muestran las etiquetas de \code{Hmisc::label}, si existen, en lugar
#'     de los nombres de las variables.
#'   \item Se detiene con un error si \code{x} o \code{y} no son numéricas o si
#'     alguna es constante.
#'   \item Nota: en la versión actual, \code{method = "spearman"} produce un error
#'     (\code{object 'pe' not found}). Con \code{method = "both"} el gráfico solo
#'     muestra el texto de Spearman. Con \code{method = "pearson"} no se devuelve
#'     \code{select}. Con un solo método, \code{show.pval = FALSE} hace que
#'     \code{result} sea un vector y falla si \code{corplot = TRUE}.
#'   \item La prueba de Spearman avisa cuando hay empates (no se puede calcular
#'     el p-valor exacto).
#' }
#'
#' @seealso \code{\link[stats]{cor.test}}, \code{\link[mada]{CIrho}},
#'   \code{\link{kable_ueb}}, \code{\link{desc_group_numeric}},
#'   \code{\link{desc_ggplot_numeric}}
#'
#' @examples
#' res <- quickCor(dat = mtc_bis, x = "mpg", y = "hp", xtab = FALSE)
#' res$result
#'
#' # Ambas correlaciones, sin recta de regresión y con la leyenda arriba
#' quickCor(dat = mtc_bis, x = mpg, y = hp, method = "both", xtab = FALSE,
#'          sub = "Subtítulo", lm.fit = FALSE, pos = "topright")
#'
#' # Fila para tablas descriptivas, sin gráfico
#' res2 <- quickCor(dat = mtc_bis, x = "mpg", y = "hp", method = "both",
#'                  xtab = FALSE, corplot = FALSE, prep.tab = TRUE)
#' res2$df_prep_tab
#'
#' @keywords quickCor pearson spearman plotcor correlation
#'
#' @importFrom mada CIrho
#' @importFrom stats cor.test lm na.omit
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
quickCor <- function(dat, x, y,
                     nround = 3,
                     main = NULL,
                     xtab = TRUE,
                     pos = "bottomleft",
                     sz.xtab = NULL,
                     method = "pearson", #"pearson", "spearman" or "both"
                     corplot = TRUE,
                     cex.txt = 0.8,
                     cex.main = 0.8,
                     xtab.type = "html",
                     sub = NULL,
                     lm.fit = TRUE,
                     show.res = TRUE,
                     pos.text = -1.8,
                     show.pval = TRUE,
                     prep.tab = FALSE,
                     cor_cut = 0.7) {
  x <- names(dat %>% dplyr::select({{x}}))
  y <- names(dat %>% dplyr::select({{y}}))


  if (!is.numeric(dat[, x])) stop("La variable x debe ser numérica")
  if (!is.numeric(dat[, y])) stop("La variable y debe ser numérica")
  if(length(unique(na.omit(dat[, x]))) <2 | length(unique(na.omit(dat[, y]))) <2 ) stop("No es posible calcular la correlación. Una de las variables es constante.")
  namex <- ifelse(Hmisc::label(dat[,x]) == "", x,  Hmisc::label(dat[,x]))
  namey <- ifelse(Hmisc::label(dat[,y]) == "", y,  Hmisc::label(dat[,y]))


  n <- nrow(na.omit(dat[ , c(x, y)]))

  if(method == "pearson" | method == "both"){
    pe <- cor.test(dat[, x], dat[, y], method = "pearson")
    Pearson <- c(round(pe$estimate, nround),
                 paste0("(", round(pe$conf.int[1], nround), ", ", round(pe$conf.int[2], nround), ")"),
                 round(pe$p.value, nround),n )
    if(method == "pearson") result <- t(data.frame(Pearson))
  }
  if(method == "spearman" | method == "both"){
    sp <- cor.test(dat[, x], dat[, y], method = "spearman")
    ic.sp <- CIrho(sp$estimate, dim(na.omit(dat[ , c(x, y)]))[1], level = 0.95 )
    Spearman <- c(round(sp$estimate, nround),
                  paste0("(", round(ic.sp[2], nround), ", ", round(ic.sp[3], nround), ")"),
                  round(sp$p.value, nround),n )
    if(method == "spearman") result <- t(data.frame(Spearman))
  }
  if(method == "both"){
    result <- t(data.frame(Pearson, Spearman))
  }


  colnames(result) <- c("rho", "IC", "p-value", "n")
  result[,"p-value"][which(as.numeric(as.character(result[,"p-value"])) < 0.001)] <- "<0.001"

  if (!show.pval) result <- result[, !colnames(result) %in% c("p-value")]

  fit <- lm(dat[, y] ~ dat[, x])

  if (corplot) {
    if (is.null(main)) main <- paste(namex, "with", namey)
    plot(dat[, x], dat[, y],
         xlab = namex, ylab = namey,
         col = "purple",
         pch = 19,
         main =  main, cex.main = cex.main )
    mtext(sub, 3, line = .8)

    txt.plot <- ifelse(method == "pearson",
                       paste("Pearson Correlation = ", result["Pearson", "rho"],
                             "\n 95%CI", result["Pearson", "IC"], ifelse(show.pval,paste0("p-value = ",
                                                                                          result["Pearson", "p-value"]),"")),
                       paste("Spearman Correlation = ",
                             result["Spearman", "rho"],
                             "\n 95%CI", result["Spearman", "IC"], ifelse(show.pval,paste0("p-value = ",
                                                                                           result["Spearman", "p-value"]),"")))

    if (lm.fit) {
      abline(fit, col = "red", lwd = 3, lty = 3)
      legend(pos, c("Observations", "Linear fit"), cex = 0.8, # horiz = TRUE,
             lty = c(-1, 3), pch = c(19, NA),
             lwd = c(1, 2), col = c("purple", "red"),bg = "transparent")
    }else{
      legend(pos, "Observations", cex = 0.8,
             lty = -1, pch = 19,
             lwd = 1, col = "purple",bg = "transparent")
    }
    mtext(txt.plot, cex = cex.txt, line = pos.text )

  }


  result_list <- list(coeff = summary(fit), methods = "Correlation coefficient", result = result)
  if (prep.tab) {
    qc_res <- data.frame(result)
    result_list$df_prep_tab <- data.frame(t(c(variable = namex,
                                              levels = paste(rownames(qc_res), collapse = " <br>"),
                                              summary = paste(qc_res$rho, qc_res$IC, collapse = " <br> " ),
                                              p.value = paste(qc_res$p.value, collapse = " <br> " ),
                                              n = unique(qc_res$n))))
  }

  safe_sp_estimate <- tryCatch(sp$estimate, error = function(e) NULL)

  if (!is.null(safe_sp_estimate) && (abs(pe$estimate) > cor_cut | abs(safe_sp_estimate) > cor_cut)) {
    result_list$select <- namex
  }


  if (xtab) {

    print(kable_ueb(result, caption = paste("Correlation", namex, "with", namey,".", sub)))
    # }else{
  }
  if (show.res) return( result_list )

}


## corrplot color by group
# ggplot(cansue_cantum_wide_clin, aes(x=miR_106b.CANsue, y=miR_106b.CANtum, color = TN_HER2)) +
#   geom_point() + geom_rug()


## corrplot color by group
# ggplot(cansue_cantum_wide_clin, aes(x=miR_106b.CANsue, y=miR_106b.CANtum, color = TN_HER2)) +
#   geom_point() + geom_rug()
