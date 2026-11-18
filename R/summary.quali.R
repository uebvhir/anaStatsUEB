#' @title Resumen descriptivo de variables cualitativas con comparación entre grupos
#'
#' @description
#' Genera un resumen descriptivo de una variable cualitativa (factor), con
#' frecuencias, porcentajes e intervalos de confianza exactos (binomial). Si se
#' indica una variable de agrupación, calcula el resumen por grupos y,
#' opcionalmente, el p-valor de un test de asociación (chi-cuadrado o Fisher), el
#' estadístico de prueba y las odds ratio. Los saltos de línea de las celdas se
#' adaptan a HTML, LaTeX o texto plano.
#'
#' A partir de mayo de 2025, la prueba chi-cuadrado se ejecuta por defecto con
#' \code{correct = FALSE} (sin corrección de Yates). Para replicar resultados
#' anteriores, especifique \code{correct = TRUE} explícitamente.
#'
#' @param data Data frame con las variables.
#' @param x Variable cualitativa a describir (un factor): nombre de columna, con o
#'   sin comillas (si \code{var.tidy = TRUE}).
#' @param group Variable de agrupación (un factor), en el mismo formato que
#'   \code{x}. Por defecto, \code{NULL} (solo resumen global).
#' @param include.NA Lógico. Si es \code{TRUE}, los valores perdidos de \code{x} se
#'   cuentan como un nivel más, con el nombre \code{patt.NA} (por defecto,
#'   \code{FALSE}).
#' @param patt.NA Texto con el nombre del nivel de valores perdidos cuando
#'   \code{include.NA = TRUE} (por defecto, \code{"NA"}).
#' @param format Formato de los saltos de línea en las celdas: \code{"html"} (por
#'   defecto), \code{"latex"} o \code{"R"}. Cualquier otro valor elimina los saltos
#'   de línea.
#' @param nround Número de decimales de los porcentajes y sus IC (por defecto, 1).
#'   Si llega \code{"auto"} (por ejemplo, desde \code{desc_group()}), se usa 1
#'   decimal.
#' @param test Test de asociación: \code{"Fisher's exact"} o
#'   \code{"Chi-squared"}. Si es \code{NULL} (por defecto), se usa chi-cuadrado
#'   si todas las frecuencias esperadas son al menos 5 y Fisher en caso
#'   contrario.
#' @param show.pval Lógico. Si es \code{TRUE} (por defecto), muestra el p-valor del
#'   test de asociación.
#' @param show.all Lógico. Si es \code{TRUE} (por defecto), añade la columna
#'   \code{ALL} con los resultados globales (sin estratificar).
#' @param show.n Lógico. Si es \code{TRUE} (por defecto), añade una columna con el
#'   número total de observaciones.
#' @param show.stat Lógico. Si es \code{TRUE}, añade el estadístico de prueba
#'   (chi-cuadrado; con Fisher se muestra \code{"."}). Solo se aplica si
#'   \code{show.pval = TRUE} (por defecto, \code{FALSE}).
#' @param show.or Lógico. Si es \code{TRUE} y \code{group} tiene exactamente dos
#'   niveles, añade la columna \code{OR} con las odds ratio de cada nivel de
#'   \code{x} respecto al primero (\code{"Ref."}), calculadas con
#'   \code{\link{desc_mod}} sobre una regresión logística (por defecto,
#'   \code{FALSE}).
#' @param byrow Lógico. Si es \code{FALSE} (por defecto), los porcentajes se
#'   calculan por columnas (dentro de cada grupo); si es \code{TRUE}, por filas.
#'   No se admite \code{NA}.
#' @param sub.ht Lógico. Si es \code{TRUE} (por defecto), añade el superíndice de
#'   la nota al nombre de la variable. Con \code{FALSE} la función falla; véase
#'   \sQuote{Details}.
#' @param var.tidy Lógico. Si es \code{TRUE} (por defecto), \code{x} y \code{group}
#'   pueden darse con o sin comillas. Si es \code{FALSE}, deben ser texto.
#' @param correct Lógico. Corrección de Yates en el test chi-cuadrado (por defecto,
#'   \code{FALSE}).
#'
#' @return Una lista. Sin \code{group}: \code{variable}, \code{methods},
#' \code{txt_caption} y \code{summary}. Con \code{group}: \code{rows} y
#' \code{columns} (nombres de \code{x} y \code{group}), \code{txt_test} (texto con
#' el nombre del test), \code{pval} (p-valor sin redondear, o \code{"."} si falla),
#' \code{txt_caption}, \code{methods} y \code{summary}. \code{summary} es un data
#' frame con las columnas \code{variable}, \code{levels}, una columna por grupo,
#' \code{ALL}, \code{p.value}, \code{stat}, \code{n} y \code{OR} (según los
#' argumentos \code{show.*}).
#'
#' @details
#' \itemize{
#'   \item \code{x} y \code{group} deben ser factores y \code{data} un data frame
#'     (no un tibble). Se detiene con un error en caso contrario.
#'   \item Con \code{group}, solo se usan las filas completas para \code{x} y
#'     \code{group}. Los niveles de \code{x} sin observaciones se eliminan con un
#'     mensaje.
#'   \item Con \code{test = "Fisher's exact"} en tablas mayores de 2x2, el p-valor
#'     se calcula por simulación (\code{simulate.p.value = TRUE}), por lo que puede
#'     variar entre ejecuciones; use \code{set.seed()} para reproducirlo.
#'   \item Nota: \code{sub.ht = FALSE} produce un error. \code{test} solo admite
#'     \code{"Fisher's exact"} y \code{"Chi-squared"}; otros valores producen un
#'     error poco claro. \code{byrow = NA} produce un error. La leyenda
#'     (\code{txt_caption}) repite tres veces el texto descriptivo cuando
#'     \code{show.pval = TRUE}. El aviso de más de 10 niveles de \code{group} no
#'     llega a activarse.
#' }
#'
#' @seealso \code{\link{summary.quali.rep}}, \code{\link{summary.quanti}},
#'   \code{\link{desc_group}}, \code{\link[binom]{binom.confint}},
#'   \code{\link[stats]{chisq.test}}, \code{\link[stats]{fisher.test}}
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(MUT = factor(c(rep("A", 12), rep("B", 13))),
#'                  var = factor(sample(c("Yes", "no"), 25, replace = TRUE)))
#' # Resumen global
#' summary.quali(df, x = "var")$summary
#'
#' # Por grupos, sin la columna global
#' summary.quali(df, x = "var", group = "MUT", show.all = FALSE)$summary
#'
#' # Nombres sin comillas y porcentajes por filas
#' res <- summary.quali(mtc_bis, x = gear, group = vs, byrow = TRUE)
#' res$pval
#'
#' # Odds ratio, estadístico de prueba y corrección de Yates
#' res2 <- summary.quali(mtc_bis, x = am, group = vs, show.or = TRUE,
#'                       show.stat = TRUE, test = "Chi-squared", correct = TRUE)
#' res2$summary[, c("levels", "p.value", "stat", "OR")]
#'
#' @keywords summary ci qualitative txt_descriptive exploratory
#'
#' @importFrom binom binom.confint
#' @importFrom stats chisq.test fisher.test glm na.omit
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
summary.quali <- function(data,
                          x,
                          group = NULL,
                          include.NA = FALSE,
                          patt.NA = "NA",
                          format = "html",
                          nround = 1,
                          test = NULL,
                          show.pval = TRUE,
                          show.all = TRUE,
                          show.n = TRUE,
                          show.stat = FALSE,
                          show.or = FALSE,
                          byrow = FALSE,
                          sub.ht = TRUE,
                          var.tidy = TRUE,
                          correct = FALSE) {

  ## nround = "auto" llega via desc_group(...); los porcentajes mantienen 1 decimal
  if (identical(nround, "auto")) nround <- 1


  if (var.tidy) {
    ## Les 3 seguents linies permeten pasar el nom de la variable com a text o estil tidyverse
    x <- gsub('\"', "", deparse(substitute(x)))
    try(group <- gsub('\"', "", deparse(substitute(group))), TRUE)
    if (group == "NULL") group <- NULL
  }

  if (include.NA) {
    lb <- Hmisc::label(data[,x])
    data[,x] <- as.character(data[,x])
    data[,x][which(is.na(data[,x]))] <- patt.NA
    data[,x] <- factor_ueb(data[,x])
    Hmisc::label(data[,x]) <- lb
  }

  ## Comprovacions variades
  if (!is.null(group)) {

    if (length(length(table(data[,group]))) > 10) warning("La variable group tiene mas de 10 niveles")
    if (class(data[,group])[length(class(data[,group]))] != "factor") stop("La variable group debe ser factor")
    ### només dades completes
    data <- na.omit(data[,c(x,group)])
  }
  if (class(data[,x])[length(class(data[,x]))] != "factor") stop("La variable x debe ser factor")



  if (any(table(data[,x]) == 0 ) ) {
    lb <- Hmisc::label(data[,x])
    if (is.factor(data[,x] )) data[,x] <- droplevels(data[,x])
    message("Some levels of ", x, " are removed since no observation in that/those levels")
    Hmisc::label(data[,x]) <- lb
  }



  ## Assignació paametres i variables
  new_line <- switch(format, "html" = " <br> ", "latex" = " \\\\ " , "R" = " \n ")
  xx <- data[,x]
  varname_x <- ifelse( Hmisc::label(data[,x]) != "", Hmisc::label(data[,x]), x)
  if (!is.null(group)) {
    varname_group <- ifelse( Hmisc::label(data[,group]) != "", Hmisc::label(data[,group]), group)
    yy <- data[, group]
  }
  if (sub.ht) sub <- "<sub>1</sub>"


  ## Resum univariat
  if (!byrow) {n <-  sum(table(xx))} else {n <- table(xx) }
  uni <- binom.confint(table(xx), n , methods = "exact")
  if (is.null(uni$x)) uni$x <- uni$x.Freq
  if (is.null(uni$mean)) uni$mean <- uni$mean.Freq
  res_uni <- data.frame( ALL =  paste0(uni$x, " (", round(uni$mean*100,nround), "%)", new_line,
                                       "[",round(uni$lower*100, nround),"; ", round(uni$upper*100, nround), "]" ),row.names =  levels(xx) )
  res_uni <- cbind(variable = c(paste0(varname_x, sub),rep("",nrow(res_uni) - 1)),levels = levels(xx), res_uni)
  if (show.n) res_uni$n <-  c(sum(table(xx)),rep("",nrow(res_uni) - 1))
  txt_caption <- paste0(" <font size='1'> 1: n(%) <br> [Exact CI] </font>")

  if (!is.null(group)) {
    # Calculem resum estadístic n(%) IC

    ## PER COLUMNES (analisi habitual)
    if (!byrow) {
      res_bi <-  apply(table(xx, yy), 2, function(x)  {
        bb <- binom.confint(x,sum(x), methods = "exact")
        if (is.null(bb$x)) bb$x <- bb$x.Freq
        if (is.null(bb$mean)) bb$mean <- bb$mean.Freq
        data.frame(paste0(bb$x, " (", round(bb$mean*100,nround), "%)", new_line,
                          "CI[",round(bb$lower*100, nround),"; ", round(bb$upper*100, nround), "]" ))
      })
      res_all <- do.call(cbind,res_bi)
      colnames(res_all) <- levels(yy)
      rownames(res_all) <- levels(xx)
      res_all <- cbind(variable = c(paste0(varname_x, sub),rep("",nrow(res_all) - 1)),levels = levels(xx), res_all)
      txt_descriptive <- " <font size='1'> <br> 1: by col <br> n(%) <br> [Exact CI] </font>"
      txt_caption <- paste0("Summary of results by groups of ",varname_group ,txt_descriptive)
      ## PER FILES
    } else{
      res_bi <-  apply(table(xx, yy), 1, function(x)  {
        bb <- binom.confint(x,sum(x), methods = "exact")
        if (is.null(bb$x)) bb$x <- bb$x.Freq
        if (is.null(bb$mean)) bb$mean <- bb$mean.Freq
        data.frame(paste0(bb$x, " (", round(bb$mean*100,nround), "%)", new_line,
                          "CI[",round(bb$lower*100, nround),"; ", round(bb$upper*100, nround), "]" ))
      })
      res_all <- data.frame(t(do.call(cbind,res_bi)))
      colnames(res_all) <- levels(yy)
      rownames(res_all) <- levels(xx)

      res_all <- cbind(variable = c(paste0(varname_x, sub),rep("",nrow(res_all) - 1)),levels = levels(xx), res_all)
      txt_descriptive <- " <font size='1'> <br> 1: by row <br> n(%) <br> [Exact CI] </font>"
      txt_caption <- paste0("Summary of results by groups of ",varname_group ,txt_descriptive)
    }

    ## Afegim columna ALL als resultats
    if (show.all)    res_all$ALL  <- res_uni$ALL


    ## Es realitza test estadístic
    if (show.pval ) {
      ## Decidim test que es realitza
      if (is.null(test))    test <- ifelse( any(chisq.test(table(xx, yy))$expected < 5) , "Fisher's exact","Chi-squared")
      ## Calculem test
      pval <- try(switch(test,
                         "Fisher's exact" = fisher.test(table(xx,yy),simulate.p.value = T )$p.va,
                         "Chi-squared" = chisq.test(xx,yy,correct = correct)$p.val), TRUE)
      ## arreglem p.val final
      pval <- ifelse(grepl("Error", pval), ".",pval)
      pval_round <- ifelse(grepl("Error", try(round(pval,3), TRUE)), "-", round(pval,3))
      pval_round <- switch(test,
                           "Fisher's exact" = paste0(pval_round,  "<sup>3</sup>"),
                           "Chi-squared" = paste0(pval_round,  "<sup>4</sup>"))
      pval_round[grep("-",pval_round, fixed = T)] <- "-"

      res_all$p.value <- c(ifelse(pval != "." & pval < 0.001, "<0.001", pval_round ), rep("", nrow(res_all) - 1))
      txt_pval <- paste0("<font size='1'> <br> p.value: ",switch(test, "Fisher's exact" = "<sup>3</sup>","Chi-squared" = "<sup>4</sup>"), test, "</font>")
      txt_caption <-  paste(txt_caption,txt_descriptive,txt_descriptive,txt_pval )



      if (show.stat ) {

        ## Calculem test
        stat <- try(switch(test,
                           "Fisher's exact" = "-",
                           "Chi-squared" = chisq.test(xx,yy,correct = correct)$stat), TRUE)
        ## arreglem stat final
        stat_round <- ifelse(stat == "-", ".", round(stat,3))
        res_all$stat <- c(stat_round, rep("", nrow(res_all) - 1))
      }else{
        stat <- NULL

      }



    }else{
      pval <- NULL
      txt_pval <- NULL
    }

    if (show.n)  {
      res_all$n <-  c(sum(table(xx,yy)),rep("",nrow(res_all) - 1))
      }

    if (show.or & (length(levels(yy)) == 2) ) {
      or_mod <- try(desc_mod(glm(yy ~xx, family = "binomial")), TRUE)
      if(inherits(or_mod, "try-error")){
        OR <- "-"
      }else{
        OR <- paste0(round(or_mod$`Odds Ratio`,nround), "[", round(or_mod$`CI (lower)`,nround), ", ", round(or_mod$`CI (upper)`,nround), "]")
        OR <- c("Ref.", OR)
      }

      res_all$OR <- OR
    }else{
      OR <- NULL

    }

  }




  ## RESULTATS
  ifelse(!is.null(group),
         return(list(rows = x,
                     columns = group,
                     txt_test = txt_pval,
                     pval = pval,
                     txt_caption = txt_caption,
                     methods = txt_descriptive ,
                     summary = res_all )),
         return(list(variable = x, methods = txt_caption, txt_caption = txt_caption, summary = res_uni)))
}








