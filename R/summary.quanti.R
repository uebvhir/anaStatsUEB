#' @title Resumen descriptivo y bivariante de variables cuantitativas
#'
#' @description
#' Calcula estadísticos descriptivos (media, desviación estándar, intervalo de
#' confianza de la media, mediana y cuartiles) de una variable cuantitativa. Los
#' resultados se pueden calcular de forma global o estratificada por un factor
#' (grupo), ejecutando automáticamente el test de contraste de hipótesis
#' adecuado (paramétrico o no paramétrico, para datos independientes o
#' apareados).
#'
#' @param data Data frame con las variables del análisis (no se admite un tibble).
#' @param x Variable cuantitativa a analizar: nombre de columna, con o sin comillas
#'   (si \code{var.tidy = TRUE}).
#' @param group Variable de agrupación (un factor), en el mismo formato que
#'   \code{x}. Si no es un factor, se convierte en uno con un aviso. Por defecto,
#'   \code{NULL} (solo resumen global).
#' @param method Tipo de análisis: \code{"param"} (paramétrico, por defecto) o
#'   \code{"non-param"} (no paramétrico, con guión). Cualquier otro valor
#'   (por ejemplo, \code{"nonparam"}) no calcula ningún test y el p-valor aparece
#'   como \code{"."}.
#' @param format Formato de los saltos de línea de las celdas: \code{"html"} (por
#'   defecto), \code{"latex"} o \code{"R"}. Cualquier otro valor elimina los saltos
#'   de línea.
#' @param nround Número de decimales (por defecto, 1) o \code{"auto"}. Con
#'   \code{"auto"} los decimales se calculan por variable (regla de Hopkins/Cole:
#'   los necesarios para que la SD tenga 2 cifras significativas), acotados a 1-3
#'   y comunes a todas las columnas (grupos y \code{ALL}); véase
#'   \code{\link{summary.nround}}. Las odds ratio usan 2 decimales con
#'   \code{"auto"}; con un valor numérico, ese mismo valor.
#' @param test Test estadístico que se quiere forzar, con uno de estos nombres
#'   exactos: \code{"Student's T"}, \code{"Mann–Whitney U"} (con guion largo,
#'   U+2013), \code{"Anova"}, \code{"Kruskal-Wallis"}, \code{"Paired Student's T"},
#'   \code{"Wilcoxon signed-rank test"}, \code{"RM-ANOVA"} o \code{"Friedman"}.
#'   Si es \code{NULL} (por defecto), se elige según \code{method}, el número de
#'   niveles de \code{group} y \code{paired}; véase \sQuote{Details}.
#' @param show.pval Lógico. Si es \code{TRUE} (por defecto), muestra la columna con
#'   el p-valor del test.
#' @param show.all Lógico. Si es \code{TRUE} (por defecto), añade la columna
#'   \code{ALL} con los resultados de la muestra total (se fuerza a \code{FALSE}
#'   con \code{paired = TRUE}).
#' @param show.n Lógico. Si es \code{TRUE} (por defecto), añade la columna \code{n}
#'   con el número de observaciones completas.
#' @param show.stat Lógico. Si es \code{TRUE}, añade el estadístico del test (por
#'   ejemplo, \eqn{t}, \eqn{W} o \eqn{F}). Solo se aplica si
#'   \code{show.pval = TRUE} (por defecto, \code{FALSE}).
#' @param show.or Lógico. Si es \code{TRUE} y \code{group} tiene exactamente dos
#'   niveles, añade la columna \code{OR} con la odds ratio de un modelo logístico
#'   \code{group ~ x} (por defecto, \code{FALSE}).
#' @param prep2sum Lógico. Si es \code{TRUE}, añade una columna \code{levels} vacía
#'   a la tabla para que se integre con \code{desc_ggstats} (por defecto,
#'   \code{FALSE}).
#' @param prep.tab Lógico. Si es \code{TRUE} y hay \code{group}, añade a la salida
#'   \code{df_prep_tab}, la tabla intermedia que usa \code{desc_group_numeric}.
#'   Requiere \code{show.n = TRUE} y \code{show.pval = TRUE} (por defecto,
#'   \code{FALSE}).
#' @param sub.ht Lógico. Si es \code{TRUE} (por defecto), añade un superíndice
#'   HTML de nota al nombre de la variable. Con \code{FALSE} la función falla;
#'   véase \sQuote{Details}.
#' @param paired Lógico. Si es \code{TRUE}, se usan los tests para datos apareados
#'   (por defecto, \code{FALSE}). Requiere \code{idvar}.
#' @param idvar Nombre (texto) de la variable de identificación del sujeto,
#'   necesaria si \code{paired = TRUE} (por defecto, \code{NULL}).
#' @param var.tidy Lógico. Si es \code{TRUE} (por defecto), \code{x} y \code{group}
#'   pueden darse con o sin comillas. Si es \code{FALSE}, deben ser texto.
#'
#' @return Una lista. Sin \code{group}: \code{variable}, \code{methods},
#' \code{txt_caption} y \code{summary}. Con \code{group}:
#' \describe{
#'   \item{\code{rows}}{Nombre de la variable cuantitativa.}
#'   \item{\code{txt_test}}{Texto con el nombre del test aplicado (\code{NULL} si
#'     \code{show.pval = FALSE}).}
#'   \item{\code{pval}}{P-valor del test sin redondear, o \code{"."} si el test
#'     falla (\code{NULL} si \code{show.pval = FALSE}).}
#'   \item{\code{txt_caption}}{Título de la tabla.}
#'   \item{\code{methods}}{Descripción del contenido de las celdas.}
#'   \item{\code{summary}}{Data frame con las columnas \code{variable}, una por
#'     grupo, \code{ALL}, \code{p.value}, \code{stat}, \code{n} y \code{OR}
#'     (según los argumentos \code{show.*}). Cada celda contiene n, media (SD),
#'     IC 95 \% de la media y mediana [Q1, Q3].}
#'   \item{\code{df_prep_tab}}{Solo si \code{prep.tab = TRUE}: data frame con
#'     \code{variable}, \code{levels}, \code{summary}, \code{p.value} y \code{n}.}
#' }
#'
#' @details
#' \itemize{
#'   \item Test por defecto con datos independientes: con \code{"param"},
#'     \code{"Student's T"} (2 grupos) o \code{"Anova"} (más de 2); con
#'     \code{"non-param"}, \code{"Mann–Whitney U"} o \code{"Kruskal-Wallis"}. Con
#'     \code{paired = TRUE}: \code{"Paired Student's T"} o \code{"RM-ANOVA"}
#'     (paramétrico), \code{"Wilcoxon signed-rank test"} o \code{"Friedman"}
#'     (no paramétrico).
#'   \item El test llamado \code{"Student's T"} es en realidad el de Welch
#'     (\code{t.test()} sin \code{var.equal}).
#'   \item Con datos apareados solo se usan los sujetos con valores completos en
#'     todos los niveles de \code{group}.
#'   \item Las observaciones con valores perdidos en \code{x} o en \code{group} se
#'     ignoran.
#'   \item Nota: \code{sub.ht = FALSE} produce un error. Un \code{test} no válido,
#'     o \code{test} apareado con \code{paired = FALSE}, no calcula el p-valor
#'     (error o \code{"."}). \code{"RM-ANOVA"} devuelve \code{"."} si el paquete
#'     \pkg{ez} no está cargado (\code{ezANOVA} no está importada).
#'     \code{prep.tab = TRUE} falla con \code{show.n = FALSE} o
#'     \code{show.pval = FALSE}.
#' }
#'
#' @seealso \code{\link{summary.quali}}, \code{\link{summary.quanti.rep}},
#'   \code{\link{summary.nround}}, \code{\link{desc_group_numeric}},
#'   \code{\link{desc_ggstats}}, \code{\link[stats]{t.test}},
#'   \code{\link[stats]{wilcox.test}}, \code{\link[stats]{kruskal.test}},
#'   \code{\link[stats]{friedman.test}}, \code{\link[Publish]{ci.mean}}
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   id = rep(1:13, 2),
#'   MUT = factor(c(rep("A", 13), rep("B", 13))),
#'   var = rnorm(26, mean = 10, sd = 2)
#' )
#' # Resumen global
#' summary.quanti(data = df, x = var)$summary
#'
#' # Por grupos, con el estadístico del test
#' res <- summary.quanti(data = df, x = "var", group = "MUT", show.stat = TRUE)
#' res$summary[, c("p.value", "stat")]
#' res$txt_test
#'
#' # Test no paramétrico
#' summary.quanti(data = df, x = var, group = MUT, method = "non-param")$pval
#'
#' # Datos apareados (idvar identifica al sujeto)
#' res_pa <- summary.quanti(data = df, x = var, group = MUT, idvar = "id", paired = TRUE)
#' res_pa$txt_test
#'
#' # Odds ratio y decimales automáticos
#' summary.quanti(data = df, x = var, group = MUT, show.or = TRUE, nround = "auto")$summary$OR
#'
#' @keywords summary quanti quantitative descriptive biostatistics
#'
#' @importFrom dplyr %>% select
#' @importFrom stats glm aov t.test wilcox.test kruskal.test friedman.test reshape
#' @importFrom stats aggregate complete.cases na.omit sd median quantile
#' @importFrom Publish ci.mean
#'
#' @author
#' Miriam Mota \email{miriam.mota@vhir.org}
#'
#' Biomedical Data Intelligence Unit (BIDU)
#' Vall d'Hebron Research Institute (VHIR) | Vall d'Hebron Barcelona Hospital Campus.
#'
#' @export
summary.quanti <- function(data,
                            x,
                            group = NULL,
                            method = "param",
                            format = "html",
                            nround = 1,
                            test = NULL,
                            show.pval = TRUE,
                            show.all = TRUE,
                            show.n = TRUE,
                            show.stat = FALSE,
                            show.or = FALSE,
                            prep2sum = FALSE,
                            prep.tab = FALSE,
                            sub.ht = TRUE,
                            paired = FALSE,
                            idvar = NULL,
                            var.tidy = TRUE)
{


  if (var.tidy) {
    ## Les 3 seguents linies permeten pasar el nom de la variable com a text o estil tidyverse
    x <- gsub('\"', "", deparse(substitute(x)))
    try(group <- gsub('\"', "", deparse(substitute(group))), TRUE)
    if (group == "NULL") group <- NULL
  }

  ## Comprovacions, stops i warnings
  if (all(is.na(data[,x]))) stop(paste0("The variable '",x,"' is empty"))
  if (is.factor(data[,x])) stop(paste0("La variable '",x,"' debe ser numérica"))

  if (!is.null(group) & !is.factor(data[,group])) {
    data[,group] <- factor(data[,group])
    warning( paste0("La variable '", group, "' ha sido transformada a factor" ))
  }

  ## només dades completes
  # if(!is.null(group))   data <- na.omit(data[,c(x,group)])


  ## Definicio de parametres
  new_line <- switch(format, "html" = " <br> ", "latex" = " \\\\ " , "R" = " \n ")
  xx <- data[,x]
  varname_x <- ifelse( Hmisc::label(data[,x]) != "", Hmisc::label(data[,x]), x)
  if (!is.null(group)) {
    varname_group <- ifelse( Hmisc::label(data[,group]) != "", Hmisc::label(data[,group]), group)
    yy <- data[, group]
  }

  if (sub.ht) sub <- "<sub>2</sub>"
  txt_descriptive <-  "<br> <font size='1'> 2: N <br> mean(sd) <br> [CI95% mean] <br> median[IQR] </font>"
  txt_caption = txt_descriptive

  # si es dato apareado
  if (paired){

    # estudiamos solo los casos completos
    show.all = F
    names(data)[names(data) == idvar] <- "id"
    idvar <- "id"
    data_wide <- reshape(data[,c(x,group,idvar)], timevar = group, idvar = idvar, direction = "wide") #, v.names = "x")
    idcomplete <- na.omit(data_wide)$id
    data <- data[which(data[,idvar] %in% idcomplete ), ]
    xx <- data[,x]
    yy <- data[,group]

    # si tenemos más de dos niveles
    if(length(levels(yy)) > 2){
      # pasar a long (por ser requerimiento de ezANOVA)
      data_long <- data.frame(
        id = factor(data[[idvar]]),  # Asegurar que 'id' es factor
        tiempo = yy,                 # Variable de tiempo (p.e. basal, visita1, visita2)
        puntuacion = xx              # Variable dependiente
      )
     }
  }


  ## Decimales dinamicos: nround = "auto" (si nround es numerico no cambia nada)
  nround_or <- if (is.numeric(nround)) nround else 2   # el OR tiene otra escala que la variable
  nround <- summary.nround(xx, nround)

  ## Resum univariat mean(sd) \\ IC mean \\ median[IQR]
  ci_uni <- ci.mean(xx)
  mn_sd <- paste0(round(mean(xx,na.rm = T),nround), " (", round(sd(xx,na.rm = T),nround), ")")
  md_iqr <- paste0(round(median(xx,na.rm = T),nround), " [",
                   round(quantile(xx,na.rm = T, probs = 0.25),nround),",",
                   round(quantile(xx,na.rm = T, probs = 0.75),nround),"]")

  ci_uni <- paste0("CI[",round(ci_uni$lower, nround), ";", round(ci_uni$upper, nround), "]")

  if (!is.null(group))  {
    n <-  sum(complete.cases(xx) & complete.cases(yy))
  }else{
    n <- sum(complete.cases(xx))
  }
  res_uni <- data.frame( ALL = paste0(n,new_line,mn_sd, new_line, ci_uni, new_line, md_iqr))

  if (!prep2sum) {
    res_uni <- cbind(variable = paste0(varname_x,sub), res_uni)
  }else{
    res_uni <- cbind(variable = paste0(varname_x,sub), levels = "" , res_uni)}
  if (show.n) res_uni$n <- n


  ### Análisis per grup
  if (!is.null(group)) {
    sum_bi <- aggregate(xx ~ yy, data = data, FUN = function(x) c(n = sum(complete.cases(x)),
                                                                  mean = round(mean(x, na.rm = T),nround),
                                                                  sd = round(sd(x, na.rm = T),nround),
                                                                  median = round(median(x,na.rm = T),nround),
                                                                  q25 = round(quantile(x,na.rm = T, probs = 0.25),nround),
                                                                  q75 = round(quantile(x,na.rm = T, probs = 0.75),nround)))

    ci_bi <- as.data.frame(ci.mean(xx ~ yy, data = data))[c("yy","lower", "upper")]

    ### En el cas que alguna de les categories no tingui recollit cap valor (p.e. homes no test embaràs), crear les celes buides
    if (nrow(sum_bi) != length(levels(yy))) {
      sum_bi <- rbind(sum_bi, data.frame(yy = levels(yy)[!levels(yy) %in% sum_bi$yy], xx = rep(NA, length(levels(yy)) - nrow(sum_bi)) ) )
      rownames(sum_bi) <- sum_bi$yy
      sum_bi <- sum_bi[levels(yy),]

      ## IC
      ci_bi <- as.data.frame(ci.mean(xx ~ yy, data = data))[c("yy","lower", "upper")]
      ci_bi <- rbind(ci_bi, data.frame(yy = levels(yy)[!levels(yy) %in% ci_bi$yy],
                                       lower = rep(NA, length(levels(yy)) - nrow(ci_bi)),
                                       upper = rep(NA, length(levels(yy)) - nrow(ci_bi))) )
      rownames(ci_bi) <- ci_bi$yy
      ci_bi <- ci_bi[levels(yy),]
    }

    res_all <- data.frame(t(paste0(paste0(sum_bi$xx[,"n"]), new_line,
                                   paste0( sum_bi$xx[,"mean"]," (", sum_bi$xx[,"sd"], ")" ), new_line,
                                   paste0("CI[",round(ci_bi$lower,nround), "; ", round(ci_bi$upper,nround),"]" ), new_line,
                                   paste0( sum_bi$xx[,"median"]," [", sum_bi$xx[,"q25.25%"],", ", sum_bi$xx[,"q75.75%"], "]" ))))
    colnames(res_all) <- levels(yy)
    rownames(res_all) <- paste0(varname_x,sub)

    if (!prep2sum) {
      res_all <- cbind(variable = paste0(varname_x,sub), res_all)
    }else{
      res_all <- cbind(variable = paste0(varname_x,sub), levels = "" , res_all)}

    ### Es mostra columna ALL
    if (show.all)    res_all$ALL  <- res_uni$ALL



    ### Test
    if (show.pval) {
      ## Decidim test que es realitza

      if (is.null(test) & !paired)    test <- switch(method,
                                                     "param" = ifelse(length(levels(yy)) > 2, "Anova","Student's T"),
                                                     "non-param" = ifelse(length(levels(yy)) > 2, "Kruskal-Wallis","Mann–Whitney U"))

      if (is.null(test) & paired)    test <- switch(method,
                                                    "param" = ifelse(length(levels(yy)) > 2, "RM-ANOVA","Paired Student's T"),
                                                    "non-param" = ifelse(length(levels(yy)) > 2, "Friedman","Wilcoxon signed-rank test"))

      ##### Esto se podria juntar para hacerlo mas eficiente y no calcular 2 veces lo mismo
      ## Calculem test
      pval <- try(switch(test,
                         "Student's T" = t.test(xx~yy)$p.va,
                         "Mann–Whitney U" = wilcox.test(xx~yy)$p.va,
                         "Anova" = summary(aov(xx~yy))[[1]][["Pr(>F)"]][1],
                         "Kruskal-Wallis" = kruskal.test(xx~yy)$p.va,
                         "Paired Student's T" = t.test(data_wide[,paste0(x,".",levels(yy)[1], collapse = "" )],
                                                       data_wide[,paste0(x,".",levels(yy)[2], collapse = "")], paired = TRUE)$p.va,
                         "Wilcoxon signed-rank test" = wilcox.test(data_wide[,paste0(x,".",levels(yy)[1], collapse = "" )],
                                                                   data_wide[,paste0(x,".",levels(yy)[2], collapse = "")], paired = TRUE)$p.va,
                         "RM-ANOVA" = ezANOVA(data = data_long, dv = puntuacion, wid = id, within = tiempo, detailed = TRUE)$ANOVA$p[1],
                         "Friedman" = friedman.test(puntuacion ~ tiempo | id, data = data_long)$p.value),TRUE)
      pval <- ifelse(grepl("Error", pval), ".",pval)
      pval_round <- ifelse(grepl("Error", try(round(pval,3), TRUE)), ".", round(pval,3))


      res_all$p.value <- ifelse(pval != "." & pval < 0.001, "<0.001", pval_round  )
      txt_pval = paste0("<font size='1'> <br> p.value:  ", test, "</font>")


      if(show.stat){
        stat <- try(switch(test,
                           "Student's T" = t.test(xx~yy)$stat,
                           "Mann–Whitney U" = wilcox.test(xx~yy)$stat,
                           "Anova" = summary(aov(xx~yy))[[1]][["F value"]][1],
                           "Kruskal-Wallis" = kruskal.test(xx~yy)$stat,
                           "Paired Student's T" = t.test(data_wide[,paste0(x,".",levels(yy)[1], collapse = "" )],
                                                         data_wide[,paste0(x,".",levels(yy)[2], collapse = "")], paired = TRUE)$stat,
                           "Wilcoxon signed-rank test" = wilcox.test(data_wide[,paste0(x,".",levels(yy)[1], collapse = "" )],
                                                                     data_wide[,paste0(x,".",levels(yy)[2], collapse = "")], paired = TRUE)$stat,
                           "RM-ANOVA" = ezANOVA(data = data_long, dv = puntuacion, wid = id, within = tiempo, detailed = TRUE)$ANOVA$F[1],
                           "Friedman" = friedman.test(puntuacion ~ tiempo | id, data = data_long)$statistic),TRUE)
        stat <- ifelse(grepl("Error", stat), ".",stat)
        stat_round <- ifelse(grepl("Error", try(round(pval,3), TRUE)), ".", round(stat,3))


        res_all$stat <- stat_round

      }else{
        stat <- NULL

      }



    }else{
      pval <- NULL
      txt_pval <- NULL
    }








    if (show.n) res_all$n <- sum(complete.cases(xx) & complete.cases(yy))

    if (show.or & (length(levels(yy)) == 2) ) {
      or_mod <- desc_mod(glm(yy ~xx, family = "binomial"))
      OR <- paste0(round(or_mod$`Odds Ratio`,nround_or), "[", round(or_mod$`CI (lower)`,nround_or), ", ", round(or_mod$`CI (upper)`,nround_or), "]")
      res_all$OR <- OR
    }else{
      OR <- NULL

    }

    txt_caption = paste0("Summary of results by groups of ",varname_group,txt_descriptive)

    list_return <- list(rows = x,
                        txt_test = txt_pval,
                        pval = pval,
                        txt_caption = txt_caption,
                        methods = txt_descriptive,
                        summary = res_all )

    if (prep.tab) {

      sq_s <- data.frame(res_all)
      sq_sum <- t(res_all %>% select(-variable,-p.value, -n))
      list_return$df_prep_tab <- data.frame(data.frame(variable = varname_group,
                                                       levels = rownames(sq_sum),
                                                       summary = sq_sum[,1],
                                                       p.value = unlist(c(sq_s %>% select(p.value), rep("", nrow(sq_sum) - 1))),
                                                       n =  unlist(c(sq_s %>% select(n), rep("", nrow(sq_sum) - 1)))))
    }
  }


  ## RESULTATS
  ifelse(!is.null(group),
         return(list_return),
         return(list(variable = x,methods = txt_caption, txt_caption = txt_caption,  summary = res_uni)))

}
