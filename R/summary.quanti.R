#' Resumen Descriptivo y Bivariante de Variables Cuantitativas
#'
#' @description
#' Calcula estadísticos descriptivos (media, desviación estándar, mediana y cuartiles)
#' para una variable cuantitativa continua. Los resultados se pueden calcular de manera
#' global o estratificada por un factor (grupo), ejecutando automáticamente los tests
#' de contraste de hipótesis adecuados (paramétricos o no paramétricos, independientes o apareados).
#'
#' @param data Un \code{data.frame}, lista o entorno que contiene las variables del análisis.
#' @param x Carácter o nombre simbólico (estilo特 tidyverse) de la variable cuantitativa a analizar.
#' @param group Carácter o nombre simbólico (estilo tidyverse) de la variable factor utilizada para la estratificación (opcional). Por defecto es \code{NULL}.
#' @param method Carácter. Especifica el tipo de análisis: \code{'param'} (paramétrico, por defecto) o \code{'nonparam'} (no paramétrico).
#' @param format Carácter. Formato de salida para el resumen: \code{'html'} (por defecto), \code{'r'} o \code{'no'}.
#' @param nround Entero. Número de decimales utilizado para el redondeo de los valores. Por defecto es \code{1}.
#' @param test Carácter opcional para forzar un test estadístico específico (ej: 'Anova', 'Student's T', 'Kruskal-Wallis', 'Mann-Whitney U', 'Paired Student's T', 'Wilcoxon signed-rank test', 'RM-ANOVA'). Si es \code{NULL} (por defecto), se elige automáticamente según los datos.
#' @param show.pval Lógico. Si es \code{TRUE} (por defecto), muestra la columna con el p-valor del test de asociación.
#' @param show.all Lógico. Si es \code{TRUE} (por defecto), añade una columna con los resultados descriptivos de la muestra total global.
#' @param show.n Lógico. Si es \code{TRUE} (por defecto), muestra el recuento de la N de la muestra.
#' @param show.stat Lógico. Si es \code{TRUE}, añade el estadístico del test a la salida. Por defecto es \code{FALSE}.
#' @param show.or Lógico. Si es \code{TRUE} y el grupo tiene exactamente dos niveles, calcula y muestra las Odds Ratios de un modelo logístico aplicado. Por defecto es \code{FALSE}.
#' @param prep2sum Lógico. Modifica la estructura interna de la tabla de salida para que se integre correctamente con la función \code{desc_group}. Por defecto es \code{FALSE}.
#' @param prep.tab Lógico. Modifica la estructura interna de la tabla de salida para que se integre correctamente con la función \code{desc_quanti}. Por defecto es \code{FALSE}.
#' @param sub.ht Lógico. Si es \code{TRUE} (por defecto), formatea ciertos elementos (como indicadores de grupo o método) como subíndices HTML.
#' @param paired Lógico. Si es \code{TRUE}, se ejecutarán las versiones de tests estadísticos apareados. Por defecto es \code{FALSE}.
#' @param idvar Carácter opcional. El nombre de la variable de identificación de sujeto/clúster necesaria si \code{paired = TRUE}. Por defecto es \code{NULL}.
#' @param var.tidy Lógico. Si es \code{TRUE} (por defecto), permite pasar los nombres de las variables tanto entre comillas (\code{"var"}) como directamente como expresiones sin comillas.
#'
#' @return Una lista que contiene los siguientes elementos:
#' \describe{
#'   \item{rows}{Nombre de la variable cuantitativa evaluada.}
#'   \item{txt_test}{Cadena de texto que identifica el test estadístico aplicado.}
#'   \item{pval}{Valor p de carácter numérico resultante del test.}
#'   \item{txt_caption}{Cadena con el título generado para la tabla.}
#'   \item{methods}{Descripción textual del tipo de metodología empleada (paramétrica/no paramétrica).}
#'   \item{summary}{Un \code{data.frame} estructurado y formateado listo para la visualización final en HTML.}
#'   \item{df_prep_tab}{Si \code{prep.tab = TRUE}, incluye el dataframe intermedio procesado.}
#' }
#' @export summary.quanti
#'
#' @keywords summary quanti quantitative descriptive biostatistics
#'
#' @author Miriam Mota \email{miriam.mota@@vhir.org}
#'
#' @importFrom dplyr %>% select mutate
#' @importFrom stats glm
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(kableExtra)
#'
#' df_prova <- data.frame(
#'   id = rep(1:13, 2),
#'   MUT = factor(c(rep("A", 13), rep("B", 13))),
#'   var = rnorm(26, mean = 10, sd = 2)
#' )
#'
#' # Resumen global simple
#' summary.quanti(data = df_prova, x = var)
#'
#' # Resumen bivariante estratificado por grupo mutacional
#' tab <- summary.quanti(data = df_prova, x = "var", group = "MUT")
#'
#' # Resumen utilizando métodos no paramétricos y datos apareados
#' tab_aparellat <- summary.quanti(
#'   data = df_prova, x = var, group = MUT,
#'   idvar = "id", paired = TRUE, method = "nonparam"
#' )
#'
#' # Renderizado en una tabla con kableExtra
#' kable(tab$summary, escape = FALSE, row.names = FALSE, align = "c",
#'       caption = paste(tab$txt_caption, tab$txt_test)) %>%
#'   kable_styling(full_width = FALSE, font_size = 14)
#' }
#'
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
      OR <- paste0(round(or_mod$`Odds Ratio`,nround), "[", round(or_mod$`CI (lower)`,nround), ", ", round(or_mod$`CI (upper)`,nround), "]")
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
