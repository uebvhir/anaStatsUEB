# Modificacion de doROC para permitir cambiar el método de cutoff
doROC <- function(frml, x , group  , dat,
                  tag.healthy = NULL,
                  title = NULL,
                  modGLM = NULL,
                  method.cutoff = "Youden",
                  sens_target = 0.90,
                  spec_target = NULL,
                  doPlot = TRUE,
                  cex.main = 2,
                  cex.text = 1.4,
                  cex.sub = 0.9,
                  cex = 0.5,
                  show.lg = TRUE,
                  show.cascon = TRUE,
                  show.detail = TRUE,
                  xtab = FALSE,
                  xtab.type = "latex",
                  direction = c("<", ">"), ...)
{
  
  ## comprovacions varies, warnings i errors
  if (exists(deparse(substitute(show.ci)))) message("\n UEBmessage: Argument 'show.ci' is deprecated \n")
  if (exists(deparse(substitute(validation)))) message("\n UEBmessage: Argument 'validation' is deprecated \n")
  if (exists(deparse(substitute(test_y)))) message("\n UEBmessage: Arguments 'test' and 'test_y' are deprecated \n")
  if (exists(deparse(substitute(col.thres)))) message("\n UEBmessage: Argument 'col.thres' is deprecated \n")
  if (exists(deparse(substitute(col.ic)))) message("\n UEBmessage: Argument 'col.ic' is deprecated \n")
  if (exists(deparse(substitute(x.axes)))) message("\n UEBmessage: Argument 'x.axes' is deprecated \n")
  if (exists(deparse(substitute(show.thr)))) message("\n UEBmessage: Argument 'show.thr' is deprecated \n")
  if (is.null(modGLM)) stop("Es necesario indicar, TRUE o FALSE para el parametro modGLM.")
  if ((missing(x) | missing(group)) & missing(frml))  stop("'x' and 'group' argument required, or 'frml' argument required", call. = FALSE)
  
  # if (missing(x)) x <- strsplit(as.character(frml), "~", fixed = T)[[3]]
  if (missing(x)) x <-  rhs.vars(frml)
  if (missing(frml)) frml <- as.formula(paste(group, "~", paste0(x, collapse = " + ")))
  if (missing(group)) group <- lhs.vars(frml)
  if (is.null(title)) title <- paste(group, "-",paste0(x, collapse = "+"))
  if (is.null(tag.healthy)) tag.healthy <- levels(dat[,group])[1]
  
  dat[,group] <- relevel(dat[,group], ref = tag.healthy)
  
  results <- list()
  
  ## assignació variable resposta (group) i variable cuantitativa (x) o formula (frml). També titol y nivell de referencia (tag.healthy)
  if (modGLM) {
    mod <- glm(frml, data = dat, family = binomial, na.action = "na.omit")
    results$mod <- mod
    pred <- predict(mod, type = "response")
    dat$pred <- NA
    dat[names(pred),]$pred <- pred
    x <- "pred"
  }
  results$dat <- dat
  
  # Para otros métodos que no son Youden y que requieren parámetros
  control_args <- switch(
    method.cutoff,
    "MinValueSe" = control.cutpoints(valueSe = sens_target),
    "ValueSe"    = control.cutpoints(valueSe = sens_target),
    "MinValueSp" = control.cutpoints(valueSp = spec_target),
    "ValueSp"    = control.cutpoints(valueSp = spec_target),
    "MinValueSpSe" = control.cutpoints(valueSe = sens_target, valueSp = spec_target),
    control.cutpoints()  # default
  )
  
  
  
  # calcul corba ROC, punt optim amb index de youden i mesures de clasificacio
  positive.class <- levels(dat[,group])[levels(dat[,group]) != tag.healthy]
  
  clasRes <- optimal.cutpoints(
    X = x,
    status = group,
    methods = method.cutoff,
    data = dat,
    tag.healthy = tag.healthy,
    ci.fit = TRUE,
    direction = direction,
    control = control_args
  )
  
  results$res_sum <- summary(clasRes)
  
  
  if (doPlot) {
    opt <- par(cex = cex, cex.main = cex.main, ...)
    plot(clasRes, which = 1, legend = show.lg,
         ylim = c(0,1))
    par(opt)
    mtext(title, side = 3, cex = cex.sub)
    if (show.cascon) {
      text(.85, .25,
           paste0("controls: ", clasRes[[method.cutoff]]$Global$measures.acc$n$h,
                  "\n cases: ", clasRes[[method.cutoff]]$Global$measures.acc$n$d),
           cex = cex.text)
    }
    
  }
  
  if (show.detail) {
    results$res_detail <- cbind(results$res_sum[[method.cutoff]]$Global$measures.acc$cutoffs,
                                results$res_sum[[method.cutoff]]$Global$measures.acc$Se[,1],
                                results$res_sum[[method.cutoff]]$Global$measures.acc$Sp[,1],
                                results$res_sum[[method.cutoff]]$Global$measures.acc$DLR.Positive[,1],
                                results$res_sum[[method.cutoff]]$Global$measures.acc$DLR.Negative[,1])
    colnames(results$res_detail) <- c("Cutpoint", "Sensitivity", "Specificity", "LR+", "LR-")
  }
  
  ## es mostren els resultats general com xtable per a latex
  if (xtab) {
    # print(xtable(results$res_sum$p.table$Global[[method.cutoff]][[1]],
    #        caption = paste(title,". AUC ", results$res_sum$p.table$Global$AUC_CI )))
    
    res_xtab <- kable(results$res_sum$p.table$Global[[method.cutoff]][[1]], format = xtab.type, booktabs = T,
                      caption = paste(title,". AUC ", results$res_sum$p.table$Global$AUC_CI ), longtable = TRUE,
                      escape = F)
    results$xtab <- res_xtab
    
  }
  
  ## punts de talls
  if (modGLM) {
    results$cutoff.probability <- clasRes[[method.cutoff]]$Global$optimal.cutoff$cutoff # threshold  de Youden probability
    name_var_cuanti <-  rhs.vars(frml)
    
    if (length(rhs.vars(frml)) == 1) {
      results$cutoff.variable <- unique(results$dat[,name_var_cuanti][which(results$dat$pred == results$cutoff.probability)])
    }else{
      results$cutoff.variable <- "No se puede calcular debido a que existe más de una variable explicativa."
    }
    if (identical(direction, ">") ) {
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.probability, tag.healthy, positive.class ))
    }else{
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.probability, positive.class, tag.healthy ))
    }
    
  }else{
    results$cutoff.variable <- clasRes[[method.cutoff]]$Global$optimal.cutoff$cutoff # punto de corte optimo, segun Youden para variable numerica
    
    if (identical(direction, ">") ) {
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.variable, tag.healthy, positive.class ))
    }else{
      results$dat$outcome.predict <- factor(ifelse(dat[,x] >= results$cutoff.variable, positive.class, tag.healthy ))
    }
  }
  results$dat$outcome.predict <- factor(results$dat$outcome.predict, c(tag.healthy,positive.class))
  
  results[[method.cutoff]] <- clasRes[[method.cutoff]]$Global$optimal.criterion
  results$auc <- results$res_sum[[method.cutoff]]$Global$measures.acc$AUC
  results$table <- table(Group = results$dat[,group], predict = results$dat$outcome.predict)
  results$tag.healthy <- tag.healthy
  
  # missatge canvi de nom a output
  message(" !!!!!!!!! \n UEBmessage: Output 'thres.best' are deprecated, new same output is 'cutoff.probability' \n !!!!!!!!!")
  
  message(" !!!!!!!!! \n UEBmessage: tag.healthy: ", tag.healthy , " \n !!!!!!!!!")
  return(results)
}