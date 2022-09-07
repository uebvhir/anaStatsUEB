

######## DE MOMENT NOMÉS FUNCIONA PER VARIABLES QUANTITATIVES DE SEGUIMENT

desc_group_strat <- function(var_strat = NULL, var_rep = NULL, var_time = NULL, data = NULL ,...){

  ## creem les taules desc_group
  ls <- list()
  for(i in seq_along(levels(data[,var_strat]))){
    lv <- levels(data[,var_strat])[i]
    ls[[lv]] <- desc_group(covariates = var_rep, group = var_time ,data = data %>% filter(get(var_strat) == lv), ...)$df_all %>%
      mutate(group = lv)
    names(  ls[[lv]]) <- vapply(strsplit(names( ls[[lv]]) ,"<br>"), `[`,1, FUN.VALUE = character(1))
  }


  ## intercalem informació
  df_res <- do.call(rbind, ls)[order(sequence(sapply(ls, nrow))), ] %>%
    relocate(group, .after = variable) %>%
    select(-levels) %>%
    mutate(variable = gsub("<sub>2</sub>", "",variable, fixed = T))

  ## generem taula
  kable_ueb(df_res %>% select(-variable),row.names=F,caption = "Evolución parámetros") %>%
    pack_rows(index = table(df_res$variable)) %>%
    add_footnote(paste0("N  <br> mean(sd)    <br> [CI 95% mean]   <br> median[IQR]"), escape = F,    notation = "symbol")

}
