
#'
#'

library(stringr)
library(dplyr)


df <- data.frame( x = rnorm(48,10,1),
 y = as.factor(c(rep("1",16), rep("0",32) ) ), match = c(rep(1:16,3) ) )

 library(survival)
 data(lung)
 dat <- lung
  names(dat)
  Hmisc::label(dat, self = F) <- paste0("label", names(dat))
  dat$sex <- factor(dat$sex)


 m <- list()
 mod <-m$log <- glm(sex~ wt.loss+  status+    age , data = dat, family = binomial)

 mod <-  m$lin <- lm(meal.cal~  wt.loss+  status+    age+  sex, data = dat)
 mod <- m$cox <- coxph(Surv(time, status) ~ sex + wt.loss, data = lung)
 mod <- m$clog <- survival::clogit(as.numeric(y) ~ x + strata(match), data = df)

get.vars(terms(mod))
matches <- stringr::str_c(vars_mod, collapse ="|")
vars_name <- stringr::str_extract_all(df_lasso$feature, matches, simplify = T)[,1]
var_label <- c("Intercept", Hmisc::label(dat[,vars_name[!vars_name %in% ""]]))
levs <- stringr::str_replace_all(df_lasso$feature,vars_name,"")


desc_mod(m$lin, xtab = T)
desc_mod(m$lin, xtab = T,show.pretty=T)


desc_mod(m$log, xtab = T)
desc_mod(m$log, xtab = T,show.pretty=T)





 desc_mod(m$cox, xtab = FALSE,title='OR de los coeficientes', show.intcp = TRUE)

 (prettify(summary(mod), digits = 4))
 desc_mod(mod, xtab = T,title='OR de los coeficientes')

alias(mod)$terms
