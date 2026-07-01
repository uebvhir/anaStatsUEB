set.seed(1111)

df <- data.frame(
  id = rep(1:20, each = 2),
  visita = factor(rep(c("Basal", "Seguimiento"), 20)),
  edad = rnorm(40, 65, 10),
  dolor = factor(sample(c("Sí", "No"), 40, replace = TRUE))
)

# Comparación longitudinal básica
desc_group_rep(
  data = df,
  covariates = c(edad, dolor),
  tiempo = visita,
  id = "id"
)

# Mostrando únicamente variables significativas
desc_group_rep(
  data = df,
  covariates = c(edad, dolor),
  tiempo = visita,
  id = "id",
  pval_cut = 0.01
)

# Mostrar p-valores ajustados
desc_group_rep(
  data = df,
  covariates = c(edad, dolor),
  tiempo = visita,
  id = "id",
  show.pval.adj = TRUE
)



data(mtcars)

mtcars$am <- factor(mtcars$am,
                    levels = c(0,1),
                    labels = c("Automatic", "Manual"))

# ROC simple con una variable cuantitativa
res_roc_base <- doROC(
  x = "mpg",
  group = "am",
  dat = mtcars,
  modGLM = FALSE
)

# ROC empleando fórmula
doROC(
  frml = am ~ mpg,
  dat = mtcars,
  modGLM = FALSE
)

# ROC a partir de un modelo logístico
res_roc_glm <- doROC(
  frml = am ~ mpg + wt,
  dat = mtcars,
  modGLM = TRUE
)

# Punto de corte basado en sensibilidad mínima
res_roc <- doROC(
  frml = am ~ mpg,
  dat = mtcars,
  modGLM = FALSE,
  method.cutoff = "MinValueSe",
  sens_target = 0.90
)


bin_data <- mtcars
bin_data$vs <- as.factor(bin_data$vs) # Binomial outcome

bin_res <- desc_lasso(
  var_out = "vs",
  var_comp = c("mpg", "disp", "hp", "drat", "wt", "qsec"),
  data = bin_data,
  lambda_criterion = "lambda.min"
)

plot_desc_lasso(bin_res)
bin_res$table # HTML Table
bin_res$varSel

