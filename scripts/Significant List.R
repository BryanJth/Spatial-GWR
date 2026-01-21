# Library
library(readxl)
library(dplyr)

data <- read_excel("Downloads/Datapasi.xlsx", sheet = 'Sheet2')
data$RA <- as.numeric(data$RA)

vars <- c("X1", "X2", "X3", "X4", "AH", "PR", "AM", "SL", "AK", "RA", "PM", "HS", "ROKOK")
combinations <- unlist(lapply(1:length(vars), function(x) combn(vars, x, simplify = FALSE)), recursive = FALSE)
model_results <- list()

# Loop 
for (i in seq_along(combinations)) {
  formula_text <- paste("Y ~", paste(combinations[[i]], collapse = " + "))
  model <- lm(as.formula(formula_text), data = data)
  summary_model <- summary(model)
  p_values <- coef(summary_model)[-1, 4]
  signif_count <- sum(p_values < 0.05)
  model_results[[i]] <- list(
    formula = formula_text,
    n_variables = length(combinations[[i]]),
    n_significant = signif_count
  )
}

model_summary <- do.call(rbind, lapply(model_results, as.data.frame))
model_summary <- model_summary %>%
  mutate(ratio_significant = n_significant / n_variables) %>%
  arrange(desc(n_significant), desc(ratio_significant))
print(model_summary)
