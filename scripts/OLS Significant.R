# Library
library(readxl)
library(corrplot)
library(dplyr)
library(ggcorrplot)
library(GGally)
library(car)
library(lmtest)
library(sf)
library(GWmodel)
library(RColorBrewer)
library(writexl)
library(sp)
library(ggplot2)

data <- read_excel("C:\\Users\\Bryan\\Downloads\\Data Tugas 3(2).xlsx", sheet = "Sheet3")


# Model awal
model <- lm(Y ~ X1 + X2 + X3 + X4, data = data)
summary(model)

# Backward elimination dengan BIC
n <- nrow(data)  # Jumlah observasi
model_BIC <- step(model, direction = "both", k = log(n))
summary(model_BIC)
