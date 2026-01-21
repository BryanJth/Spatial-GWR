# Library
library(readxl)
library(dplyr)
library(sf)
library(GWmodel)
library(sp)
library(car)
library(lmtest)
library(ggplot2)
library(corrplot)
library(GGally)
library(RColorBrewer)

# Load Data
data <- read_excel("C:\\Users\\Bryan\\Downloads\\Data Tugas 3(2).xlsx", sheet = "Sheet3")
data$longitude <- as.numeric(gsub(",", ".", data$longitude))
data$latitude <- as.numeric(gsub(",", ".", data$latitude))

# Load Shapefile
shapefile <- st_read("C:\\Users\\Bryan\\Downloads\\[geosai.my.id]Jawa_Timur_Kab\\Jawa_Timur_ADMIN_BPS.shp")
shapefile <- shapefile %>%
  left_join(data, by = "Kabupaten") %>%
  mutate(centroid = st_centroid(geometry),
         x = st_coordinates(centroid)[, 1],
         y = st_coordinates(centroid)[, 2])


plot(shapefile["geometry"], col = "grey", axes = TRUE)
points(data$longitude, data$latitude)
text(data$longitude, data$latitude, labels = data$Kabupaten, pos = 4, cex = 0.7)

# Korelasi dan pairplot
data_numeric <- dplyr::select(data, Y, X1, X2, X3, X4)
cor_matrix <- cor(data_numeric)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black")
ggpairs(data_numeric)

# Boxplot dan peta variabel
vars <- c("Y", "X1", "X2", "X3", "X4")
for (v in vars) {
  ggplot(shapefile) +
    geom_sf(aes_string(fill = v)) +
    scale_fill_gradient(low = "yellow", high = "darkgreen") +
    labs(title = paste("Peta", v)) +
    theme_minimal()
  
  boxplot(data[[v]], main = paste("Boxplot", v), col = "lightblue")
}

# Model OLS
ols <- lm(Y ~ X1 + X2 + X3 + X4, data = data)
summary(ols)
AIC(ols)

# Asumsi OLS
shapiro.test(residuals(ols))
qqnorm(residuals(ols), main = "Q-Q Plot Residual OLS")
qqline(residuals(ols), col = "blue", lwd = 2)
vif(ols)
dwtest(ols)
bptest(ols)

coords <- data[, c("longitude", "latitude")]
Data.spdf <- SpatialPointsDataFrame(coords, data)
DM <- gw.dist(dp.locat = coordinates(Data.spdf))
DM

# Variabel dependen dan independen
DeVar <- "Y"
InDeVars <- c("X1", "X2", "X3", "X4")

# Gaussian
model.sel.gaus <- model.selection.gwr(DeVar, InDeVars, data = Data.spdf,
                                      kernel = "gaussian", approach = "CV", bw = 1, dMat = DM)
sorted.models.gaus <- model.sort.gwr(model.sel.gaus, numVars = length(InDeVars),
                                     ruler.vector = model.sel.gaus[[2]][, 2])
model.view.gwr(DeVar, InDeVars, model.list = sorted.models.gaus[[1]])
plot(sorted.models.gaus[[2]][, 2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection (Gaussian)", ylab = "CV", xlab = "Model number", type = "b")

# Bisquare
model.sel.bisq <- model.selection.gwr(DeVar, InDeVars, data = Data.spdf,
                                      kernel = "bisquare", approach = "CV", bw = 1, dMat = DM)
sorted.models.bisq <- model.sort.gwr(model.sel.bisq, numVars = length(InDeVars),
                                     ruler.vector = model.sel.bisq[[2]][, 2])
model.view.gwr(DeVar, InDeVars, model.list = sorted.models.bisq[[1]])
plot(sorted.models.bisq[[2]][, 2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection (Bisquare)", ylab = "CV", xlab = "Model number", type = "b")

# Tricube
model.sel.tric <- model.selection.gwr(DeVar, InDeVars, data = Data.spdf,
                                      kernel = "tricube", approach = "CV", bw = 1, dMat = DM)
sorted.models.tric <- model.sort.gwr(model.sel.tric, numVars = length(InDeVars),
                                     ruler.vector = model.sel.tric[[2]][, 2])
model.view.gwr(DeVar, InDeVars, model.list = sorted.models.tric[[1]])
plot(sorted.models.tric[[2]][, 2], col = "black", pch = 20, lty = 5,
     main = "Alternative view of GWR model selection (Tricube)", ylab = "CV", xlab = "Model number", type = "b")

# Bandwidth untuk semua kernel
bw.gauss <- bw.gwr(Y ~ X1 + X2 + X3 + X4, data = Data.spdf, kernel = "gaussian", approach = "CV")
bw.bisq <- bw.gwr(Y ~ X1 + X2 + X3 + X4, data = Data.spdf, kernel = "bisquare", approach = "CV")
bw.tric <- bw.gwr(Y ~ X1 + X2 + X3 + X4, data = Data.spdf, kernel = "tricube", approach = "CV")

bw.gauss
bw.bisq
bw.tric

# Estimasi GWR
gwr.gaus <- gwr.basic(Y ~ X1 + X2 + X3 + X4, data = Data.spdf, bw = bw.gauss, kernel = "gaussian")
gwr.bisq <- gwr.basic(Y ~ X1 + X2 + X3 + X4, data = Data.spdf, bw = bw.bisq, kernel = "bisquare")
gwr.tric <- gwr.basic(Y ~ X1 + X2 + X3 + X4, data = Data.spdf, bw = bw.tric, kernel = "tricube")

# Ringkasan hasil
result_table <- data.frame(
  Model = c("OLS", "GWR Gaussian", "GWR Bisquare", "GWR Tricube"),
  R_Squared = c(summary(ols)$r.squared,
                gwr.gaus$GW.diagnostic$gw.R2,
                gwr.bisq$GW.diagnostic$gw.R2,
                gwr.tric$GW.diagnostic$gw.R2),
  AIC = c(AIC(ols),
          gwr.gaus$GW.diagnostic$AIC,
          gwr.bisq$GW.diagnostic$AIC,
          gwr.tric$GW.diagnostic$AIC)
)
print(result_table)

# Residual OLS
resid_ols <- data.frame(Kabupaten = data$Kabupaten, res_ols = residuals(ols))

bisqcap <- data.frame(
  X1_coef = gwr.bisq$SDF$X1,
  X2_coef = gwr.bisq$SDF$X2,
  X3_coef = gwr.bisq$SDF$X3,
  X4_coef = gwr.bisq$SDF$X4,
  residual_bisq = gwr.bisq$SDF$residual
)

pvalue_bisq <- data.frame(
  Intercept_p = 2 * (1 - pnorm(abs(gwr.bisq$SDF$Intercept / gwr.bisq$SDF$Intercept_SE))),
  X1_p = 2 * (1 - pnorm(abs(gwr.bisq$SDF$X1 / gwr.bisq$SDF$X1_SE))),
  X2_p = 2 * (1 - pnorm(abs(gwr.bisq$SDF$X2 / gwr.bisq$SDF$X2_SE))),
  X3_p = 2 * (1 - pnorm(abs(gwr.bisq$SDF$X3 / gwr.bisq$SDF$X3_SE))),
  X4_p = 2 * (1 - pnorm(abs(gwr.bisq$SDF$X4 / gwr.bisq$SDF$X4_SE)))
)

resid_data <- data.frame(Kabupaten = data$Kabupaten, res_ols = residuals(ols))

bisqtab <- cbind(
  data[, c("Kabupaten", "Y")],
  bisqcap,
  resid_data,
  pvalue_bisq
)

bisqtab$ID <- 1:nrow(bisqtab)
shapefile$ID <- 1:nrow(shapefile)

bisqtab <- bisqtab %>% dplyr::select(-Kabupaten)

merged_data_bisq <- merge(shapefile, bisqtab, by = "ID", all.x = TRUE)

merged_data_bisq <- merged_data_bisq %>% dplyr::select(-ID)

# Klasifikasi kelompok berdasarkan p-value
merged_data_bisq$Kelompok <- ifelse(
  merged_data_bisq$X1_p < 0.05 & merged_data_bisq$X2_p < 0.05 & merged_data_bisq$X3_p < 0.05 & merged_data_bisq$X4_p < 0.05, 5,
  ifelse(merged_data_bisq$X1_p < 0.05 & merged_data_bisq$X2_p < 0.05 & merged_data_bisq$X3_p < 0.05, 4,
         ifelse(merged_data_bisq$X1_p < 0.05 & merged_data_bisq$X3_p < 0.05, 3,
                ifelse(merged_data_bisq$X2_p < 0.05 & merged_data_bisq$X3_p < 0.05, 2,
                       ifelse(merged_data_bisq$X3_p < 0.05, 1, 0)))))

# Peta Residual OLS
ggplot() + 
  geom_sf(data = merged_data_bisq, aes(fill = res_ols)) + 
  scale_fill_gradient2(low = "red", high = "royalblue", midpoint = 0) +
  theme_minimal() + 
  ggtitle("Residual OLS")

# Minimum dan maksimum residual OLS
print(min(merged_data_bisq$res_ols, na.rm = TRUE))
print(max(merged_data_bisq$res_ols, na.rm = TRUE))

# Peta Residual GWR Bisquare
ggplot() + 
  geom_sf(data = merged_data_bisq, aes(fill = residual_bisq)) + 
  scale_fill_gradient2(low = "red", high = "royalblue", midpoint = 0) +
  theme_minimal() + 
  ggtitle("Residual GWR Bisquare")

# Minimum dan maksimum residual GWR
print(min(merged_data_bisq$residual_bisq, na.rm = TRUE))
print(max(merged_data_bisq$residual_bisq, na.rm = TRUE))

# Peta klasifikasi kelompok (berdasarkan p-value)
ggplot() + 
  geom_sf(data = merged_data_bisq, aes(fill = factor(Kelompok))) + 
  scale_fill_manual(
    values = c("white", "darkgreen", "orange", "yellow", "pink", "red"),
    breaks = c(0, 1, 2, 3, 4, 5),
    labels = c(
      "Tidak Ada Signifikan",
      "X3 Signifikan",
      "X2 & X3 Signifikan",
      "X1 & X3 Signifikan",
      "X1, X2, & X3 Signifikan",
      "X1, X2, X3, & X4 Signifikan"
    )
  ) +
  labs(fill = "Kelompok Signifikan") +
  ggtitle("Peta Kelompok Berdasarkan Variabel Signifikan (P-value < 0.05)") +
  theme_minimal()



# Koefisien
for (v in c("X1_coef", "X2_coef", "X3_coef", "X4_coef")) {
  plot1 <- ggplot() +
    geom_sf(data = merged_data_bisq, aes_string(fill = v)) +
    scale_fill_gradient(low = "white", high = "purple") +
    ggtitle(paste("Koefisien", v)) +
    theme_minimal()
  print(plot1)
}


for (p in c("X1_p", "X2_p", "X3_p", "X4_p")) {
  plot <- ggplot() +
    geom_sf(data = merged_data_bisq, aes_string(fill = p)) +
    scale_fill_gradient2(low = "white", high = "orange", midpoint = 0.05) +
    ggtitle(paste("P-value", p)) +
    theme_minimal()
  
  print(plot)
}

