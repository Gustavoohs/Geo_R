# Pacotes
library(raster)
library(RStoolbox)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)

# Leitura da imagem
land7 <- brick("C:/Users/Gustavo/Downloads/L71221071_07120010720_DN.tif")

# Calculando o Tasseled Cap (sem a banda termal)
lsat_tc <- tasseledCap(land7, sat = "Landsat7ETM")

# Calculando o NDVI: (NIR - Red) / (NIR + Red)
ndvi <- (land7[[4]] - land7[[3]]) / (land7[[4]] + land7[[3]])
names(ndvi) <- "NDVI"

# Calculando o NDWI: (Green - NIR) / (Green + NIR)
ndwi <- (land7[[2]] - land7[[4]]) / (land7[[2]] + land7[[4]])
names(ndwi) <- "NDWI"

# Calculando o NDMI: (NIR - SWIR) / (NIR + SWIR)
ndmi <- (land7[[4]] - land7[[5]]) / (land7[[4]] + land7[[5]])
names(ndmi) <- "NDMI"

# Adicionando NDVI e NDWI ao Tasseled Cap para comparação
lsat_combined <- stack(lsat_tc, ndvi, ndwi, ndmi)

# Transformando resultado em dataframe
dt <- as.data.frame(lsat_combined, xy = TRUE)

# Comparando NDVI com Greenness
plt1 <- ggplot(data = dt, aes(x = NDVI, y = greenness)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  labs(title = "NDVI vs Greenness", x = "NDVI", y = "Greenness")

# Comparando NDWI com Wetness
plt2 <- ggplot(data = dt, aes(x = NDWI, y = wetness)) +
  geom_point(alpha = 0.3, color = "blue") +
  labs(title = "NDWI vs Wetness", x = "NDWI", y = "Wetness")

# Comparando NDMI com Wetness
plt3 <- ggplot(data = dt, aes(x = NDMI, y = wetness)) +
  geom_point(alpha = 0.3, color = "purple") +
  labs(title = "NDMI vs Wetness", x = "NDMI", y = "Wetness")

# Atualizando o layout para exibir todos os gráficos juntos
grid.arrange(plt1, plt2, plt3, ncol = 3)

# Funções para métricas quantitativas
rmse <- function(x, y) {
  sqrt(mean((x - y)^2, na.rm = TRUE))
}

r_squared <- function(x, y) {
  cor(x, y, use = "complete.obs")^2
}

# Calculando métricas
# Correlação de Pearson
cor_ndvi_greenness <- cor(dt$NDVI, dt$greenness, use = "complete.obs")
cor_ndwi_wetness <- cor(dt$NDWI, dt$wetness, use = "complete.obs")
cor_ndmi_wetness <- cor(dt$NDMI, dt$wetness, use = "complete.obs")

# RMSE
rmse_ndvi_greenness <- rmse(dt$NDVI, dt$greenness)
rmse_ndwi_wetness <- rmse(dt$NDWI, dt$wetness)
rmse_ndmi_wetness <- rmse(dt$NDMI, dt$wetness)

# R²
r2_ndvi_greenness <- r_squared(dt$NDVI, dt$greenness)
r2_ndwi_wetness <- r_squared(dt$NDWI, dt$wetness)
r2_ndmi_wetness <- r_squared(dt$NDMI, dt$wetness)

# Exibindo os resultados
cat("Métricas de comparação:\n")
cat("NDVI vs Greenness:\n")
cat(sprintf("  Correlação de Pearson: %.3f\n", cor_ndvi_greenness))
cat(sprintf("  RMSE: %.3f\n", rmse_ndvi_greenness))
cat(sprintf("  R²: %.3f\n", r2_ndvi_greenness))

cat("\nNDWI vs Wetness:\n")
cat(sprintf("  Correlação de Pearson: %.3f\n", cor_ndwi_wetness))
cat(sprintf("  RMSE: %.3f\n", rmse_ndwi_wetness))
cat(sprintf("  R²: %.3f\n", r2_ndwi_wetness))

cat("\nNDMI vs Wetness:\n")
cat(sprintf("  Correlação de Pearson: %.3f\n", cor_ndmi_wetness))
cat(sprintf("  RMSE: %.3f\n", rmse_ndmi_wetness))
cat(sprintf("  R²: %.3f\n", r2_ndmi_wetness))

# Função para testar a normalidade
test_normality <- function(x) {
  shapiro <- shapiro.test(x)
  p_value <- shapiro$p.value
  result <- if (p_value > 0.05) "Normal" else "Não Normal"
  return(list(p_value = p_value, result = result))
}

# Aplicando aos dados
normality_ndvi <- test_normality(sample(dt$NDVI, size = 5000))
normality_greenness <- test_normality(sample(dt$greenness, size = 5000))
normality_ndwi <- test_normality(sample(dt$NDWI, size = 5000))
normality_wetness <- test_normality(sample(dt$wetness, size = 5000))
normality_ndmi <- test_normality(sample(dt$NDMI, size = 5000))

# Exibindo os resultados
cat("Teste de Normalidade (Shapiro-Wilk):\n")
cat(sprintf("NDVI: p-valor = %.3f (%s)\n", normality_ndvi$p_value, normality_ndvi$result))
cat(sprintf("Greenness: p-valor = %.3f (%s)\n", normality_greenness$p_value, normality_greenness$result))
cat(sprintf("NDWI: p-valor = %.3f (%s)\n", normality_ndwi$p_value, normality_ndwi$result))
cat(sprintf("Wetness: p-valor = %.3f (%s)\n", normality_wetness$p_value, normality_wetness$result))
cat(sprintf("NDMI: p-valor = %.3f (%s)\n", normality_ndmi$p_value, normality_ndmi$result))

# Função para MAE
mae <- function(x, y) {
  mean(abs(x - y), na.rm = TRUE)
}

# Correlação de Spearman
spearman_ndvi_greenness <- cor(dt$NDVI, dt$greenness, method = "spearman", use = "complete.obs")
spearman_ndwi_wetness <- cor(dt$NDWI, dt$wetness, method = "spearman", use = "complete.obs")
spearman_ndmi_wetness <- cor(dt$NDMI, dt$wetness, method = "spearman", use = "complete.obs")

# MAE
mae_ndvi_greenness <- mae(dt$NDVI, dt$greenness)
mae_ndwi_wetness <- mae(dt$NDWI, dt$wetness)
mae_ndmi_wetness <- mae(dt$NDMI, dt$wetness)

# Exibindo os resultados
cat("Métricas baseadas em distribuições não normais:\n")
cat("NDVI vs Greenness:\n")
cat(sprintf("  Correlação de Spearman: %.3f\n", spearman_ndvi_greenness))
cat(sprintf("  MAE: %.3f\n", mae_ndvi_greenness))

cat("\nNDWI vs Wetness:\n")
cat(sprintf("  Correlação de Spearman: %.3f\n", spearman_ndwi_wetness))
cat(sprintf("  MAE: %.3f\n", mae_ndwi_wetness))

cat("\nNDMI vs Wetness:\n")
cat(sprintf("  Correlação de Spearman: %.3f\n", spearman_ndmi_wetness))
cat(sprintf("  MAE: %.3f\n", mae_ndmi_wetness))

# Gráficos para Greenness e NDVI
par(mfrow = c(1, 1), mar = c(4, 4, 4, 6)) # Layout para um gráfico por vez, com margens ajustadas
plot(lsat_combined$greenness, 
     main = "Greenness", 
     col = rev(brewer.pal(11, "Greens")), 
     legend.args = list(text = 'Greenness', side = 4, line = 2.5))
plot(lsat_combined$NDVI, 
     main = "NDVI", 
     col = rev(brewer.pal(11, "YlGn")), 
     legend.args = list(text = 'NDVI', side = 4, line = 2.5))

# Gráficos para Wetness e NDWI
plot(lsat_combined$wetness, 
     main = "Wetness", 
     col = rev(brewer.pal(11, "Blues")), 
     legend.args = list(text = 'Wetness', side = 4, line = 2.5))
plot(lsat_combined$NDWI, 
     main = "NDWI", 
     col = rev(brewer.pal(11, "PuBuGn")), 
     legend.args = list(text = 'NDWI', side = 4, line = 2.5))

# Gráfico para NDMI
plot(lsat_combined$NDMI, 
     main = "NDMI", 
     col = rev(brewer.pal(11, "Oranges")), 
     legend.args = list(text = 'NDMI', side = 4, line = 2.5))
# Restaurando layout padrão para futuros gráficos
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
