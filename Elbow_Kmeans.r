# Bibliotecas
library(raster)
library(ggplot2)
library(RColorBrewer)

# Lendo imagem
img <- raster('C:/Users/Documents/SAR_DF_cut.tif')

# Calculando métrica (wss)
K <- 1:10
distortions <- numeric(length(K))
for (k in K) {
  km <- kmeans(img[], centers = k, iter.max = 1000, nstart = 5)
  distortions[k] <- sum(km$withinss)
}

# Visualizando gráfico
ggplot() + geom_line(aes(x = K, y = distortions), color = 'red', size = 2) + xlab('Number of Clusters (K)') + ylab('Distortion')

# Rodando Kmeans
km <- kmeans(img[], centers = 5, iter.max = 1000, nstart = 5)

# Criando imagem com o formato da primeira banda
class <- raster(img[[1]])

# Armazenando clusters kmeans
class <- setValues(class, km$cluster)

# Visualizando resultado
plot(class, col = rev(brewer.pal(n = 5, name = "Paired")))