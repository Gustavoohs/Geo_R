# Pacotes
library(sf)
library(spdep)
library(ggplot2)

# Leitura de arquivo
MT <- st_read("C:/Users/Aluga.com/Downloads/MT.geojson")

# Computando vizinhos
nb <- spdep::poly2nb(MT, queen = TRUE)

# Visualizando resultado
plot(st_geometry(MT), border = "lightgray")
plot.nb(nb, st_geometry(MT), add = TRUE)

id <- 126 # id de Sorriso
MT$neighbors <- "Outros"
MT$neighbors[id] <- "Sorriso"
MT$neighbors[nb[[id]]] <- "Vizinhos"
ggplot(MT) + geom_sf(aes(fill = neighbors)) + theme_bw() +
  scale_fill_manual(values = c("white", "#ee0808", "#08adee"))