# Carregar pacotes necessários
library(terra)
library(sf)
library(ggplot2)

# Caminhos dos arquivos
raster_path <- "C:/Users/Gustavo/Downloads/LST_DF_2023.tif"
vector_path <- "C:/Users/Gustavo/Downloads/pts_LST.gpkg"

# Carregar o raster e o vetor
raster <- rast(raster_path)
points <- st_read(vector_path)

# Garantir que ambos estão no mesmo sistema de coordenadas
points <- st_transform(points, crs = crs(raster))

# Criar uma linha entre os dois pontos
line <- st_sfc(st_linestring(st_coordinates(points)), crs = st_crs(points))

# Amostrar valores ao longo da linha
line_spatial <- vect(line)  # Converter a linha para um objeto 'SpatVector'
profile <- terra::extract(raster, line_spatial, along = TRUE)

# Distância fixa de 100 metros entre os pontos
fixed_distance <- 100
n_points <- nrow(profile)

# Criar as distâncias acumuladas com base no número de pontos extraídos
accumulated_distance <- seq(0, by = fixed_distance, length.out = n_points)

# Organizar os dados do perfil para visualização
profile_df <- data.frame(
  Distance = accumulated_distance,  
  Temp = profile[, 2]  
)

# Converter o raster em um data frame para plotagem
raster_df <- as.data.frame(raster, xy = TRUE)
colnames(raster_df) <- c("x", "y", "Temp")  

# Converter os pontos e a linha para data frame para ggplot
points_df <- as.data.frame(st_coordinates(points))
line_df <- as.data.frame(st_coordinates(line))

# Plot 1: Apenas o raster
plot_raster <- ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = Temp)) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = "Temperatura (°C)", limits = c(26, 50)) +
  labs(
    title = "Land Surface Temperature",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Plot 2: Raster, pontos e linha
plot_combined <- ggplot() +
  geom_raster(data = raster_df, aes(x = x, y = y, fill = Temp)) +
  geom_point(data = points_df, aes(x = X, y = Y), color = "red", size = 3) +
  geom_path(data = line_df, aes(x = X, y = Y), color = "blue", size = 1) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1, name = "Temperatura (°C)", limits = c(26, 50)) +
  labs(
    title = "Área utilizada para geração do perfil",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

# Plot 3: Perfil topográfico com área preenchida abaixo da linha 
plot_profile <- ggplot(profile_df, aes(x = Distance, y = Temp)) +
  geom_ribbon(aes(ymin = 0, ymax = Temp), fill = "orange", alpha = 0.5) + 
  geom_line(color = "orange", size = 1) +
  labs(
    title = "Perfil de temperatura",
    x = "Distância (m)",
    y = "Temperatura (°C)"
  ) +
  theme_minimal()

# Mostrar os plots
print(plot_raster)
print(plot_combined)
print(plot_profile)
