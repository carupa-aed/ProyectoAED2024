coma_to_num <- function(x) {
  dotless_x <- gsub("\\.", "", x)
  num <- as.numeric(sub(",", ".", dotless_x))
  return(num)
}



spain_map <- function(data, periodo, title, edad = "Total", sexo = "Ambos sexos", label_title = 'Total', label_color = 'Blues') {
  
  # limpieza de datos
  clean_data <- data[data$Sexo == sexo & data$Periodo == periodo & data$Edad == edad & data$Comunidades.y.Ciudades.Autónomas != "Total Nacional", c("Comunidades.y.Ciudades.Autónomas", "Total")]
  
  clean_data <- clean_data %>% separate(Comunidades.y.Ciudades.Autónomas, into = c("codauto", "ine.ccaa.name"), sep = " ", extra = "merge")
  
  clean_data <- clean_data %>% mutate_at("Total", coma_to_num)
  
  # mapa españa
  CCAA <- esp_get_ccaa()
  CCAA <- merge(CCAA, clean_data)
  Box <- esp_get_can_box() # Caja alrededor de Canarias
  
  ggplot(CCAA) +
    geom_sf(aes(fill = Total),
            color = "grey70",
            lwd = .3
    ) +
    ggtitle(title) +
    geom_sf(data = Box, color = "grey70") +
    geom_sf_label(aes(label = Total),
                  fill = "white", alpha = 0.5,
                  size = 3,
                  label.size = 0
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, label_color, rev = TRUE),
      n.breaks = 10,
      guide = guide_legend(title = label_title)
    ) +
    theme_void() +
    theme(legend.position = c(0.1, 0.6), plot.title = element_text(size = 17, hjust = 0.5))
}



spain_map_relative <- function(data, rel_data, periodo, title, edad = "Total", sexo = "Ambos sexos", label_title = "Porc.", label_color = 'Blues') {
  
  # rel_data debe ser un dataframe con columnas:
  # · "Comunidades.y.Ciudades.Autónomas"
  # · "Total_rel"
  
  # limpieza de datos
  clean_data <- data[data$Sexo == sexo & data$Periodo == periodo & data$Edad == edad & data$Comunidades.y.Ciudades.Autónomas != "Total Nacional", c("Comunidades.y.Ciudades.Autónomas", "Total")]
  
  clean_data <- merge(clean_data, rel_data)
  
  clean_data <- clean_data %>% separate(Comunidades.y.Ciudades.Autónomas, into = c("codauto", "ine.ccaa.name"), sep = " ", extra = "merge")
  
  clean_data <- clean_data %>% mutate_at(c("Total", "Total_rel"), coma_to_num)
  
  clean_data$Porc <- clean_data$Total / clean_data$Total_rel
  clean_data$Porc_lab <- paste0(round(100 * clean_data$Porc, 2), "%")
  
  # mapa españa
  CCAA <- esp_get_ccaa()
  CCAA <- merge(CCAA, clean_data)
  Box <- esp_get_can_box() # Caja alrededor de Canarias
  
  ggplot(CCAA) +
    geom_sf(aes(fill = Porc),
            color = "grey70",
            lwd = .3
    )  +
    ggtitle(title) +
    geom_sf(data = Box, color = "grey70") +
    geom_sf_label(aes(label = Porc_lab),
                  fill = "white", alpha = 0.5,
                  size = 3,
                  label.size = 0
    ) +
    scale_fill_gradientn(
      colors = hcl.colors(10, label_color, rev = TRUE),
      n.breaks = 10,
      labels = function(x) {
        sprintf("%1.1f%%", 100 * x)
      },
      guide = guide_legend(title = label_title)
    ) +
    theme_void() +
    theme(legend.position = c(0.1, 0.6), plot.title = element_text(size = 17, hjust = 0.5))
}




# Función para calcular clustering y generar gráficos y mapas
crear_clustering_grafico_mapa <- function(tasa, mapa_comunidades, anio) {
  
  # Calcular la media de la tasa seleccionada por comunidad para el año dado
  dataset_anio <- datos %>%
    filter(format(Periodo, "%Y") == anio) %>%
    group_by(Codigo, `Comunidades y Ciudades Autónomas`) %>%
    summarise(TasaMedia = mean(.data[[tasa]], na.rm = TRUE), .groups = "drop")
  
  # Escalar la tasa
  dataset_scaled <- scale(dataset_anio$TasaMedia)
  
  # Aplicar clustering
  set.seed(123)
  kmeans_result <- kmeans(dataset_scaled, centers = 3)
  
  # Añadir los clusters al dataset
  dataset_anio$Cluster <- as.factor(kmeans_result$cluster)
  
  # Reordenar los clusters basándose en los valores de TasaMedia
  cluster_ordenado <- dataset_anio %>%
    group_by(Cluster) %>%
    summarise(TasaPromedio = mean(TasaMedia)) %>%
    arrange(TasaPromedio) %>%
    mutate(ClusterOrdenado = factor(Cluster, levels = Cluster[order(TasaPromedio)]))
  
  # Mapear los nuevos clusters ordenados al dataset original
  dataset_anio <- dataset_anio %>%
    left_join(cluster_ordenado, by = "Cluster") %>%
    mutate(Cluster = ClusterOrdenado)
  
  # Crear gráfico de barras agrupadas por cluster
  grafico_barras <- ggplot(dataset_anio, aes(x = reorder(`Comunidades y Ciudades Autónomas`, TasaMedia), 
                                             y = TasaMedia, fill = Cluster)) +
    geom_col() +
    geom_text(aes(label = round(TasaMedia, 1)), # Añadir los valores
              hjust = 1, # Ajustar la posición del texto fuera de la barra
              size = 3) +  # Tamaño del texto
    coord_flip() +
    labs(
      x = "Comunidades Autónomas",
      y = paste(tasa, "(%)"),
      fill = "Cluster"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Unir el dataset con el GeoJSON
  mapa <- mapa_comunidades %>%
    left_join(dataset_anio, by = c("cod_ccaa" = "Codigo"))
  
  # Crear el mapa
  mapa_clustering <- ggplot(mapa) +
    geom_sf(aes(fill = Cluster)) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Combinar ambos gráficos con patchwork
  combinado <- grafico_barras + mapa_clustering + 
    plot_layout(ncol = 2) + 
    plot_annotation(title = paste("Análisis de Clustering y Mapa para ", tasa, " (", anio, ")", sep = ""))
  
  # Mostrar el gráfico combinado
  print(combinado)
}




# Función para calcular el gráfico de codo
grafico_codo <- function(tasa, anio, datos) {
  # Calcular la media de la tasa seleccionada por comunidad para el año dado
  dataset_anio <- datos %>%
    filter(format(Periodo, "%Y") == anio) %>%
    group_by(Codigo, `Comunidades y Ciudades Autónomas`) %>%
    summarise(TasaMedia = mean(.data[[tasa]], na.rm = TRUE), .groups = "drop")
  
  # Escalar la tasa
  dataset_scaled <- scale(dataset_anio$TasaMedia)
  
  # Calcular WSS para un rango de k
  k_range <- 1:10  # Número máximo de clusters a evaluar
  wss <- sapply(k_range, function(k) {
    kmeans(dataset_scaled, centers = k, nstart = 10)$tot.withinss
  })
  
  # Crear un data frame para graficar
  codo_data <- data.frame(Clusters = k_range, WSS = wss)
  
  # Generar el gráfico de codo
  codo <- ggplot(codo_data, aes(x = Clusters, y = WSS)) +
    geom_line(size = 1, color = "steelblue") +
    geom_point(size = 3, color = "darkblue") +
    labs(
      title = paste("Gráfico de Codo para", tasa, "(", anio, ")", sep = " "),
      x = "Número de Clusters (k)",
      y = "Suma de Cuadrados Dentro del Cluster (WSS)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
  print(codo)
}




# Función para calcular el gráfico de codo usando la media global
grafico_codo_global <- function(tasa, datos) {
  # Calcular la media de la tasa seleccionada por comunidad para todos los años
  dataset_global <- datos %>%
    group_by(Codigo, `Comunidades y Ciudades Autónomas`) %>%
    summarise(TasaMedia = mean(.data[[tasa]], na.rm = TRUE), .groups = "drop")
  
  # Escalar la tasa
  dataset_scaled <- scale(dataset_global$TasaMedia)
  
  # Calcular WSS para un rango de k
  k_range <- 1:10  # Número máximo de clusters a evaluar
  wss <- sapply(k_range, function(k) {
    kmeans(dataset_scaled, centers = k, nstart = 10)$tot.withinss
  })
  
  # Crear un data frame para graficar
  codo_data <- data.frame(Clusters = k_range, WSS = wss)
  
  # Generar el gráfico de codo
  codo <- ggplot(codo_data, aes(x = Clusters, y = WSS)) +
    geom_line(size = 1, color = "steelblue") +
    geom_point(size = 3, color = "darkblue") +
    labs(
      title = paste("Gráfico de Codo para la", tasa),
      x = "Número de Clusters (k)",
      y = "Suma de Cuadrados Dentro del Cluster (WSS)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.text = element_text(size = 10)
    )
  
  print(codo)
}