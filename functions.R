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
    geom_sf(data = Can, color = "grey70") +
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
    geom_sf(data = Can, color = "grey70") +
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