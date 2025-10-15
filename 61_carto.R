getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_GPS")
Macro_Ptscontacts_GPS= read.csv("Macro_Ptscontacts_GPS.csv", header = TRUE, sep = ",", dec=".")

Macro_Ptscontacts_GPS <- Macro_Ptscontacts_GPS %>%
  mutate(
    Lon = as.numeric(gsub(",", ".", as.character(Lon))),
    Lat = as.numeric(gsub(",", ".", as.character(Lat)))
  )

Macro_Ptscontacts_GPS <- Macro_Ptscontacts_GPS %>%
  mutate(
    Lon = as.numeric(as.character(Lon)),
    Lat = as.numeric(as.character(Lat))
  )


species_cols = names(Macro_Ptscontacts_GPS)[!(names(Macro_Ptscontacts_GPS) %in% c("Annee", "Site", "ID_LAG", "Lon", "Lat"))]

agg_by_year <- Macro_Ptscontacts_GPS %>%
  group_by(ID_LAG, Annee) %>%
  summarise(
    across(
      all_of(species_cols),
      ~ if (all(is.na(.))) NA_real_ else max(., na.rm = TRUE)
    ),
    Lon = mean(as.numeric(gsub(",", ".", as.character(Lon))), na.rm = TRUE),
    Lat = mean(as.numeric(gsub(",", ".", as.character(Lat))), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    especes_presentes = paste(
      names(across(all_of(species_cols)))[c_across(all_of(species_cols)) > 0],
      collapse = ", "
    ),
    couleur = case_when(
      Annee == 2020 ~ "#F8766D",
      Annee == 2025 ~ "#00BFC4",
      TRUE ~ "gray"
    ),
    Lon_jittered = case_when(
      Annee == 2020 ~ Lon - 0.0002,
      Annee == 2025 ~ Lon + 0.0002,
      TRUE ~ Lon
    )
  ) %>%
  ungroup()


carto_macrophytes <- leaflet(agg_by_year %>% filter(!is.na(Lat), !is.na(Lon_jittered))) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~Lon_jittered,
    lat = ~Lat,
    radius = 6,
    color = ~couleur,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<strong>ID_LAG : </strong>", ID_LAG, "<br>",
      "<strong>Année : </strong>", Annee, "<br>",
      "<strong>Espèces présentes : </strong>", especes_presentes
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors = c("#F8766D", "#00BFC4"),
    labels = c("2020", "2025"),
    title = "Année"
  )

carto_macrophytes

# Export en HTML
htmlwidgets::saveWidget(carto_macrophytes, "carto_macrophytes.html", selfcontained = TRUE)




#### Carte article -----
sites_coord <- Macro_Ptscontacts_GPS %>%
  group_by(Site) %>%
  summarise(
    Lon = mean(as.numeric(gsub(",", ".", as.character(Lon))), na.rm = TRUE),
    Lat = mean(as.numeric(gsub(",", ".", as.character(Lat))), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Lon), !is.na(Lat))

# Villes majeures
villes_maj <- data.frame(
  nom = c("Marseille", "Montpellier", "Perpignan"),
  lat = c(43.2965, 43.6119, 42.6887),
  lon = c(5.3698, 3.8777, 2.8948)
)

# Séparer les sites selon position des labels
sites_label_top <- sites_coord %>% filter(!Site %in% c("LAP_SAL", "ORB_ORP", "PAL_FRO"))
sites_label_bottom <- sites_coord %>% filter(Site %in% c("LAP_SAL", "ORB_ORP", "PAL_FRO"))

# Carte
carto_sites_simple <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addCircleMarkers(
    data = sites_coord,
    lng = ~Lon,
    lat = ~Lat,
    radius = 6,
    color = "#1B263B",
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0("<strong>Site : </strong>", Site)
  ) %>%
  # Labels au-dessus
  addLabelOnlyMarkers(
    data = sites_label_top,
    lng = ~Lon,
    lat = ~Lat,
    label = ~Site,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'top',
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size" = "14px",
        "color" = "#1B263B"
      )
    )
  ) %>%
  # Labels en dessous
  addLabelOnlyMarkers(
    data = sites_label_bottom,
    lng = ~Lon,
    lat = ~Lat,
    label = ~Site,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'bottom',
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size" = "14px",
        "color" = "#1B263B"
      )
    )
  ) %>%
  # Villes majeures avec labels plus gros
  addLabelOnlyMarkers(
    data = villes_maj,
    lng = ~lon,
    lat = ~lat,
    label = ~nom,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = 'top',
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size" = "18px",
        "color" = "#333333"
      )
    )
  ) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
  addControl(
    html = "<img src='https://raw.githubusercontent.com/deldersveld/topographic-map-symbols/master/north-arrow.png' height='50'>",
    position = "topright"
  )

carto_sites_simple


carte_globale = leaflet(sites_coord) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)

carte_globale
