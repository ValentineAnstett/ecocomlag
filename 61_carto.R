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
  group_by(ID_LAG, Site, Annee) %>%  # ← Ajout de Site ici
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
    ),
    label = ~paste0("Site : ", Site),  # ← ici, ajout du label
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "auto",
      textsize = "13px",
      style = list("font-weight" = "bold", "color" = "#333")
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
# --- Lire et préparer le shapefile des côtes ---
cotes_lines <- st_read("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Terre_Mer/SHAPE/Limite_terre-mer_facade_Mediterranee_ligne.shp") %>%
  st_transform(4326)  # reprojection WGS84 pour Leaflet

# --- Préparer les coordonnées des sites ---
sites_coord <- Macro_Ptscontacts_GPS %>%
  group_by(Site) %>%
  summarise(
    Lon = mean(as.numeric(gsub(",", ".", as.character(Lon))), na.rm = TRUE),
    Lat = mean(as.numeric(gsub(",", ".", as.character(Lat))), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(Lon), !is.na(Lat))

sites_label_top <- sites_coord %>% filter(!Site %in% c("A", "C", "F"))
sites_label_bottom <- sites_coord %>% filter(Site %in% c("A", "C", "F"))

# --- Villes majeures ---
villes_maj <- data.frame(
  nom = c("Marseille", "Montpellier", "Perpignan"),
  lat = c(43.2965, 43.6119, 42.6887),
  lon = c(5.3698, 3.8777, 2.8948)
)

# --- Groupes de sites ---
sites_croix <- c("C", "D", "H", "I", "J")
sites_cercles <- sites_coord %>% filter(!Site %in% sites_croix)
sites_croix_df <- sites_coord %>% filter(Site %in% sites_croix)

carto_sites_simple <- leaflet(sites_coord, options = leafletOptions(zoomControl = FALSE)) %>%
  
  # Fond de carte Esri Shaded Relief
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  
  # Trait de côte noir fin
  addPolylines(data = cotes_lines, color = "#1B263B", weight = 1) %>%
  
  # Cercles pour les autres sites
  addCircleMarkers(
    data = sites_cercles,
    lng = ~Lon,
    lat = ~Lat,
    radius = 6,
    color = "black",
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0("<strong>Site : </strong>", Site)
  ) %>%
  
  # Croix noires sans bulle
  addLabelOnlyMarkers(
    data = sites_croix_df,
    lng = ~Lon,
    lat = ~Lat,
    label = "×",
    labelOptions = labelOptions(
      noHide = TRUE,
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size" = "30px",
        "color" = "black",
        "text-shadow" = "0 0 3px white"
      )
    )
  ) %>%
  
  # Labels au-dessus
  addLabelOnlyMarkers(
    data = sites_label_top,
    lng = ~Lon,
    lat = ~Lat,
    label = ~Site,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size" = "22px",
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
      direction = "bottom",
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size" = "22px",
        "color" = "#1B263B"
      )
    )
  ) %>%
  
  # Villes majeures
  addLabelOnlyMarkers(
    data = villes_maj,
    lng = ~lon,
    lat = ~lat,
    label = ~nom,
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "top",
      textOnly = TRUE,
      style = list(
        "font-weight" = "bold",
        "font-size" = "24px",
        "color" = "#333333"
      )
    )
  ) %>%
  
  # Échelle agrandie
  addScaleBar(
    position = "bottomleft",
    options = scaleBarOptions(
      imperial = FALSE,
      maxWidth = 400,
      updateWhenIdle = FALSE
    )
  )

# Affichage
carto_sites_simple

carte_globale = leaflet(sites_coord) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas)

carte_globale
