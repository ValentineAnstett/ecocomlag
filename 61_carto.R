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


species_cols <- names(Macro_Ptscontacts_GPS)[!(names(Macro_Ptscontacts_GPS) %in% c("Annee", "Site", "ID_LAG", "Lon", "Lat"))]

agg_by_year <- Macro_Ptscontacts_GPS %>%
  group_by(ID_LAG, Annee) %>%
  summarise(
    across(all_of(species_cols), ~ max(., na.rm = TRUE)),
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
      Annee == 2020 ~ "red",
      Annee == 2025 ~ "blue",
      TRUE ~ "gray"
    ),
    # Jitter horizontal : décalage fixe +/- 0.0002 selon l'année
    Lon_jittered = case_when(
      Annee == 2020 ~ Lon - 0.0002,
      Annee == 2025 ~ Lon + 0.0002,
      TRUE ~ Lon
    )
  ) %>%
  ungroup()

carto_macrophytes = leaflet(agg_by_year) %>%
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
    colors = c("red", "blue"),
    labels = c("2020", "2025"),
    title = "Année"
  )

# Export en HTML
htmlwidgets::saveWidget(carto_macrophytes, "carto_macrophytes.html", selfcontained = TRUE)
