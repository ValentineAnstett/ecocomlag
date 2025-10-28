getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_GPS")
Macro_Ptscontacts_GPS= read.csv("Macro_Ptscontacts_GPS.csv", header = TRUE, sep = ",", dec=".")


# === Chargement et préparation des données ===
Macro_Ptscontacts_GPS <- read.csv("Macro_Ptscontacts_GPS.csv", header = TRUE, sep = ",", dec = ",")

Macro_Ptscontacts_GPS <- Macro_Ptscontacts_GPS %>%
  mutate(
    Lon = as.numeric(gsub(",", ".", as.character(Lon))),
    Lat = as.numeric(gsub(",", ".", as.character(Lat)))
  )

species_cols = names(Macro_Ptscontacts_GPS)[!(names(Macro_Ptscontacts_GPS) %in% c("Annee", "Site", "ID_LAG", "Lon", "Lat"))]

# Forcer les colonnes d'espèces à être numériques
Macro_Ptscontacts_GPS <- Macro_Ptscontacts_GPS %>%
  mutate(across(all_of(species_cols), ~ as.numeric(as.character(.))))

# Puis faire ton agrégation
agg_by_year <- Macro_Ptscontacts_GPS %>%
  group_by(ID_LAG, Site, Annee) %>%
  summarise(
    across(
      all_of(species_cols),
      ~ if (all(is.na(.))) NA_real_ else max(., na.rm = TRUE)
    ),
    Lon = mean(Lon, na.rm = TRUE),
    Lat = mean(Lat, na.rm = TRUE),
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

# === Interface utilisateur ===
ui <- fluidPage(
  titlePanel("Carte des macrophytes par année"),
  leafletOutput("macrophytes_map", width = "100%", height = "800px")
)

# === Logique serveur ===
server <- function(input, output, session) {
  
  output$macrophytes_map <- renderLeaflet({
    
    leaflet(agg_by_year %>% filter(!is.na(Lat), !is.na(Lon_jittered))) %>%
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
      addLabelOnlyMarkers(
        lng = ~Lon_jittered,
        lat = ~Lat,
        label = ~paste0("Site : ", Site),
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "top",
          textsize = "13px",
          style = list(
            "font-weight" = "bold",
            "color" = "#333",
            "background" = "rgba(255,255,255,0.7)",
            "padding" = "2px 4px",
            "border-radius" = "4px"
          )
        )
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("#F8766D", "#00BFC4"),
        labels = c("2020", "2025"),
        title = "Année"
      )
  })
}

# === Lancement de l'application ===
shinyApp(ui, server)


install.packages("rsconnect")
rsconnect::setAccountInfo(name='valentineanstett',
                          token='0D98A5AD9DA255F05C2BD9191EE0A62C',
                          secret='FJWMdUM1jAebUmAWqbSgszUzEwqDXi8jtGT6ySzr')
rsconnect::deployApp("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/ecocomlag/62_Carto_shiny.R")
