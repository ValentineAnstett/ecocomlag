library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(tibble)

setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")

# ------------------------------
# Paramètres
# ------------------------------
img_dir <- "sentinel2_downloads"
bands <- c("B02","B03","B04","B08","B12")
cloud_max <- 0.1

# WIW thresholds – Sentinel-2 (MDPI 2019)
WIW_NIR_THR   <- 0.1804
WIW_SWIR2_THR <- 0.1131

# ------------------------------
# Shapefile
# ------------------------------
sites_exclus <- c("Canet", "Pissevache", "Hyères", "Villeroy")
zones <- st_read("Shapefile_lagunes.shp", quiet = TRUE)

name_col <- intersect(c("Site","Nom","site","name"), names(zones))[1]
if (is.na(name_col)) {
  zones$site_name <- paste0("Zone_", seq_len(nrow(zones)))
  name_col <- "site_name"
}

zones <- zones %>%
  filter(!.data[[name_col]] %in% sites_exclus)

id_col <- if ("id" %in% names(zones)) "id" else {
  zones$oid <- seq_len(nrow(zones))
  "oid"
}

# ------------------------------
# Projection vers CRS Sentinel-2
# ------------------------------
sample_file <- list.files(img_dir, pattern="_B02.tif$", full.names = TRUE)[1]
B2_ref <- rast(sample_file)
zones_v <- vect(st_transform(zones, crs(B2_ref)))

# ------------------------------
# Liste des images (items)
# ------------------------------
tiff_files <- list.files(img_dir, pattern="_B0[2348]|_B12\\.tif$", full.names = TRUE)
items_ids <- unique(gsub("_(B0[2348]|B12)\\.tif$", "", basename(tiff_files)))

# ------------------------------
# Boucle principale
# ------------------------------
stats_list <- list()  # liste vide pour stocker les résultats
k <- 1

total_zones <- nrow(zones_v)
total_items <- length(items_ids)
total_calcs <- total_zones * total_items  # total des calculs possibles

calc_counter <- 0  # compteur global

for (i in seq_len(total_zones)) {
  
  zone <- zones_v[i]
  zone_name_val <- as.character(zones[[name_col]][i])
  zone_id_val   <- as.character(zones[[id_col]][i])
  
  message("🔹 Zone ", i, "/", total_zones, " : ", zone_name_val)
  
  for (j in seq_along(items_ids)) {
    
    calc_counter <- calc_counter + 1  # on avance dans le total
    item_id <- items_ids[j]
    
    # Vérifier que tous les fichiers existent
    files_exist <- all(file.exists(file.path(img_dir, paste0(item_id, "_", bands, ".tif"))))
    if (!files_exist) next
    
    # Charger les rasters individuellement
    B02 <- rast(file.path(img_dir, paste0(item_id, "_B02.tif")))
    B03 <- rast(file.path(img_dir, paste0(item_id, "_B03.tif")))
    B04 <- rast(file.path(img_dir, paste0(item_id, "_B04.tif")))
    B08 <- rast(file.path(img_dir, paste0(item_id, "_B08.tif")))
    B12 <- rast(file.path(img_dir, paste0(item_id, "_B12.tif")))
    
    # Normalisation
    B02 <- B02 / 10000
    B03 <- B03 / 10000
    B04 <- B04 / 10000
    B08 <- B08 / 10000
    B12 <- B12 / 10000
    
    # NDWI
    ndwi <- (B03 - B08) / (B03 + B08)
    vals_ndwi <- terra::extract(ndwi, zone)[,2]
    if (length(vals_ndwi) == 0 || all(is.na(vals_ndwi))) next
    
    cloud_ratio <- mean(is.na(vals_ndwi))
    if (cloud_ratio > cloud_max) next
    
    # WIW
    B12_10m <- resample(B12, B08, method = "bilinear")
    WIW <- (B08 <= WIW_NIR_THR) & (B12_10m <= WIW_SWIR2_THR)
    WIW_num <- as.numeric(WIW)
    vals_wiw <- terra::extract(WIW_num, zone)[,2]
    if (length(vals_wiw) == 0 || all(is.na(vals_wiw))) next
    wiw_ratio <- mean(vals_wiw, na.rm = TRUE)
    
    # Date
    date_str <- substr(item_id, 12, 19)
    date_img <- as.Date(date_str, "%Y%m%d")
    if (is.na(date_img)) next
    
    # Stockage
    stats_list[[length(stats_list)+1]] <- tibble(
      zone_id   = zone_id_val,
      zone_name = zone_name_val,
      date      = date_img,
      ndwi_mean = mean(vals_ndwi, na.rm = TRUE),
      wiw_ratio = wiw_ratio
    )
    
    # ------------------------------
    # Message d'avancement global
    # ------------------------------
    if (calc_counter %% 10 == 0 || calc_counter == total_calcs) {
      message("⏱️ Progression globale : ", round(100*calc_counter/total_calcs,1), "% (", 
              calc_counter, "/", total_calcs, " calculs)")
    }
    
  }
} 

# ------------------------------
# Table finale
# ------------------------------
if (length(stats_list) == 0) stop("Aucune donnée valide produite, vérifier les bandes et les seuils WIW.")
stats_table <- bind_rows(stats_list) %>%
  arrange(zone_id, date)

# Export CSV
write.csv(stats_table, "stats_table.csv", row.names = FALSE)


