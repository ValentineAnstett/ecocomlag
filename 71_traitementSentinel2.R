library(sf)
library(terra)
library(dplyr)
library(lubridate)
library(tibble)

setwd("/home/anstett/Documents/LTM-Flora/Hydroperiode_sat")

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
# Liste des images
# ------------------------------
tiff_files <- list.files(img_dir, pattern="_B0[2348]|_B12\\.tif$", full.names = TRUE)
items_ids <- unique(gsub("_(B0[2348]|B12)\\.tif$", "", basename(tiff_files)))

# ------------------------------
# Boucle principale
# ------------------------------
stats_list <- vector("list", length(items_ids) * nrow(zones_v))
k <- 1

for (i in seq_len(nrow(zones_v))) {
  
  zone <- zones_v[i]
  zone_name_val <- as.character(zones[[name_col]][i])
  zone_id_val   <- as.character(zones[[id_col]][i])
  
  message("🔹 Zone ", i, "/", nrow(zones_v), " : ", zone_name_val)
  
  for (j in seq_along(items_ids)) {
    
    item_id <- items_ids[j]
    files <- file.path(img_dir, paste0(item_id, "_", bands, ".tif"))
    if (any(!file.exists(files))) next
    
    # Charger rasters
    img <- rast(files)
    
    # Vérifier chevauchement rapide
    if (!relate(ext(img), ext(zone), "T********")) next
    
    # ------------------------------
    # NDWI
    # ------------------------------
    ndwi <- (img[["B03"]] - img[["B08"]]) /
      (img[["B03"]] + img[["B08"]])
    
    vals_ndwi <- terra::extract(ndwi, zone)[,2]
    if (length(vals_ndwi) == 0 || all(is.na(vals_ndwi))) next
    
    cloud_ratio <- mean(is.na(vals_ndwi))
    if (cloud_ratio > cloud_max) next
    
    # ------------------------------
    # WIW – MDPI 2019
    # ------------------------------
    B08 <- img[["B08"]]  # NIR (10 m)
    B12 <- img[["B12"]]  # SWIR2 (20 m)
    
    # Rééchantillonnage SWIR2 → 10 m
    B12_10m <- resample(B12, B08, method = "bilinear")
    
    # WIW binaire
    WIW <- (B08 <= WIW_NIR_THR) & (B12_10m <= WIW_SWIR2_THR)
    WIW_num <- as.numeric(WIW)
    
    vals_wiw <- terra::extract(WIW_num, zone)[,2]
    if (length(vals_wiw) == 0 || all(is.na(vals_wiw))) next
    
    wiw_ratio <- mean(vals_wiw, na.rm = TRUE)
    
    # ------------------------------
    # Date Sentinel-2
    # ------------------------------
    date_str <- substr(item_id, 12, 19)
    date_img <- as.Date(date_str, "%Y%m%d")
    if (is.na(date_img)) next
    
    # ------------------------------
    # Stockage
    # ------------------------------
    stats_list[[k]] <- tibble(
      zone_id   = zone_id_val,
      zone_name = zone_name_val,
      date      = date_img,
      ndwi_mean = mean(vals_ndwi, na.rm = TRUE),
      wiw_ratio = wiw_ratio
    )
    k <- k + 1
    
    if (j %% 50 == 0) {
      message("   • ", j, "/", length(items_ids), " images traitées")
    }
  }
}

# ------------------------------
# Table finale
# ------------------------------
stats_table <- bind_rows(stats_list) %>%
  arrange(zone_id, date)

write.csv(stats_table, "stats_table.csv", row.names = FALSE)

# ------------------------------
# Détection mise en eau / assec (WIW)
# ------------------------------
stats_table <- read.csv("stats_table.csv") %>%
  mutate(
    date = as.Date(date),
    ndwi_mean = as.numeric(ndwi_mean),
    wiw_ratio = as.numeric(wiw_ratio)
  )

detect_water_transitions_wiw <- function(df_zone,
                                         wiw_on = 0.3,
                                         wiw_off = 0.05,
                                         min_gap_days = 30){
  
  df_zone <- df_zone %>% arrange(date)
  
  water_dates <- df_zone$date[df_zone$wiw_ratio >= wiw_on]
  dry_dates   <- df_zone$date[df_zone$wiw_ratio <= wiw_off]
  
  date_mise_en_eau <- if (length(water_dates) > 0) min(water_dates) else NA
  date_assec <- NA
  
  if (!is.na(date_mise_en_eau)) {
    candidates <- dry_dates[dry_dates > date_mise_en_eau + min_gap_days]
    date_assec <- if (length(candidates) > 0) min(candidates) else NA
  }
  
  tibble(
    zone_id = df_zone$zone_id[1],
    zone_name = df_zone$zone_name[1],
    date_mise_en_eau = date_mise_en_eau,
    date_assec = date_assec
  )
}

final_transitions <- stats_table %>%
  group_by(zone_id, zone_name) %>%
  group_modify(~ detect_water_transitions_wiw(.x)) %>%
  ungroup()

write.csv(final_transitions, "hydroperiods_WIW.csv", row.names = FALSE)