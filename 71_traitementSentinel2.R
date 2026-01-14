# ------------------------------
# Packages
# ------------------------------
library(sf)
library(terra)
library(dplyr)
library(tibble)
library(foreach)
library(doParallel)

# ------------------------------
# Répertoire de travail
# ------------------------------
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")

# ------------------------------
# Paramètres
# ------------------------------
img_dir <- "sentinel2_downloads"
cloud_max <- 0.1
WIW_NIR_THR   <- 0.1804
WIW_SWIR2_THR <- 0.1131

# ------------------------------
# Images disponibles
# ------------------------------
tiff_files <- list.files(img_dir, pattern = "_B0[348]|_B12\\.tif$", full.names = TRUE)
items_ids <- unique(gsub("_(B0[348]|B12)\\.tif$", "", basename(tiff_files)))

total_items <- length(items_ids)
message(total_items, " images à traiter")

# ------------------------------
# Fichier de suivi d'avancement
# ------------------------------
progress_file <- "progress_images.txt"
if (file.exists(progress_file)) file.remove(progress_file)

# ------------------------------
# Configuration du cluster parallèle
# ------------------------------
num_cores <- min(4, parallel::detectCores() - 1)
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# ------------------------------
# Calcul WIW et NDWI (boucle par image) ----
# ------------------------------
stats_list <- foreach(
  j = seq_along(items_ids),
  .packages = c("terra","dplyr","tibble","sf")
) %dopar% {
  
  # Lecture locale AU WORKER
  zones <- st_read("Shapefile_lagunes.shp", quiet = TRUE)
  zones_v <- vect(zones)
  
  item_id <- items_ids[j]
  
  files <- file.path(img_dir, paste0(item_id, c("_B03.tif","_B08.tif","_B12.tif")))
  if (!all(file.exists(files))) return(NULL)
  
  B03 <- rast(files[1]) / 10000
  B08 <- rast(files[2]) / 10000
  B12 <- rast(files[3]) / 10000
  
  B12_10m <- resample(B12, B08, method = "bilinear")
  
  ndwi <- (B03 - B08) / (B03 + B08)
  wiw  <- (B08 <= WIW_NIR_THR) & (B12_10m <= WIW_SWIR2_THR)
  
  ndwi_vals <- extract(ndwi, zones_v)
  wiw_vals  <- extract(as.numeric(wiw), zones_v)
  
  valid_ratio <- apply(!is.na(ndwi_vals[, -1, drop = FALSE]), 1, mean)
  keep <- valid_ratio >= (1 - cloud_max)
  if (!any(keep)) return(NULL)
  
  date_img <- as.Date(substr(item_id, 12, 19), "%Y%m%d")
  zone_idx <- ndwi_vals[keep, 1]
  
  res <- tibble(
    ID_LAG   = zones$id[zone_idx],
    Site = zones$Site[zone_idx],
    date      = date_img,
    ndwi_mean = apply(ndwi_vals[keep, -1, drop = FALSE], 1, mean, na.rm = TRUE),
    wiw_ratio = apply(wiw_vals[keep, -1, drop = FALSE], 1, mean, na.rm = TRUE)
  )
  
  # ---- Avancement (robuste en parallèle)
  cat(sprintf("Image %d / %d traitée : %s\n", j, total_items, item_id),
      file = progress_file, append = TRUE)
  
  res
}

# ------------------------------
# Nettoyage cluster
# ------------------------------
stopCluster(cl)

# ------------------------------
# Résultat final
# ------------------------------
stats_list <- stats_list[!sapply(stats_list, is.null)]

stats_table <- bind_rows(stats_list) %>%
  arrange(zone_id, date)

#stats_table <- stats_table %>%
  #rename(ID_LAG = zone_id)
#stats_table <- stats_table %>%
  #rename(Site = zone_name)

write.csv(stats_table, "stats_table_wiw_ndwi.csv", row.names = FALSE)
