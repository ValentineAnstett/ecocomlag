#Calcul NDWI sur les pixels qui toucent ou sont inclus dans les shapefils 
#Pas de parralélisation // Sans cluster 
#Shapefile cercles de relevés // Pas lagune entière 
#Extraction des pixels PUIS calcul NDWI // Pas dans la même boucle pour alleger le process



####Test sans parraleliser et extraction des pixels puis calcul du NDWI pas dans le même script----
##Extyraction des pixels ----

# ------------------------------
# Packages
# ------------------------------
library(sf)
library(terra)
library(dplyr)

# ------------------------------
# Répertoire de travail
# ------------------------------
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")

# ------------------------------
# Paramètres
# ------------------------------
img_dir    <- "sentinel2_downloads_V2_2020"
output_csv <- "pixels_extraits_2020.csv"

if (file.exists(output_csv)) file.remove(output_csv)

# ------------------------------
# Images disponibles
# ------------------------------
tiff_files <- list.files(img_dir, pattern = "_B0[38]\\.tif$", full.names = TRUE)
items_ids  <- unique(gsub("_(B0[38])\\.tif$", "", basename(tiff_files)))
total_items <- length(items_ids)
message(total_items, " images à traiter")

# ------------------------------
# Shapefile (en Lambert-93)
# ------------------------------
zones_v <- vect("cercles_20m.shp")

zone_info_df <- data.frame(
  ID_LAG = 1:length(zones_v),
  CODE_LAG = zones_v$COD_LAG
)

# ------------------------------
# Liste pour stocker les résultats
# ------------------------------
all_pixels <- list()
start_time <- Sys.time()

# ------------------------------
# Boucle
# ------------------------------
for (j in seq_along(items_ids)) {
  
  item_id <- items_ids[j]
  files <- file.path(img_dir, paste0(item_id, c("_B03.tif", "_B08.tif")))
  if (!all(file.exists(files))) next
  
  r <- rast(files)
  names(r) <- c("B03","B08")
  
  # Reprojection nécessaire car images en UTM et shapefile en Lambert-93
  zones_proj <- project(zones_v, crs(r))
  
  r <- crop(r, zones_proj)
  r <- mask(r, zones_proj) / 10000
  
  pixels <- extract(r, zones_proj, ID = TRUE)
  
  # ------------------------------
  # Conserver uniquement les pixels valides B03 et B08
  # ------------------------------
  pixels <- pixels[!is.na(pixels$B03) & !is.na(pixels$B08), ]
  if (nrow(pixels) == 0) next
  
  # ------------------------------
  # Mise en forme
  # ------------------------------
  pixels$date <- as.Date(substr(item_id, 12, 19), "%Y%m%d")
  pixels <- left_join(pixels, zone_info_df, by = c("ID" = "ID_LAG"))
  pixels <- pixels %>% select(CODE_LAG, date, B03, B08)
  
  all_pixels[[length(all_pixels)+1]] <- pixels
  
  # Nettoyage mémoire
  rm(r, pixels, zones_proj); gc()
  
  # ------------------------------
  # Message simplifié : images traitées + temps restant
  # ------------------------------
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  avg_time <- elapsed / j
  est_remaining <- round(avg_time * (total_items - j))
  
  message(j, "/", total_items, " images traitées | Estimation restant : ~", est_remaining, "s")
}

# ------------------------------
# Combiner et écrire CSV
# ------------------------------
final_df <- do.call(rbind, all_pixels)
write.csv(final_df, output_csv, row.names = FALSE)

message("Extraction terminée ! CSV généré : ", output_csv)

#Calcul NDWI ----- 
# ------------------------------
# Packages
# ------------------------------
library(dplyr)
library(tibble)

# ------------------------------
# Fichiers
# ------------------------------
input_csv  <- "pixels_extraits_2020.csv"
output_csv <- "ndwi_pixels_2020.csv"
cloud_max  <- 0.20


# ------------------------------
# Lire les pixels extraits
# ------------------------------
pixels <- read.csv(input_csv)

# ------------------------------
# Calcul NDWI
# ------------------------------
pixels$NDWI <- (pixels$B03 - pixels$B08) / (pixels$B03 + pixels$B08)

# ------------------------------
# Filtre cloud par zone
# ------------------------------
valid_ratio <- pixels %>%
  group_by(CODE_LAG, date) %>%
  summarise(
    ratio = mean(!is.na(NDWI)),
    .groups = "drop"
  )

keep_ids <- valid_ratio %>%
  filter(ratio >= (1 - cloud_max)) %>%
  select(CODE_LAG, date)

pixels <- pixels %>%
  inner_join(keep_ids, by = c("CODE_LAG", "date"))

# ------------------------------
# Conserver colonnes finales
# ------------------------------
pixels_final <- pixels %>% select(CODE_LAG, date, NDWI)

# ------------------------------
# Écriture CSV final
# ------------------------------
write.csv(pixels_final, output_csv, row.names = FALSE)
