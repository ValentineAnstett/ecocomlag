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
img_dir   = "sentinel2_downloads_V2_2025"
cloud_max = 0.20

# ------------------------------
# Images disponibles
# ------------------------------
tiff_files = list.files(img_dir, pattern = "_B0[38]\\.tif$", full.names = TRUE)
items_ids = unique(gsub("_(B0[38])\\.tif$", "", basename(tiff_files)))

total_items = length(items_ids)
message(total_items, " images à traiter")

# ------------------------------
# Fichier de suivi
# ------------------------------
progress_file = "progress_images.txt"
if (file.exists(progress_file)) file.remove(progress_file)

# ------------------------------
# Charger le shapefile UNE FOIS
# ------------------------------
zones   = st_read("cercles_20m.shp", quiet = TRUE)
zones_v = vect("cercles_20m.shp")
zones_id = zones_v$COD_LAG
rm(zones)
gc()
# ------------------------------
# Cluster parallèle
# ------------------------------
num_cores = 2  # Limite volontaire pour la RAM : Les coeurs utilisent la même RAM en simultané
cl <- makeCluster(num_cores)
registerDoParallel(cl)
# ------------------------------
# Boucle NDWI (valeurs brutes)
# ------------------------------
stats_list <- foreach(
  j = seq_along(items_ids),
  .packages = c("terra", "dplyr", "tibble")
) %dopar% {
  
  # ID de l'image
  item_id <- items_ids[j]
  
  # Fichiers B03 et B08
  files <- file.path(img_dir, paste0(item_id, c("_B03.tif", "_B08.tif")))
  if (!all(file.exists(files))) return(NULL)
  
  # Lecture raster et mise à l’échelle
  B03 <- rast(files[1]) / 10000
  B08 <- rast(files[2]) / 10000
  
  # Calcul NDWI
  ndwi <- (B03 - B08) / (B03 + B08)
  
  # ------------------------------
  # Extraction PIXEL PAR PIXEL
  # ------------------------------
  ndwi_vals <- extract(
    ndwi,
    zones_v,
    touches = TRUE,   # tous les pixels qui touchent
    weights = TRUE,   # proportion de recouvrement
    ID = TRUE         # format long (1 pixel = 1 ligne)
  )
  
  if (is.null(ndwi_vals) || nrow(ndwi_vals) == 0) return(NULL)
  
  # Renommer colonnes proprement
  colnames(ndwi_vals) <- c("ID", "NDWI", "weight")
  
  # Supprimer les NA
  ndwi_vals <- ndwi_vals[!is.na(ndwi_vals$NDWI), ]
  if (nrow(ndwi_vals) == 0) return(NULL)
  
  # ------------------------------
  # Filtre cloud AU NIVEAU ZONE
  # ------------------------------
  valid_ratio <- ndwi_vals %>%
    group_by(ID) %>%
    summarise(ratio = n() / sum(weight > 0), .groups = "drop")
  
  keep_ids <- valid_ratio$ID[valid_ratio$ratio >= (1 - cloud_max)]
  ndwi_vals <- ndwi_vals[ndwi_vals$ID %in% keep_ids, ]
  
  if (nrow(ndwi_vals) == 0) return(NULL)
  
  # ------------------------------
  # Ajouter infos
  # ------------------------------
  date_img <- as.Date(substr(item_id, 12, 19), "%Y%m%d")
  
  res <- tibble(
    ID_LAG = zones_id[ndwi_vals$ID],
    date   = date_img,
    ndwi   = ndwi_vals$NDWI,
    weight = ndwi_vals$weight
  )
  
  # Nettoyage mémoire
  rm(B03, B08, ndwi, ndwi_vals)
  gc()
  
  res
}

# ------------------------------
# Table finale
# ------------------------------
stats_list = stats_list[!sapply(stats_list, is.null)]

stats_table = bind_rows(stats_list) %>%
  arrange(ID_LAG, date)


# ------------------------------
#Arranger comme les autres datas 
# ------------------------------

stats_table$ID_LAG = sub("_(\\d)$", "_0\\1", stats_table$ID_LAG)

#Renommer PCA_CHA et FOS_CAB 
stats_table = stats_table %>%
  mutate(ID_LAG = dplyr::recode(ID_LAG,
                                "PCA_CHA_01" = "PCA_CAP_06",
                                "PCA_CHA_03" = "PCA_CAP_08",
                                "PCA_CHA_05" = "PCA_CAP_10"))

stats_table$ID_LAG = gsub("FOS_CAB", "FOS_REL", stats_table$ID_LAG)

#Créer colonne Site  
stats_table = stats_table %>%
  mutate(Site = sub("_[^_]+$", "", ID_LAG))

#Supprimer les sites et les lagunes non intégrés 
sites_a_exclure = c("BPA_PIS", "BAG_GRA", "CAN_NAZ", "HYE_VIE", "THA_SET", "SAL_DEV")
stats_table = stats_table %>%
  filter(!Site %in% sites_a_exclure)

lag_a_exclure =  c("PCA_CHA_02", "PCA_CHA_04", "ORB_ORP_11")
stats_table = stats_table %>%
  filter(!ID_LAG %in% lag_a_exclure)

#Mettre des lettres pour les sites 
stats_table = stats_table %>%
  mutate(Site = dplyr::recode(Site,
                              "LAP_SAL" = "A",
                              "SIG_GSA" = "B",
                              "ORB_ORP" = "C",
                              "ORB_MAI" = "D",
                              "BAG_PET" = "E",
                              "PAL_FRO" = "F",
                              "PAL_VIL" = "G",
                              "EOR_MOT" = "H",
                              "PCA_CAP" = "I",
                              "GCA_RNC" = "J",
                              "FOS_REL" = "K"))


#Adapter dans la colonne ID_LAG les nouveaux codes sites 
stats_table = stats_table %>%
  mutate(ID_LAG = paste0(Site, "_", sub(".*_", "", ID_LAG)))


#Télécharger csv
write.csv(stats_table, "ndwi_2025.csv", row.names = FALSE)




#test sequenciel 
# ------------------------------
# Packages
# ------------------------------
library(terra)
library(dplyr)
library(tibble)

# ------------------------------
# Répertoire de travail
# ------------------------------
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")

# ------------------------------
# Paramètres
# ------------------------------
img_dir   <- "sentinel2_downloads_2025"
cloud_max <- 0.1

# ------------------------------
# Configuration terra
# ------------------------------
terraOptions(
  threads = 1,      # sécurité maximale
  progress = 0
)

# ------------------------------
# Lister les images Sentinel-2
# ------------------------------
tiff_files <- list.files(
  img_dir,
  pattern = "_B0[38]\\.tif$",
  full.names = TRUE
)

items_ids <- unique(
  gsub("_(B0[38])\\.tif$", "", basename(tiff_files))
)

message(length(items_ids), " images à traiter")

# ------------------------------
# Charger les zones UNE FOIS
# ------------------------------
zones_v <- vect("cercles_20m.shp")

# ID interne stable pour zonal()
zones_v$zone_id <- seq_len(nrow(zones_v))

# ------------------------------
# Liste de sortie
# ------------------------------
res_all <- vector("list", length(items_ids))

# ------------------------------
# Boucle principale sur les images
# ------------------------------
for (j in seq_along(items_ids)) {
  
  item_id <- items_ids[j]
  message("Traitement : ", item_id)
  
  # Fichiers B03 et B08
  files <- file.path(
    img_dir,
    paste0(item_id, c("_B03.tif", "_B08.tif"))
  )
  
  if (!all(file.exists(files))) {
    message("  -> bandes manquantes, ignorée")
    next
  }
  
  # ------------------------------
  # Lecture des rasters
  # ------------------------------
  B03 <- rast(files[1]) / 10000
  B08 <- rast(files[2]) / 10000
  
  # Harmoniser projection si besoin
  if (!compareGeom(B03, zones_v, stopOnError = FALSE)) {
    zones_v <- project(zones_v, B03)
  }
  
  # ------------------------------
  # Crop aux zones (GROS gain perf)
  # ------------------------------
  B03c <- crop(B03, zones_v)
  B08c <- crop(B08, zones_v)
  
  rm(B03, B08)
  gc()
  
  # ------------------------------
  # NDWI
  # ------------------------------
  ndwi <- (B03c - B08c) / (B03c + B08c)
  
  rm(B03c, B08c)
  gc()
  
  # ------------------------------
  # Mask avec les polygones
  # ------------------------------
  ndwi_m <- mask(ndwi, zones_v)
  
  rm(ndwi)
  gc()
  
  # ------------------------------
  # Proportion de pixels valides
  # ------------------------------
  valid_ratio <- zonal(
    !is.na(ndwi_m),
    zones_v,
    fun = "mean",
    na.rm = TRUE
  )
  
  keep <- valid_ratio$mean >= (1 - cloud_max)
  if (!any(keep)) {
    message("  -> trop de nuages")
    rm(ndwi_m)
    gc()
    next
  }
  
  # ------------------------------
  # NDWI moyen par zone
  # ------------------------------
  ndwi_mean <- zonal(
    ndwi_m,
    zones_v,
    fun = "mean",
    na.rm = TRUE
  )
  
  rm(ndwi_m)
  gc()
  
  # ------------------------------
  # Date image
  # ------------------------------
  date_img <- as.Date(substr(item_id, 12, 19), "%Y%m%d")
  
  # ------------------------------
  # Résultat
  # ------------------------------
  res <- tibble(
    ID_LAG    = zones_v$COD_LAG[ndwi_mean$zone_id],
    date      = date_img,
    ndwi_mean = ndwi_mean$mean
  ) %>%
    slice(keep) %>%
    mutate(
      ndwi_class = case_when(
        ndwi_mean < -0.2 ~ "vegetation",
        ndwi_mean < 0    ~ "assec_sel",
        TRUE             ~ "eau"
      )
    )
  
  res_all[[j]] <- res
}


#Test paralleliser 
library(future)
library(future.apply)

plan(multisession, workers = 2)  # ajuste selon ta RAM

# ------------------------------
# Lister les images Sentinel-2
# ------------------------------
img_dir <- "sentinel2_downloads_2025"
cloud_max <- 0.1

tiff_files <- list.files(
  img_dir,
  pattern = "_B0[38]\\.tif$",
  full.names = TRUE
)

items_ids <- unique(
  gsub("_(B0[38])\\.tif$", "", basename(tiff_files))
)

length(items_ids)



process_one_image <- function(item_id, cloud_max) {
  
  library(terra)
  library(dplyr)
  library(tibble)
  
  terraOptions(threads = 1, progress = 0)
  
  zones_v <- vect("cercles_20m.shp")
  zones_v$zone_id <- seq_len(nrow(zones_v))
  
  files <- file.path(
    "sentinel2_downloads_2025",
    paste0(item_id, c("_B03.tif", "_B08.tif"))
  )
  if (!all(file.exists(files))) return(NULL)
  
  B03 <- rast(files[1]) / 10000
  B08 <- rast(files[2]) / 10000
  
  if (!same.crs(B03, zones_v)) {
    zones_v <- project(zones_v, crs(B03))
  }
  
  ndwi <- (crop(B03, zones_v) - crop(B08, zones_v)) /
    (crop(B03, zones_v) + crop(B08, zones_v))
  
  ndwi_m <- mask(ndwi, zones_v)
  
  valid_ratio <- zonal(!is.na(ndwi_m), zones_v, "mean")$mean
  keep <- valid_ratio >= (1 - cloud_max)
  if (!any(keep)) return(NULL)
  
  ndwi_mean <- zonal(ndwi_m, zones_v, "mean", na.rm = TRUE)
  
  date_img <- as.Date(substr(item_id, 12, 19), "%Y%m%d")
  
  tibble(
    ID_LAG    = zones_v$COD_LAG[ndwi_mean$zone_id],
    date      = date_img,
    ndwi_mean = ndwi_mean$mean
  ) %>%
    slice(keep) %>%
    mutate(
      ndwi_class = case_when(
        ndwi_mean < -0.2 ~ "vegetation",
        ndwi_mean < 0    ~ "assec_sel",
        TRUE             ~ "eau"
      )
    )
}

res_all = future_lapply(
  items_ids,
  process_one_image,
  cloud_max = cloud_max, 
  future.seed = TRUE
)

stats_ndwi <- bind_rows(res_all)



####Sans Parralelisation -----
library(sf)
library(terra)
library(dplyr)
library(tibble)

setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")

img_dir   = "sentinel2_downloads_V2_2025"
cloud_max = 0.20

tiff_files = list.files(img_dir, pattern = "_B0[38]\\.tif$", full.names = TRUE)
items_ids = unique(gsub("_(B0[38])\\.tif$", "", basename(tiff_files)))

total_items = length(items_ids)
message(total_items, " images à traiter")

zones   = st_read("cercles_20m.shp", quiet = TRUE)
zones_v = vect("cercles_20m.shp")
zones_id = zones_v$COD_LAG
rm(zones)
gc()

start_time <- Sys.time()
pb <- txtProgressBar(min = 0, max = total_items, style = 3)

# ------------------------------
# Boucle NDWI (séquentielle)
# ------------------------------
stats_list <- vector("list", length(items_ids))

for (j in seq_along(items_ids)) {
  
  # ID de l'image
  item_id <- items_ids[j]
  
  # Progress bar
  setTxtProgressBar(pb, j)
  
  # Calcul ETA toutes les 5 images (évite spam console)
  if (j %% 5 == 0) {
    
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    avg_time <- elapsed / j
    remaining <- avg_time * (total_items - j)
    
    eta_time <- Sys.time() + remaining
    
    message(
      " | Image ", j, "/", total_items,
      " | Temps écoulé: ", round(elapsed/60, 1), " min",
      " | ETA: ", format(eta_time, "%H:%M:%S")
    )
  }
  
  # Fichiers B03 et B08
  files <- file.path(img_dir, paste0(item_id, c("_B03.tif", "_B08.tif")))
  if (!all(file.exists(files))) next
  
  # Lecture raster
  B03 <- rast(files[1]) / 10000
  B08 <- rast(files[2]) / 10000
  
  # NDWI
  ndwi <- (B03 - B08) / (B03 + B08)
  
  # ------------------------------
  # Extraction pixel
  # ------------------------------
  ndwi_vals <- extract(
    ndwi,
    zones_v,
    touches = TRUE,
    weights = TRUE,
    ID = TRUE
  )
  
  if (is.null(ndwi_vals) || nrow(ndwi_vals) == 0) next
  
  colnames(ndwi_vals) <- c("ID", "NDWI", "weight")
  
  # Supprimer NA
  ndwi_vals <- ndwi_vals[!is.na(ndwi_vals$NDWI), ]
  if (nrow(ndwi_vals) == 0) next
  
  # ------------------------------
  # Filtre cloud
  # ------------------------------
  valid_ratio <- ndwi_vals %>%
    group_by(ID) %>%
    summarise(ratio = n() / sum(weight > 0), .groups = "drop")
  
  keep_ids <- valid_ratio$ID[valid_ratio$ratio >= (1 - cloud_max)]
  ndwi_vals <- ndwi_vals[ndwi_vals$ID %in% keep_ids, ]
  
  if (nrow(ndwi_vals) == 0) next
  
  # ------------------------------
  # Ajouter infos
  # ------------------------------
  date_img <- as.Date(substr(item_id, 12, 19), "%Y%m%d")
  
  res <- tibble(
    ID_LAG = zones_id[ndwi_vals$ID],
    date   = date_img,
    ndwi   = ndwi_vals$NDWI,
    weight = ndwi_vals$weight
  )
  
  write.table(res,
              file = "ndwi_pixels.csv",
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = !file.exists("ndwi_pixels.csv"))
  
  # Nettoyage mémoire
  rm(B03, B08, ndwi, ndwi_vals)
  gc()
}
close(pb)
# ------------------------------
# Table finale
# ------------------------------
stats_list <- stats_list[!sapply(stats_list, is.null)]

stats_table <- bind_rows(stats_list) %>%
  arrange(ID_LAG, date)


