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
# créer zone shapefile
# ------------------------------
points = st_read("points_lagunes.shp")
st_crs(points) # Vérifier le système de coordonnées
#Création du cercle
#Déjà fait = plus à refaire
#circles = st_buffer(points, dist = 10) #dist = radius du cercle
#st_write(circles, "cercles_20m.shp")



# ------------------------------
# Paramètres
# ------------------------------
img_dir   = "sentinel2_downloads"
cloud_max = 0.1

# ------------------------------
# Images disponibles
# ------------------------------
tiff_files = list.files(img_dir, pattern = "_B0[38]\\.tif$", full.names = TRUE)
items_ids = unique(gsub("_(B0[38])\\.tif$", "", basename(tiff_files)))

total_items <- length(items_ids)
message(total_items, " images à traiter")

# ------------------------------
# Fichier de suivi
# ------------------------------
progress_file <- "progress_images.txt"
if (file.exists(progress_file)) file.remove(progress_file)

# ------------------------------
# Cluster parallèle
# ------------------------------
num_cores <- min(4, parallel::detectCores() - 1)
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# ------------------------------
# Boucle NDWI uniquement
# ------------------------------
stats_list <- foreach(
  j = seq_along(items_ids),
  .packages = c("terra","dplyr","tibble","sf")
) %dopar% {
  
  # Lecture shapefile dans le worker
  zones <- st_read("cercles_20m.shp", quiet = TRUE)
  zones_v <- vect(zones)
  
  item_id <- items_ids[j]
  
  files <- file.path(img_dir, paste0(item_id, c("_B03.tif","_B08.tif")))
  if (!all(file.exists(files))) return(NULL)
  
  # Lecture bandes (réflectance)
  B03 <- rast(files[1]) / 10000
  B08 <- rast(files[2]) / 10000
  
  # NDWI (McFeeters)
  ndwi <- (B03 - B08) / (B03 + B08)
  
  # Extraction NDWI
  ndwi_vals <- extract(ndwi, zones_v)
  
  # Filtrage nuages / pixels valides
  valid_ratio <- apply(!is.na(ndwi_vals[, -1, drop = FALSE]), 1, mean)
  keep <- valid_ratio >= (1 - cloud_max)
  if (!any(keep)) return(NULL)
  
  # Date image
  date_img <- as.Date(substr(item_id, 12, 19), "%Y%m%d")
  zone_idx <- ndwi_vals[keep, 1]
  
  # NDWI moyen par zone
  ndwi_mean <- apply(
    ndwi_vals[keep, -1, drop = FALSE],
    1,
    mean,
    na.rm = TRUE
  )
  
  # Classification NDWI
  ndwi_class <- case_when(
    ndwi_mean < -0.2            ~ "vegetation",
    ndwi_mean >= -0.2 & ndwi_mean < 0 ~ "assec_sel",
    ndwi_mean >= 0              ~ "eau"
  )
  
  res <- tibble(
    ID_LAG    = zones$COD_LAG[zone_idx],
    date       = date_img,
    ndwi_mean  = ndwi_mean,
    ndwi_class = ndwi_class
  )
  
  # Avancement
  cat(sprintf("Image %d / %d traitée : %s\n", j, total_items, item_id),
      file = progress_file, append = TRUE)
  
  res
}

stopCluster(cl)

# ------------------------------
# Table finale
# ------------------------------
stats_list <- stats_list[!sapply(stats_list, is.null)]

stats_table <- bind_rows(stats_list) %>%
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
write.csv(stats_table, "ndwi_2020.csv", row.names = FALSE)
