# Packages ----
install.packages(c("rstac", "sf", "terra", "dplyr", "reticulate", "httr", "jsonlite"))

library(sf)
library(rstac)
library(terra)
library(dplyr)
library(httr)
library(reticulate)


####TELECHARGEMENT DES IMAGES ####

# 0️⃣ Configurer reticulate pour Python
# pip install planetary-computer
planetarycomputer <- import("planetary_computer")

# 1️⃣ Lire le shapefile et calculer la bbox globale

zones <- st_read("Shapefile_lagunes.shp") %>% st_transform(4326)
bbox <- st_bbox(zones)
bbox_vec <- c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)

# 2️⃣ Connexion STAC Planetary Computer

pc <- stac("https://planetarycomputer.microsoft.com/api/stac/v1")

# 3️⃣ Recherche des images Sentinel-2-L2A

search <- stac_search(
  pc,
  collections = "sentinel-2-l2a",
  bbox = bbox_vec,
  datetime = "2019-09-01/2020-08-31"
) |> get_request()

items <- items_fetch(search)

# 4️⃣ Créer dossier et définir bandes

dir.create("sentinel2_downloads", showWarnings = FALSE)
bands <- c("B02","B03","B04","B08")
options(timeout = 1800)

# 5️⃣ Fonction robuste pour tester un TIFF

check_tiff_ok <- function(path){
  tryCatch({
    r <- rast(path)
    terra::minmax(r)
    TRUE
  }, error = function(e){FALSE})
}

# 6️⃣ Fonction de téléchargement sécurisé

download_band <- function(item_id, band, url){
  destfile <- file.path("sentinel2_downloads", paste0(item_id, "_", band, ".tif"))
  if(file.exists(destfile)){
    if(check_tiff_ok(destfile)){
      message("✅ Fichier déjà présent et valide : ", destfile)
      return(TRUE)
    } else {
      message("⚠️ Fichier corrompu détecté, suppression : ", destfile)
      unlink(destfile)
    }
  }
  
  # Téléchargement avec tryCatch
  tryCatch({
    GET(url, write_disk(destfile, overwrite = TRUE), progress())
    if(check_tiff_ok(destfile)){
      message("✅ Téléchargé et valide : ", destfile)
      TRUE
    } else {
      message("❌ Téléchargement incomplet : ", destfile)
      unlink(destfile)
      FALSE
    }
  }, error = function(e){
    message("❌ Erreur téléchargement ", destfile, " : ", e$message)
    FALSE
  })
}


# 7️⃣ Télécharger toutes les bandes valides

for(item in items$features){
  for(b in bands){
    if(!is.null(item$assets[[b]])){
      url <- planetarycomputer$sign(item$assets[[b]]$href)
      download_band(item$id, b, url)
    } else {
      warning("La bande ", b, " n'existe pas pour l'item ", item$id)
    }
  }
}











####TRAITEMENT DES IMAGES####
# ------------------------------
# 8️⃣ Définir CRS UTM depuis une bande lisible
# ------------------------------
sample_file <- list.files("sentinel2_downloads", pattern="_B02.tif$", full.names=TRUE)[1]
B2_ref <- rast(sample_file)
zones_utm <- st_transform(zones, crs(B2_ref))
zones_v <- vect(zones_utm)

# ------------------------------
# 9️⃣ Traitement NDVI / NDWI
# ------------------------------
results <- list()
stats_table <- data.frame()

for(i in seq_len(nrow(zones_v))){
  
  zone <- zones_v[i]
  zone_name <- zones$Nom[i] %||% paste0("Zone_", i)
  
  for(item in items$features){
    
    date_img <- item$properties$`datetime`
    
    # chemins TIFF
    band_files <- file.path("sentinel2_downloads", paste0(item$id, "_", bands, ".tif"))
    names(band_files) <- bands
    
    if(any(!file.exists(band_files))) next
    if(any(!sapply(band_files, check_tiff_ok))) next
    
    # charger
    B2 <- rast(band_files["B02"])
    B3 <- rast(band_files["B03"])
    B4 <- rast(band_files["B04"])
    B8 <- rast(band_files["B08"])
    
    img <- c(B2,B3,B4,B8)
    
    if(!relate(ext(img), ext(zone), "T********")) next
    
    img_clip <- tryCatch(crop(img, zone), error=function(e) NULL)
    if(is.null(img_clip)) next
    
    img_mask <- tryCatch(mask(img_clip, zone), error=function(e) NULL)
    if(is.null(img_mask)) next
    
    ndvi <- (img_mask[[4]] - img_mask[[3]]) / (img_mask[[4]] + img_mask[[3]])
    ndwi <- (img_mask[[2]] - img_mask[[4]]) / (img_mask[[2]] + img_mask[[4]])
    
    cloud_ratio <- sum(is.na(values(ndvi))) / ncell(ndvi)
    if(cloud_ratio > 0.1) next
    
    key <- paste0("zone", i, "_", item$id)
    results[[key]] <- list(ndvi=ndvi, ndwi=ndwi)
    
    # ------------------------------
    # 🔟 Extraction statistiques
    # ------------------------------
    ndvi_vals <- values(ndvi)
    ndwi_vals <- values(ndwi)
    
    stats_table <- rbind(stats_table,
                         data.frame(
                           zone_id = i,
                           zone_name = zone_name,
                           date = as.Date(date_img),
                           ndvi_mean = mean(ndvi_vals, na.rm=TRUE),
                           ndvi_median = median(ndvi_vals, na.rm=TRUE),
                           ndvi_min = min(ndvi_vals, na.rm=TRUE),
                           ndvi_max = max(ndvi_vals, na.rm=TRUE),
                           ndwi_mean = mean(ndwi_vals, na.rm=TRUE),
                           ndwi_median = median(ndwi_vals, na.rm=TRUE),
                           ndwi_min = min(ndwi_vals, na.rm=TRUE),
                           ndwi_max = max(ndwi_vals, na.rm=TRUE),
                           cloud_ratio = cloud_ratio
                         )
    )
    
    message("📌 Stats ajoutées pour ", zone_name, " — ", date_img)
  }
}

write.csv(stats_table, "NDVI_NDWI_stats_by_zone.csv", row.names = FALSE)

