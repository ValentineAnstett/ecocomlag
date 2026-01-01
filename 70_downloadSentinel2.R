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
py_install("planetary-computer", envpath = "/home/anstett/.cache/R/reticulate/uv/cache/archive-v0/s4dXHS2TJT_l1_C8b9bGO", pip = TRUE)
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

#Relancer pour B12 ####
download_band <- function(item_id, band, url){
  destfile <- file.path("sentinel2_downloads", paste0(item_id, "_", band, ".tif"))
  if(file.exists(destfile)){
    message("✅ Fichier déjà présent : ", destfile)
    return(TRUE)
  }
  
  # Téléchargement avec tryCatch
  tryCatch({
    httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::progress())
    message("✅ Téléchargé : ", destfile)
    TRUE
  }, error = function(e){
    message("❌ Erreur téléchargement ", destfile, " : ", e$message)
    FALSE
  })
}

index = 0;
total_items <- length(items$features)
for(item in items$features){
  if(!is.null(item$assets[["B12"]])){
    url <- planetarycomputer$sign(item$assets[["B12"]]$href)
    index = index + 1
    message("Downloading item ", index, "/", total_items)
    download_band(item$id, "B12", url)
  } else {
    warning("La bande B12 n'existe pas pour l'item ", item$id)
  }
}
