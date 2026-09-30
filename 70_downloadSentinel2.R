# Packages ----

library(sf)
library(rstac)
library(terra)
library(dplyr)
library(reticulate)
library(httr)
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")

####TELECHARGEMENT DES IMAGES ####

#0. Configurer reticulate pour Python
py_install("planetary-computer", envpath = "/home/anstett/.cache/R/reticulate/uv/cache/archive-v0/s4dXHS2TJT_l1_C8b9bGO", pip = TRUE)
# pip install planetary-computer
planetarycomputer = import("planetary_computer")

#1. Lire le shapefile et calculer la bbox globale

zones = st_read("Shapefile_lagunes.shp") %>% st_transform(4326)
bbox = st_bbox(zones)
bbox_vec = c(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax)

#2. Connexion STAC Planetary Computer

pc = stac("https://planetarycomputer.microsoft.com/api/stac/v1")

#3. Recherche des images Sentinel-2-L2A

search = stac_search(
  pc,
  collections = "sentinel-2-l2a",
  bbox = bbox_vec,
  datetime = "2019-05-01/2020-04-30"
) |> get_request()

items <- items_fetch(search)

#4. Créer dossier et définir bandes

dir.create("sentinel2_downloads_V2_2020", showWarnings = FALSE)
#bands = c("B02","B03","B04","B08", "B12")
bands = c("B03", "B08")
options(timeout = 1800)

#5. Fonction robuste pour tester un TIFF

check_tiff_ok = function(path){
  tryCatch({
    r = rast(path)
    terra::minmax(r)
    TRUE
  }, error = function(e){FALSE})
}

#6. Fonction de téléchargement sécurisé

download_band = function(item_id, band, url){
  destfile = file.path("sentinel2_downloads_V2_2025", paste0(item_id, "_", band, ".tif"))
  if(file.exists(destfile)){
    if(check_tiff_ok(destfile)){
      message("Fichier déjà présent et valide : ", destfile)
      return(TRUE)
    } else {
      message("Fichier corrompu détecté, suppression : ", destfile)
      unlink(destfile)
    }
  }
  
  # Téléchargement avec tryCatch
  tryCatch({
    GET(url, write_disk(destfile, overwrite = TRUE), progress())
    if(check_tiff_ok(destfile)){
      message("Téléchargé et valide : ", destfile)
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


#7. Télécharger toutes les bandes valides

for(item in items$features){
    if(!is.null(item$assets[[b]])){
      url = planetarycomputer$sign(item$assets[[b]]$href)
      download_band(item$id, b, url)
    } else {
      warning("La bande ", b, " n'existe pas pour l'item ", item$id)
    }
  }
}






####Si besoin 
#Relancer pour une seule bande (ici B12) ####
download_band = function(item_id, band, url){
  destfile = file.path("sentinel2_downloads_2025", paste0(item_id, "_", band, ".tif"))
  if(file.exists(destfile)){
    message("Fichier déjà présent : ", destfile)
    return(TRUE)
  }
  
  # Téléchargement avec tryCatch
  tryCatch({
    httr::GET(url, httr::write_disk(destfile, overwrite = TRUE), httr::progress())
    message("Téléchargé : ", destfile)
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



#Retelecharger les fichiers corrompus 
# Vérifie tous les fichiers existants et retélécharge si corrompus
for(item in features_list){
  if(!is.list(item) || is.null(item$assets)) next  # ignore si pas valide
  
  for(b in bands){
    destfile <- file.path("sentinel2_downloads_2025", paste0(item$id, "_", b, ".tif"))
    
    # Si le fichier existe mais est corrompu, retélécharger
    if(file.exists(destfile)){
      if(!check_tiff_ok(destfile)){
        message("Fichier corrompu détecté, retéléchargement : ", destfile)
        url <- planetarycomputer$sign(item$assets[[b]]$href)
        download_band(item$id, b, url)
      } else {
        message("Fichier déjà valide : ", destfile)
      }
    } else {
      # Fichier manquant : on peut ignorer ou télécharger selon ton choix
      message("Fichier manquant : ", destfile, "- à télécharger si nécessaire")
    }
  }
}
