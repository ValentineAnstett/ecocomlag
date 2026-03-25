
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")
# Packages ----
library(dplyr)
library(ggplot2)

# Paramètres ----
seuil_assec = 25    # % surface en eau ≤ 20% = assec
seuil_ndwi = 0.05   # seuil NDWI pour considérer un pixel en eau
min_consec = 3  #au moins x images conssecutives pour valider l'etat 

# Shapefile ----
#NDWI des pixels de chaque cercles au différentes dates
pixels = read.csv("ndwi_pixels_2020.csv")
pixels$date = as.Date(pixels$date)


#% surface en eau par date ----
calc_pct_eau = function(ndwi_values, seuil = seuil_ndwi) {
  sum(ndwi_values > seuil, na.rm = TRUE) / sum(!is.na(ndwi_values)) * 100
}

# Calcul % eau par zone et date ----
hydro = pixels %>%
  group_by(CODE_LAG, date) %>%
  summarise(pct_eau = calc_pct_eau(NDWI), .groups = "drop")

# Détection des cycles par zone ----
result_final <- data.frame()
zones <- unique(hydro$CODE_LAG)

for (z in zones) {
  
  df_zone <- hydro %>%
    filter(CODE_LAG == z) %>%
    arrange(date)
  
  # Étape 1 : état brut
  etat_brut <- ifelse(df_zone$pct_eau <= seuil_assec, 0, 1)
  
  # Étape 2 : rolling window pour valider l'état sur min_consec dates
  library(zoo)
  etat_valide <- zoo::rollapply(etat_brut, width = min_consec, FUN = function(x) {
    if(all(x == 0)) return(0)   # assec confirmé
    if(all(x == 1)) return(1)   # eau confirmée
    return(NA)                   # pas assez de confirmation
  }, fill = NA, align = "right")
  
  # Remplacer NA par l'état précédent connu pour ne pas casser les séquences
  etat_valide <- zoo::na.locf(etat_valide, na.rm = FALSE)
  
  # run-length encoding sur l'état validé
  r <- rle(etat_valide)
  lengths <- r$lengths
  values <- r$values
  ends <- cumsum(lengths)
  starts <- ends - lengths + 1
  
  # Détection transitions assec → eau
  if(length(values) > 1){
    for(i in 1:(length(values) - 1)){
      # 0 → 1 avec confirmation de l'état eau sur min_consec dates
      if(isTRUE(values[i] == 0 && values[i + 1] == 1 && lengths[i+1] >= min_consec)){
        
        debut_assec <- df_zone$date[starts[i]]
        mise_en_eau <- df_zone$date[starts[i + 1]]
        duree_assec <- as.numeric(mise_en_eau - debut_assec)
        
        result_final <- rbind(result_final, data.frame(
          CODE_LAG = z,
          debut_assec = debut_assec,
          mise_en_eau = mise_en_eau,
          duree_assec = duree_assec
        ))
      }
    }
  }
}

# Vérification
print(result_final)


write.csv(result_final, "hydroperiode_2020_Vfinale.csv", row.names = FALSE)

