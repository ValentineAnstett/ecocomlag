setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale")
stats_table = read.csv("stats_table_wiw_ndwi.csv", header = TRUE, sep = ",", dec=".")

###############################################
#### Paramètres (a faire varier si besoin) ####
###############################################

# WIW
SEUIL_EAU_WIW  = 0.15
SEUIL_SEC_WIW  = 0.02

# NDWI
SEUIL_EAU_NDWI = 0.00
SEUIL_SEC_NDW=0.10

# Nombre d'images consécutives requises
N_CONFIRM = 15
#Validation assec 
MIN_DRY_DAYS = 15 

#Préparation des données / Organisation 
stats_table = stats_table %>%
  mutate(date = as.Date(date)) %>%
  arrange(ID_LAG, date)
#Definition de l'état hydro 
stats_table = stats_table %>%
  mutate(
    is_water = (wiw_ratio >= 0.15 | ndwi_mean >= 0.00),
    is_dry   = (wiw_ratio <= 0.02 & ndwi_mean <= -0.10)
  )
###############################################
#### Detection des transitions (1 cycle ?) ####
###############################################

#Boucle de détéction des transitions 
detect_hydro_dates <- function(df) {
  
  df <- df %>% arrange(date)
  
  # --- Mise en eau ---
  if (!any(df$is_water)) {
    return(tibble(
      date_mise_en_eau = as.Date(NA),
      date_assec       = as.Date(NA)
    ))
  }
  
  date_mise_en_eau <- min(df$date[df$is_water])
  
  # --- Recherche de l’assec APRÈS la mise en eau ---
  df_after <- df %>% filter(date > date_mise_en_eau)
  
  if (!any(df_after$is_dry)) {
    date_assec <- NA
  } else {
    
    r <- rle(df_after$is_dry)
    ends   <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1
    
    # Séquences sèches suffisamment longues
    valid <- which(r$values & r$lengths >= N_CONFIRM)
    
    if (length(valid) == 0) {
      date_assec <- NA
    } else {
      date_assec <- df_after$date[starts[valid[1]]]
    }
  }
  
  tibble(
    date_mise_en_eau = date_mise_en_eau,
    date_assec       = date_assec
  )
}

#Application sur les zones 
hydro_dates <- stats_table %>%
  group_by(ID_LAG, Site) %>%
  group_modify(~ detect_hydro_dates(.x)) %>%
  ungroup()

#Durée de mise en eau
hydro_dates <- hydro_dates %>%
  mutate(
    duree_inondation_jours = as.numeric(date_assec - date_mise_en_eau)
  )

################################################
#### Détection des cycles hydro d'une année #### 
################################################

#Boucle de detection des transitions
detect_hydro_cycles_disjoint <- function(df, N_CONFIRM, MIN_DRY_DAYS = 15) {
  
  df <- df %>% arrange(date)
  
  r <- rle(
    ifelse(df$is_water, "water",
           ifelse(df$is_dry, "dry", "other"))
  )
  
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1
  
  cycles <- list()
  cycle_id <- 1
  i <- 1
  n_seq <- length(r$values)
  
  while (i <= n_seq) {
    
    # Séquence eau validée
    if (!is.na(r$values[i]) &&
        r$values[i] == "water" &&
        r$lengths[i] >= N_CONFIRM) {
      
      # Cherche la première séquence sèche validée après eau
      found_dry <- FALSE
      j <- i + 1
      while (j <= n_seq) {
        if (!is.na(r$values[j]) &&
            r$values[j] == "dry" &&
            r$lengths[j] >= N_CONFIRM) {
          
          # Vérifier que l'assec dure au moins MIN_DRY_DAYS
          dry_days <- as.numeric(df$date[ends[j]] - df$date[starts[j]]) + 1
          if (dry_days >= MIN_DRY_DAYS) {
            cycles[[cycle_id]] <- tibble(
              cycle_id        = cycle_id,
              date_mise_en_eau = df$date[starts[i]],
              date_assec       = df$date[starts[j]]
            )
            
            cycle_id <- cycle_id + 1
            i <- j + 1  # On saute toutes les séquences jusqu'à la fin de l’assec
            found_dry <- TRUE
            break
          }
        }
        j <- j + 1
      }
      
      # Si pas de séquence sèche assez longue, fin du traitement
      if (!found_dry) break
      
    } else {
      i <- i + 1
    }
  }
  
  if (length(cycles) == 0) {
    return(tibble(
      cycle_id = integer(),
      date_mise_en_eau = as.Date(character()),
      date_assec = as.Date(character())
    ))
  }
  
  bind_rows(cycles)
}

#Application sur les zones 
hydro_cycles <- stats_table %>%
  group_by(ID_LAG, Site) %>%
  group_modify(~ detect_hydro_cycles_disjoint(.x, N_CONFIRM)) %>%
  ungroup() %>%
  mutate(
    duree_inondation_jours = as.numeric(date_assec - date_mise_en_eau)
  )

#Durée de mise en eau de chaque cycle
hydro_cycles <- hydro_cycles %>%
  mutate(
    duree_inondation_jours = as.numeric(date_assec - date_mise_en_eau)
  )

#### Enregistrement du data_frame ####
write.csv(hydro_cycles, "hydro_cycles_19_20.csv", row.names = FALSE)
