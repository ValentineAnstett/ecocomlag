####DATA HYDRO GERMI####
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro = read.csv("Hydro_Finale_19_23_25.csv", header = TRUE, sep = ",", dec=",")

Hydro = Hydro %>%
  mutate(date_releve = as.Date(date_releve, format = "%Y-%m-%d"))

##Faire data frame avec valeur unique pour chaque année

#Valeur du mois de germination 

Hydro_mois = Hydro %>%
  mutate(
    annee = year(date_releve),
    mois = month(date_releve)
  ) %>%
  filter(
    annee %in% c(2019,2020, 2025),
    mois %in% c(4, 5, 6)  # avril, mai, juin
  )
# Nombre de sites uniques par année et mois
site_counts = Hydro_mois %>%
  group_by(annee, mois) %>%
  summarise(nb_sites = n_distinct(site), .groups = "drop")
print(site_counts)

# Trouver le mois qui a le plus de sites pour chaque année
mois_max_sites = site_counts %>%
  group_by(annee) %>%
  filter(nb_sites == max(nb_sites)) %>%
  ungroup()

print("Mois optimal par année :")
print(mois_max_sites)

#Creer le dataframe pour germi


Hydro_germi <- Hydro %>%
  mutate(
    annee = year(date_releve),
    mois = month(date_releve)
  ) %>%
  filter(
    # Garder avril 2020 et avril 2025 pour tous les sites
    (annee == 2020 & mois == 4) |
      (annee == 2025 & mois == 4 & !site %in% c("Orpellieres", "La Grande Motte")) |
      # Mars 2025 uniquement pour ces sites
      (annee == 2025 & mois == 3 & site %in% c("Orpellieres", "La Grande Motte","Capelude","Chaumadou","La Palme"))
  )

Hydro_germi = Hydro_germi[,-c(3,4,5,6,13,15)]
Hydro_germi = Hydro_germi [,c(1,2,9,3:8)]
Hydro_germi = Hydro_germi %>%
  mutate(eau = if_else(tolower(eau) == "oui", 1, 0))

Hydro_germi= Hydro_germi %>%
  mutate(across(6:9, ~as.numeric(gsub(",", ".", .x))))
Hydro_germi = Hydro_germi %>%
  mutate(salinite = round(salinite, 1),
         conductivite = round(conductivite, 1)
         )

sites_a_exclure =  c("Hyeres", "Villeroy", "Salses-Leucate", "Canet", "Pissevaches")
Hydro_germi = Hydro_germi %>%
  filter(!site %in% sites_a_exclure)
lag_a_exclure =  c("PCA_CHA_02", "PCA_CHA_04", "ORB_ORP_11")
Hydro_germi = Hydro_germi %>%
  filter(!code %in% lag_a_exclure)

# Importer dans processed 
write.csv(Hydro_germi, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro/Hydro_germi.csv", row.names = FALSE)



####DATA HYDRO MENSUELLE####
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro = read.csv("Hydro_Finale_19_23_25.csv", header = TRUE, sep = ",", dec=",")

Hydro = Hydro %>%
  mutate(date_releve = as.Date(date_releve, format = "%Y-%m-%d"))


#Ne garder que les sites suivis mensuellement en 2020+2023+2025
codes_a_garder = c(
  # SIG_GSA
  "SIG_GSA_01", "SIG_GSA_04", "SIG_GSA_06", "SIG_GSA_07", "SIG_GSA_08", "SIG_GSA_09", "SIG_GSA_10",
  
  # ORB_ORP
  "ORB_ORP_01", "ORB_ORP_03", "ORB_ORP_05", "ORB_ORP_06", "ORB_ORP_08",
  
  # BAG_PET
  "BAG_PET_01", "BAG_PET_02", "BAG_PET_03", "BAG_PET_04", "BAG_PET_05", "BAG_PET_06", "BAG_PET_07", "BAG_PET_08", "BAG_PET_11",
  
  # EOR_MOT
  "EOR_MOT_04", "EOR_MOT_05", "EOR_MOT_06", "EOR_MOT_07", "EOR_MOT_08", "EOR_MOT_10",
  
  # GCA_RNC
  "GCA_RNC_01", "GCA_RNC_03", "GCA_RNC_06", "GCA_RNC_07", "GCA_RNC_08", "GCA_RNC_09", "GCA_RNC_10",
  
  # FOS_REL
  "FOS_REL_05", "FOS_REL_07", "FOS_REL_08", "FOS_REL_09", "FOS_REL_10"
)

Hydro_mens = Hydro[Hydro$code %in% codes_a_garder, ]

#Format date (retirer le jour)
Hydro_mens$date_releve = as.Date(format(Hydro_mens$date_releve, "%Y-%m-01"))
Hydro_mens = Hydro_mens[,-c(3,7,13)]

# Importer dans processed 
write.csv(Hydro_mens, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro/Hydro_mens.csv", row.names = FALSE)



