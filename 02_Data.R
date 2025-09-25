####DATA HYDRO####
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
