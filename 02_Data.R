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
sites_mars_2025 <- c("Chaumadou", "Capelude", "La Grande Motte", "La Palme")

Hydro_germi <- Hydro %>%
  mutate(
    annee = year(date_releve),
    mois = month(date_releve)
  ) %>%
  filter(
    # Garder avril 2020 et avril 2025 pour tous les sites
    (annee %in% c(2020, 2025) & mois == 4) |
      # Ajouter mars 2025 uniquement pour certains sites
      (annee == 2025 & mois == 3 & site %in% sites_mars_2025)
  )

Hydro_germi = Hydro_germi[,-c(3,4,5,6,13,15)]
Hydro_germi = Hydro_germi [,c(1,2,9,3:8)]
Hydro_germi = Hydro_germi %>%
  mutate(eau = if_else(tolower(eau) == "oui", 1, 0))

sites_a_exclure =  c("Hyeres", "Villeroy", "Salses-Leucate", "Canet", "Pissevaches")
Hydro_germi = Hydro_germi %>%
  filter(!site %in% sites_a_exclure)


# Importer dans processed 
write.csv(Hydro_germi, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro/Hydro_germi.csv", row.names = FALSE)
