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
Hydro_germi = Hydro %>%
  mutate(
    annee = year(date_releve),
    mois = month(date_releve)
  ) %>%
  filter(
    annee %in% c(2020, 2025),
    mois == 4
  )
# Importer dans processed 
write.csv(Hydro_germi, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro/Hydro_germi.csv", row.names = FALSE)
