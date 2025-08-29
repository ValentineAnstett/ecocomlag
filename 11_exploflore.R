#Explo données Flore

#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Macrophytes_/Data/Processed")
Cov_tot_moy = read.csv("Macro_CovTot_moy_2025.csv", header = TRUE, sep = ",", dec=".")

Cov_tot_moy = Cov_tot_moy[,-21]

Cov_tot_moy = Cov_tot_moy %>%
  mutate(across(4:20, as.numeric))

#### EXPLO INTER-SITES #### ----
#Existe-il une difference entre les sites ? 

#Agréger les lignes stations en une seule ligne site 
Cov_tot_moy_mat_inter = Cov_tot_moy %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_")) #Creer code anneeXsite pour regrouper
Cov_tot_moy_mat_inter = Cov_tot_moy_mat_inter[,-(1:3)]
Cov_tot_moy_mat_inter = Cov_tot_moy_mat_inter %>%
  select(colnames(Cov_tot_moy_mat_inter)[c(18, 1:17)])

df_aggregated_inter = aggregate(. ~ Site_Annee, data = Cov_tot_moy_mat_inter, FUN = mean) #Agreger les lignes des memes sites 

###GGPLOTS###
#Années confondues
Data_inter = Cov_tot_moy[, -c(1,3)]
data_long_inter = Data_inter %>%
  pivot_longer(cols = -Site, names_to = "Espèce", values_to = "Recouvrement")

ggplot(data_long_inter, aes(x = Site, y = Recouvrement, fill = Espèce)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Recouvrement moyen des espèces par site",
    x = "Site",
    y = "Taux de recouvrement (%)",
    fill = "Espèce"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Faire pivoter les noms des lagunes

#Année par année 
data_long_2_inter = Cov_tot_moy%>%
  pivot_longer(cols = -c(Annee,Site, LAGUNE), 
               names_to = "Espece", 
               values_to = "Recouvrement")

df_moyenne_inter = data_long_2_inter %>%
  group_by(Annee, Site, Espece) %>%
  summarise(Recouvrement_Moyen = mean(Recouvrement, na.rm = TRUE), .groups = "drop")


for(annee in unique(df_moyenne_inter$Annee)) {
  df_annee_inter = df_moyenne_inter %>% filter(Annee == annee)
  p = ggplot(df_annee_inter, aes(x = Site, y = Recouvrement_Moyen, fill = Espece)) + 
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Recouvrement moyen par site pour l'année", annee),
         x = "Site",
         y = "Taux de recouvrement (%)") +
    theme_minimal()
  print(p)
}

#Combinaison site-Année 
#Sur le même graph
data_long_3_inter = data_long_2_inter %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_"))
data_long_3_inter = data_long_3_inter[,-c(1,2,3)]

df_moyenne_2_inter = data_long_3_inter %>%
  group_by(Site_Annee, Espece) %>%
  summarise(Recouvrement_Moyen = mean(Recouvrement, na.rm = TRUE), .groups = "drop")

ggplot(df_moyenne_2_inter, aes(x = Site_Annee, y = Recouvrement_Moyen, fill = Espece)) +
  geom_bar(stat = "identity", position = "Stack") +
  labs(title = "Recouvrement moyen par Site et Année",
       x = "Site et Année",
       y = "Recouvrement moyen (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Extraire le site de la colonne Site_Annee pour la facette
df_moyenne_2_inter$Site = sapply(strsplit(as.character(df_moyenne_2_inter$Site_Annee), "_"), function(x) x[1])
ggplot(df_moyenne_2_inter, aes(x = Site_Annee, y = Recouvrement_Moyen, fill = Espece)) +
  geom_bar(stat = "identity", position = "Stack") +
  labs(title = "Recouvrement moyen par Site et Année",
       x = "Site et Année",
       y = "Recouvrement moyen (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Site, scales = "free_x")

### Graphs en courbes 

data_avg_inter = Cov_tot_moy %>%
  group_by(Annee, Site) %>%
  summarise(across(2:18, mean, na.rm = TRUE), .groups = "drop")

data_long_courbes_inter = data_avg_inter %>%
  pivot_longer(cols = -c(Annee,Site), 
               names_to = "Espèce", 
               values_to = "Pourcentage")

#Tout sur le même graph
ggplot(data_long_courbes_inter, aes(x = Annee, y = Pourcentage, color = Espèce, group = Espèce)) +
  geom_line() +  # Ajouter les lignes pour les courbes
  geom_point() +  # Ajouter les points pour chaque année
  facet_wrap(~ Site, scales = "free_y") +  # Créer un graphique pour chaque site avec des axes Y indépendants
  labs(title = "Evolution des pourcentages de recouvrement des espèces par site",
       x = "Année", 
       y = "Pourcentage de recouvrement") +
  theme_minimal() + 
  theme(legend.title = element_blank())

#Un graph par site 

sites = unique(data_long_courbes_inter$Site)
for (site in sites) {
  data_site_inter = subset(data_long_courbes_inter, Site == site)
  q = ggplot(data_site_inter, aes(x = Annee, y = Pourcentage, color = Espèce, group = Espèce)) +
    geom_line() +  # Ajouter les lignes pour les courbes
    geom_point() +  # Ajouter les points pour chaque année
    labs(title = paste("Evolution des pourcentages de recouvrement des espèces pour", site),
         x = "Année", 
         y = "Pourcentage de recouvrement") +
    theme_minimal() + 
    theme(legend.title = element_blank()) + 
    guides(color = guide_legend(override.aes = list(size = 3)))
  print(q)
}


#### EXPLO INTRA-SITES #### ---- 
#Existe t-il une différence entre les lagunes d'un même site ? 

data_long_intra = Cov_tot_moy %>%
  pivot_longer(cols = -c(Annee, Site, LAGUNE), names_to = "Espece", values_to = "Recouvrement")

data_long_intra$LAGUNE = gsub("^\\d{4}_", "", data_long_intra$LAGUNE)

###GGPLOTS###

##Annes par annees 
#Sur le meme graph
ggplot(data_long_intra, aes(x = LAGUNE, y = Recouvrement, fill = Espece)) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(Site ~ Annee) +  
  labs(title = "Recouvrement des espèces par LAGUNE, Site et Année",
       x = "LAGUNE", 
       y = "Pourcentage de recouvrement (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Graph par annee 
sites_intra = unique(data_long_intra$Site)
annees_intra = unique(data_long_intra$Annee)
especes_intra = colnames(data_long_intra)[4:ncol(data_long)]

for (site in sites_intra) {
  for (annee in annees_intra) {
    data_site_annee_intra = data_long_intra %>%
      filter(Site == site & Annee == annee)
    p = ggplot(data_site_annee_intra, aes(x = LAGUNE, y = Recouvrement, fill = Espece)) +
      geom_bar(stat = "identity", position = "stack") + 
      labs(title = paste("Recouvrement des espèces pour le site", site, "et l'année", annee),
           x = "LAGUNE", 
           y = "Pourcentage de recouvrement (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}

#Graphs par site 
sites_intra = unique(data_long_intra$Site)
especes_intra = colnames(data_long_intra)[4:ncol(data_long_intra)]

for (site in sites_intra) {
  data_site_intra = data_long_intra %>%
    filter(Site == site)
  p = ggplot(data_site_intra, aes(x = LAGUNE, y = Recouvrement, fill = Espece)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Recouvrement des espèces pour le site", site),
         x = "LAGUNE", 
         y = "Pourcentage de recouvrement (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#### EXPLO INTER-STATIONS #### ----
#Y a t il une difference entre les lagunes (stations), tous sites confondus

Cov_tot_moy_stations = Cov_tot_moy [,-21]
Data_stations = Cov_tot_moy_stations[, c(-1, -2)]

data_long_stations = Data_stations %>%
  pivot_longer(cols = -LAGUNE, names_to = "Espèce", values_to = "Recouvrement")

###GGPLOTS###
ggplot(data_long_stations, aes(x = LAGUNE, y = Recouvrement, fill = Espèce)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Recouvrement des espèces par Lagune",
    x = "Lagune",
    y = "Taux de recouvrement (%)",
    fill = "Espèce"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


