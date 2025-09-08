#Explo données Flore

#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Macrophytes_/Data/Processed")
Cov_tot_moy = read.csv("Macro_CovTot_moy_2025.csv", header = TRUE, sep = ",", dec=".")

Cov_tot_moy = Cov_tot_moy[,-21]

Cov_tot_moy = Cov_tot_moy %>%
  mutate(across(4:20, as.numeric))

#### EXPLO INTER-SITES ####
#Existe-il une difference entre les sites ? 

Cov_tot_moy_mat_inter <- Cov_tot_moy %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_")) %>%  # Créer Site_Annee
  select(Site_Annee, everything(), -c(Site, Annee, LAGUNE))  # Réorganiser les colonnes

df_aggregated_inter <- Cov_tot_moy_mat_inter %>%
  group_by(Site_Annee) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")

# --- GGPLOTS ---

# 1. Recouvrement moyen par site (toutes années confondues)
data_long_inter <- Cov_tot_moy %>%
  select(-Annee, -LAGUNE) %>%
  pivot_longer(-Site, names_to = "Espece", values_to = "Recouvrement")

ggplot(data_long_inter, aes(x = Site, y = Recouvrement, fill = Espece)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Recouvrement moyen des espèces par site",
       x = "Site", y = "Taux de recouvrement (%)", fill = "Espèce") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 2. Recouvrement moyen par site et par année
df_moyenne_inter <- Cov_tot_moy %>%
  pivot_longer(cols = -c(Annee, Site, LAGUNE), names_to = "Espece", values_to = "Recouvrement") %>%
  group_by(Annee, Site, Espece) %>%
  summarise(Recouvrement_Moyen = mean(Recouvrement, na.rm = TRUE), .groups = "drop")

# Graphes en barres par année
lapply(unique(df_moyenne_inter$Annee), function(annee) {
  df_annee <- filter(df_moyenne_inter, Annee == annee)
  ggplot(df_annee, aes(x = Site, y = Recouvrement_Moyen, fill = Espece)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste("Recouvrement moyen par site pour l'année", annee),
         x = "Site", y = "Taux de recouvrement (%)") +
    theme_minimal() %>%
    print()
})

# 3. Recouvrement moyen par Site_Annee (sur un même graphique ou facetté)
df_moyenne_2_inter <- df_moyenne_inter %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_")) %>%
  group_by(Site_Annee, Espece) %>%
  summarise(Recouvrement_Moyen = mean(Recouvrement_Moyen, na.rm = TRUE), .groups = "drop") %>%
  mutate(Site = sub("_.*", "", Site_Annee))  # Extraire le site pour facettage

# Graph global
ggplot(df_moyenne_2_inter, aes(x = Site_Annee, y = Recouvrement_Moyen, fill = Espece)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Recouvrement moyen par Site et Année",
       x = "Site et Année", y = "Recouvrement moyen (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Graph facetté par site
ggplot(df_moyenne_2_inter, aes(x = Site_Annee, y = Recouvrement_Moyen, fill = Espece)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Recouvrement moyen par Site et Année",
       x = "Site et Année", y = "Recouvrement moyen (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Site, scales = "free_x")

# 4. Graphiques en courbes - Évolution temporelle
data_long_courbes_inter <- Cov_tot_moy %>%
  group_by(Annee, Site) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(cols = -c(Annee, Site), names_to = "Espece", values_to = "Pourcentage")

# Tout sur un même graphique avec facettes
ggplot(data_long_courbes_inter, aes(x = Annee, y = Pourcentage, color = Espece, group = Espece)) +
  geom_line() + geom_point() +
  facet_wrap(~ Site, scales = "free_y") +
  labs(title = "Évolution des pourcentages de recouvrement des espèces par site",
       x = "Année", y = "Pourcentage de recouvrement") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Un graphique par site
lapply(unique(data_long_courbes_inter$Site), function(site) {
  ggplot(filter(data_long_courbes_inter, Site == site),
         aes(x = Annee, y = Pourcentage, color = Espece, group = Espece)) +
    geom_line() + geom_point() +
    labs(title = paste("Évolution des pourcentages pour", site),
         x = "Année", y = "Pourcentage de recouvrement") +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 3))) %>%
    print()
})

#### EXPLO INTRA-SITES #### 
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

#Nuage de points 
##/!\Repartir du dataframe de base /!\##
Cov_tot_moy = Cov_tot_moy %>%
  mutate(across(4:21, as.numeric))

Cov_tot_moy <- Cov_tot_moy %>%
  mutate(LAGUNE = str_remove(LAGUNE, "^\\d{4}_"))

variables = "TOT"

for (var in variables) {
  data_filtered = Cov_tot_moy %>%
    filter(Annee %in% c(2020, 2025)) %>%
    select(Site, LAGUNE, Annee, value = all_of(var))
  
  data_wide = data_filtered %>%
    pivot_wider(
      names_from = Annee,
      values_from = value,
      names_prefix = paste0(gsub("[^A-Za-z0-9]", "_", var), "_")
    )
  
  print(colnames(data_wide))  # Debug
  
  col_2020 = colnames(data_wide)[grepl("2020$", colnames(data_wide))]
  col_2025 = colnames(data_wide)[grepl("2025$", colnames(data_wide))]
  
  if (length(col_2020) == 1 && length(col_2025) == 1) {
    data_wide = data_wide %>%
      filter(!is.na(!!sym(col_2020)) & !is.na(!!sym(col_2025)))
    
    print(nrow(data_wide))  # Debug
    
    sites = unique(data_wide$Site)
    
    for (s in sites) {
      message(paste("Traitement du site:", s))
      df_site = data_wide %>% filter(Site == s)
      
      p = ggplot(df_site, aes_string(x = col_2025, y = col_2020, color = "LAGUNE")) +
        geom_point(size = 3) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
        coord_fixed(ratio = 1, expand = TRUE) + 
        labs(
          title = paste0("Évolution du recouvrement végétal (%) sur le site : ", s),
          x = paste0(var, " en 2025"),
          y = paste0(var, " en 2020"),
          color = "Lagune"
        ) +
        theme_minimal()
      
      print(p)  # Affiche le graphique
    }
    
  } else {
    message(paste("Colonnes manquantes pour la variable", var))
  }
}



#### EXPLO INTER-STATIONS ####
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



#### ANALYSE MULTIVARIEE ####


Data_multi = Cov_tot_moy

sites_a_exclure = c("BPA_PIS", "BAG_GRA", "CAN_NAZ", "HYE_VIE", "THA_SET")

Data_multi = Data_multi %>%
  filter(!(Site %in% sites_a_exclure))

Data_multi$Annee = as.factor(Data_multi$Annee)
Data_multi$Site = as.factor(Data_multi$Site)
Data_multi$LAGUNE = as.factor(Data_multi$LAGUNE)

Data_multi = Data_multi %>%
  mutate(piece_eau = sub("^[0-9]{4}_", "", LAGUNE))

Data_multi = Data_multi %>%
  select(colnames(Data_multi)[c(1:3, 21, 4:20)])

df_species = Data_multi %>% select(5:21)
df_meta = Data_multi %>% select(Annee, Site, LAGUNE, piece_eau)

#NMDS :  distance ecologique mais suppression des lignes avec que des zéros  #----

# Supprimer les lignes où toutes les espèces sont à 0 ou NA 
ligne_non_vides = rowSums(df_species, na.rm = TRUE) > 0 
df_species_clean = df_species[ligne_non_vides, ] 
# Supprimer les colonnes (espèces) qui sont toujours à 0 ou NA 
colonne_non_vides = colSums(df_species_clean, na.rm = TRUE) > 0 
df_species_clean = df_species_clean[, colonne_non_vides] 
# Remplacer les NA par 0 si nécessaire 
df_species_clean[is.na(df_species_clean)] <- 0 
df_meta_clean = df_meta[ligne_non_vides, ]


#NMDS
nmds = metaMDS(df_species_clean, distance = "bray", k = 2, trymax = 100)

# Extraire les scores (coordonnées)
scores_df = as.data.frame(scores(nmds, display = "sites"))
scores_df = bind_cols(df_meta_clean, scores_df)

lagunes_2020_2025 = scores_df %>%
  filter(Annee %in% c(2020, 2025)) %>%   # On prend uniquement 2020 et 2025
  group_by(LAGUNE) %>%
  summarise(annees_presente = list(unique(Annee))) %>%
  filter(all(c(2020, 2025) %in% unlist(annees_presente))) %>%
  pull(LAGUNE)

scores_df_filtre = scores_df %>%
  filter(LAGUNE %in% lagunes_2020_2025)

#Graph1 : Ellipses par sites 
ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(size = 1) +
  stat_ellipse(type = "t") +
  theme_minimal() +
  labs(title = "Nuage de points NMDS + Ellipses par Site")

#Graph2 : Ellipses par Annees 
scores_df_filtre_ellipses = scores_df_filtre %>%
  group_by(Site) %>%
  filter(n() >= 3) %>%
  ungroup()

ggplot(scores_df_filtre, aes(x = NMDS1, y = NMDS2, color = Annee)) +
  geom_point(size = 2) +
  stat_ellipse(type = "t") +
  theme_minimal() +
  labs(title = "NMDS + Ellipses par Année")

#Graph 3 : Ellipses par Site/Annees + fleches 

scores_df_filtre = scores_df_filtre %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_"))

scores_df_filtre %>%
  group_by(Site_Annee) %>%
  summarise(nb_points = n()) %>%
  filter(nb_points < 3)
scores_df_filtre_ellipse <- scores_df_filtre %>%
  group_by(Site_Annee) %>%
  filter(n() >= 3) %>%
  ungroup()

centroids = scores_df_filtre_ellipse %>%
  group_by(Site, Annee, Site_Annee) %>%
  summarise(
    NMDS1 = mean(NMDS1, na.rm = TRUE),
    NMDS2 = mean(NMDS2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Site, Annee)

arrows_df = centroids %>%
  group_by(Site) %>%
  arrange(Annee) %>%
  mutate(
    NMDS1_end = lead(NMDS1),
    NMDS2_end = lead(NMDS2)
  ) %>%
  filter(!is.na(NMDS1_end) & !is.na(NMDS2_end)) %>%
  ungroup()

ggplot(scores_df_filtre_ellipse, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(aes(shape = Annee), size = 2) +
  stat_ellipse(aes(group = Site_Annee), type = "t") +
  geom_segment(data = arrows_df,
               aes(x = NMDS1, y = NMDS2, xend = NMDS1_end, yend = NMDS2_end, color = Site),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")),
               linewidth = 1) +
  theme_minimal() +
  labs(title = "Trajectoire temporelle des Sites dans l’espace NMDS (flèches entre ellipses)")



#Graph 4 : Trajectoires par lagunes 
ggplot(scores_df_filtre, aes(x = NMDS1, y = NMDS2)) +
  geom_path(aes(group = piece_eau), 
            arrow = arrow(type = "closed", length = unit(0.15, "inches")),
            color = "grey50", linewidth = 0.8) +
  geom_point(aes(color = Annee), size = 3) +
  theme_minimal() +
  labs(
    title = "Trajectoires temporelles (coloration par année)",
    x = "NMDS1",
    y = "NMDS2",
    color = "Année"
  )





#NMDS + TRICHE : Ajouter une minuscule valeur pour les valeurs à zéro ---- 

colonnes_non_vides_triche <- colSums(df_species) > 0
df_species_clean_triche <- df_species[, colonnes_non_vides_triche]

# Identifier les lignes vides (toutes 0)
lignes_vides_triche <- rowSums(df_species_clean_triche) == 0
# Remplacer les lignes vides par une petite valeur (ex: 1e-6)
df_species_clean_triche[lignes_vides_triche, ] <- 1e-6

#test 
# 1. Supprimer les colonnes d'espèces qui sont toutes à 0
colonnes_non_vides_triche <- colSums(df_species, na.rm = TRUE) > 0
df_species_clean_triche <- df_species[, colonnes_non_vides_triche]

# 2. Identifier les lignes totalement nulles (sites sans aucune espèce)
lignes_vides_triche <- rowSums(df_species_clean_triche) == 0

# 3. Remplacer les lignes vides par une valeur minimale (triche)
df_species_clean_triche[lignes_vides_triche, ] <- 1e-6

# 4. Mettre à jour les métadonnées (pas besoin de filtrer ici)
df_meta_clean_triche <- df_meta  # Même longueur que df_species_clean_triche

#NMDS
nmds_triche = metaMDS(df_species_clean_triche, distance = "bray", k = 2, trymax = 100)

# Extraire les scores (coordonnées)
scores_df_triche = as.data.frame(scores(nmds_triche, display = "sites"))
scores_df_triche = bind_cols(df_meta_clean_triche, scores_df_triche)

lagunes_2020_2025 = scores_df_triche %>%
  filter(Annee %in% c(2020, 2025)) %>%   # On prend uniquement 2020 et 2025
  group_by(LAGUNE) %>%
  summarise(annees_presente = list(unique(Annee))) %>%
  filter(all(c(2020, 2025) %in% unlist(annees_presente))) %>%
  pull(LAGUNE)

scores_df_triche_filtre = scores_df_triche %>%
  filter(LAGUNE %in% lagunes_2020_2025)

#Graph1 : Ellipses par sites 
ggplot(scores_df_triche, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(size = 1) +
  stat_ellipse(type = "t") +
  theme_minimal() +
  labs(title = "Nuage de points NMDS + Ellipses par Site")

#Graph2 : Ellipses par Annees 
scores_df_triche_filtre_ellipses = scores_df_triche_filtre %>%
  group_by(Site) %>%
  filter(n() >= 3) %>%
  ungroup()

ggplot(scores_df_triche_filtre, aes(x = NMDS1, y = NMDS2, color = Annee)) +
  geom_point(size = 2) +
  stat_ellipse(type = "t") +
  theme_minimal() +
  labs(title = "NMDS + Ellipses par Année")

#Graph 3 : Ellipses par Site/Annees + fleches 

scores_df_triche_filtre = scores_df_triche_filtre %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_"))

scores_df_triche_filtre %>%
  group_by(Site_Annee) %>%
  summarise(nb_points = n()) %>%
  filter(nb_points < 3)
scores_df_triche_filtre_ellipse <- scores_df_triche_filtre %>%
  group_by(Site_Annee) %>%
  filter(n() >= 3) %>%
  ungroup()

centroids_triche = scores_df_triche_filtre_ellipse %>%
  group_by(Site, Annee, Site_Annee) %>%
  summarise(
    NMDS1 = mean(NMDS1, na.rm = TRUE),
    NMDS2 = mean(NMDS2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Site, Annee)

arrows_df_triche = centroids_triche %>%
  group_by(Site) %>%
  arrange(Annee) %>%
  mutate(
    NMDS1_end = lead(NMDS1),
    NMDS2_end = lead(NMDS2)
  ) %>%
  filter(!is.na(NMDS1_end) & !is.na(NMDS2_end)) %>%
  ungroup()

ggplot(scores_df_triche_filtre_ellipse, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(aes(shape = Annee), size = 2) +
  stat_ellipse(aes(group = Site_Annee), type = "t") +
  geom_segment(data = arrows_df_triche,
               aes(x = NMDS1, y = NMDS2, xend = NMDS1_end, yend = NMDS2_end, color = Site),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")),
               linewidth = 1) +
  theme_minimal() +
  labs(title = "Trajectoire temporelle des Sites dans l’espace NMDS (flèches entre ellipses)")



#Graph 4 : Trajectoires par lagunes 
ggplot(scores_df_triche_filtre, aes(x = NMDS1, y = NMDS2)) +
  geom_path(aes(group = piece_eau), 
            arrow = arrow(type = "closed", length = unit(0.09, "inches")),
            color = "grey50", linewidth = 0.8) +
  geom_point(aes(color = Annee), size = 1) +
  theme_minimal() +
  labs(
    title = "Trajectoires temporelles (coloration par année)",
    x = "NMDS1",
    y = "NMDS2",
    color = "Année"
  )
