#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Sol/Data/Processed")
Sol = read.csv("Sol_21_25.csv", header = TRUE, sep = ",", dec=",")

Sol= Sol %>%
  mutate(across(4:14, as.numeric))

####INTER-SITES####
#Type de sol 
Sol_sanslag = Sol [,-3]
Data = Sol_sanslag [, -c(3:6)]
data_long = Data %>%
  pivot_longer(cols = -c(Site, Annee), names_to = "Sol", values_to = "Pourcentage")
ggplot(data_long, aes(x = Site, y = Pourcentage, fill = Sol)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Annee) +
  labs(
    title = "Type de sol par site",
    x = "Site",
    y = "Pourcentage (%)",
    fill = "Sol"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Faire pivoter les noms des lagunes

#Chimie ? 

Data1 = Sol_sanslag [, -c(7:14)]
data_long1 = Data1 %>%
  pivot_longer(cols = -c(Site, Annee), names_to = "Sol", values_to = "Pourcentage")
ggplot(data_long1, aes(x = Site, y = Pourcentage, fill = Sol)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Annee) +
  labs(
    title = "Type de sol par site",
    x = "Site",
    y = "Pourcentage (%)",
    fill = "Sol"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Faire pivoter les noms des lagunes


#Evolution Azote dans le sol 2019/2025

azote_data = Sol %>%
  select(Site, ID_LAG, Annee, AZOTE_TOT.mg.kg.) %>%
  pivot_wider(names_from = Annee, values_from = AZOTE_TOT.mg.kg. , names_prefix = "Azote_") %>%
  drop_na(Azote_2021, Azote_2025)

ggplot(azote_data, aes(x = Azote_2025, y = Azote_2021, color = Site)) +
  geom_point(size = 3) +
  labs(
    x = "Azote en 2025",
    y = "Azote en 2021",
    title = "Comparaison des niveaux d'Azote dans le sol (%) par site (2021 vs 2025)",
    color = "Site"
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal()




####INTRA-SITES####

##GGPLOTS##
#Type de sol 
Sol_sanslag = Sol[,-1]
Data_sanslag = Sol_sanslag [, -c(2:4)]
data_long_sanslag = Data_sanslag %>%
  pivot_longer(cols = -Site, names_to = "Sol", values_to = "Pourcentage")
ggplot(data_long_sanslag, aes(x = Site, y = Pourcentage, fill = Sol)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Type de sol par site",
    x = "Site",
    y = "Pourcentage (%)",
    fill = "Sol"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Faire pivoter les noms des lagunes


##GGPLOTS##
#POURCENTAGES 
Sol_pourcentage = Sol[, -c(5,6,7)]

data_long_pourcentage = Sol_pourcentage %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "Compo_sol", values_to = "Pourcentage")
##Annes par annees 
#Sur le meme graph
ggplot(data_long_pourcentage, aes(x = ID_LAG, y = Pourcentage, fill = Compo_sol)) +
  geom_bar(stat = "identity", position = "stack") + 
  facet_grid(Site ~ Annee) +  
  labs(title = "Composition du sol",
       x = "LAGUNE", 
       y = "Pourcentage dans le sol (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Graph par annee 
sites = unique(data_long_pourcentage$Site)
annees = unique(data_long_pourcentage$Annee)
especes = colnames(data_long_pourcentage)[4:ncol(data_long_pourcentage)]

for (site in sites) {
  for (annee in annees) {
    data_site_annee = data_long_pourcentage %>%
      filter(Site == site & Annee == annee)
    p = ggplot(data_site_annee, aes(x = ID_LAG, y = Pourcentage, fill = Compo_sol)) +
      geom_bar(stat = "identity", position = "stack") + 
      labs(title = paste("Composition du sol pour le site", site, "et l'année", annee),
           x = "LAGUNE", 
           y = "Pourcentage (%)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
}
#Graphs par site 
sites = unique(data_long_pourcentage$Site)
especes = colnames(data_long_pourcentage)[4:ncol(data_long)]

for (site in sites) {
  data_site = data_long_pourcentage %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = Pourcentage, fill = Compo_sol)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Composition du sol pour le site", site),
         x = "LAGUNE", 
         y = "Pourcentage (%)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}


###C/N
Sol_CARBAZ = Sol[, c(1,2,3,7)]

data_long_CARBAZ = Sol_CARBAZ %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "CarboneAzote", values_to = "Valeur")
#Graphs par site 
sites = unique(data_long_CARBAZ$Site)
especes = colnames(data_long_CARBAZ)[4:ncol(data_long_CARBAZ)]

for (site in sites) {
  data_site = data_long_CARBAZ %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = Valeur, fill = CarboneAzote)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Rapport C/N pour le site", site),
         x = "LAGUNE", 
         y = "C/N") +
    scale_fill_manual(values = (CarboneAzote = "#a6761d")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}


#P2O5 -- engrais phosphatés 

Sol_P2O5 = Sol[, c(1,2,3,5)]

data_long_P2O5 = Sol_P2O5 %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "Engrais_phosphates", values_to = "Pourcentage")
#Graphs par site 
sites = unique(data_long_P2O5$Site)
especes = colnames(data_long_P2O5)[4:ncol(data_long_P2O5)]

for (site in sites) {
  data_site = data_long_P2O5 %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = Pourcentage, fill = Engrais_phosphates)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Pourcentage de P2O5 dans le sol pour le site", site),
         x = "LAGUNE", 
         y = "P2O5 (%)") +
    scale_fill_manual(values = (Engrais_phosphates = "#1b9e77")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

#Azote Tot 
Sol_N = Sol[, c(1,2,3,6)]

data_long_N = Sol_N %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "AzoteTotal", values_to = "mg_kg")
#Graphs par site 
sites = unique(data_long_N$Site)
especes = colnames(data_long_N)[4:ncol(data_long_N)]

for (site in sites) {
  data_site = data_long_N %>%
    filter(Site == site)
  p = ggplot(data_site, aes(x = ID_LAG, y = mg_kg, fill = AzoteTotal)) +
    geom_bar(stat = "identity", position = "stack") +  
    facet_wrap(~ Annee) +  
    labs(title = paste("Quantité d'azote dans le sol sur le site", site),
         x = "LAGUNE", 
         y = "N (mg/kg)") +
    scale_fill_manual(values = (Engrais_phosphates = "#7570b3")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

##NUAGE DE POINT = Evolution par site et par composés ###

variables = c("MO_TOT...","P2O5_TOT...","AZOTE_TOT.mg.kg.","C.N","CAILLOUX...","ARGILE...","LIMONS_FINS...","LIMONS_GROSSIERS...","SABLES_FIN...","SABLES_GROSSIERS...")
range_limits = range(c(df_site[[col_2025]], df_site[[col_2021]]), na.rm = TRUE)

for (var in variables) {
  
  # Étape 1 : Préparer les données
  data_wide = Sol %>%
    select(Site, ID_LAG, Annee, value = all_of(var)) %>%
    pivot_wider(names_from = Annee, values_from = value,
                names_prefix = paste0(gsub("[^A-Za-z0-9]", "_", var), "_")) %>%
    drop_na()  # retirer les lignes incomplètes
  
  # Noms des colonnes après pivot (ex: CARBONE_TOT_mg_kg__2021, etc.)
  colnames_data = colnames(data_wide)
  col_2021 = colnames_data[grepl("2021$", colnames_data)]
  col_2025 = colnames_data[grepl("2025$", colnames_data)]
  
  # Vérifier que les deux colonnes existent avant de continuer
  if (length(col_2021) == 1 && length(col_2025) == 1) {
    
    # Liste des sites
    sites = unique(data_wide$Site)
    
    # Boucle sur chaque site
    for (s in sites) {
      
      df_site = data_wide %>% filter(Site == s)
      
      # Construire le graphique dynamiquement
      p = ggplot(df_site, aes_string(x = col_2025, y = col_2021, color = "ID_LAG")) +
        geom_point(size = 3) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
        coord_fixed(ratio = 1, expand = TRUE) + 
        labs(
          title = paste0("Évolution de ", var, " - Site : ", s),
          x = paste0(var, " en 2025"),
          y = paste0(var, " en 2021"),
          color = "Lagune (ID_LAG)"
        ) +
        theme_minimal()
      
      print(p)  # Affiche le graphique dans la console
      
      
    }
  }
}

####NUAGE DE POINTS ####

Compo = colnames(Sol)[!(colnames(Sol) %in% c("Annee", "Site", "ID_LAG"))]

for (comp in Compo) {
  df_wide = Sol %>%
    select(Annee, Site, ID_LAG, value = all_of(comp)) %>%
    filter(Annee %in% c(2021, 2025)) %>%
    pivot_wider(
      names_from = Annee,
      values_from = value,
      names_prefix = paste0(comp, "_")
    ) %>%
    drop_na()  # enlève les lignes incomplètes
  col_2021 = paste0(comp, "_2021")
  col_2025 = paste0(comp, "_2025")
  
  if (all(c(col_2021, col_2025) %in% colnames(df_wide))) {
    
    p = ggplot(df_wide, aes_string(x = col_2025, y = col_2021, color = "Site")) +
      geom_jitter(size = 3,width = 0.2, height = 0.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 100), expand = TRUE) +
      labs(
        title = paste("Niveau de", comp),
        x = "2025",
        y = "2020",
        color = "Site"
      ) +
      theme_minimal()+
      theme(
        panel.grid.major = element_line(color = "grey60", size = 0.6),  # Quadrillage principal renforcé
        panel.grid.minor = element_line(color = "grey80", size = 0.3),  # Quadrillage secondaire visible
        panel.border = element_rect(color = "black", fill = NA, size = 0.8),  # Contour du graphique
        axis.line = element_line(color = "black", size = 0.8)  # Axes visibles
      )
    
    print(p) 
  }
}


#### ANALYSE MULTI ####

Data_multi = Sol %>% select(-PARTICULES_FINES)
sites_a_exclure = c("BPA_PIS", "BAG_GRA", "CAN_NAZ", "HYE_VIE", "THA_SET")
Data_multi = Data_multi %>%
  filter(!(Site %in% sites_a_exclure))
Data_multi$Annee = as.factor(Data_multi$Annee)
Data_multi$Site = as.factor(Data_multi$Site)
Data_multi$ID_LAG = as.factor(Data_multi$ID_LAG)
df_composes = Data_multi %>% select(4:13)
df_meta = Data_multi %>% select(Annee, Site, ID_LAG)
df_composes_clean = df_composes %>%
  mutate(across(everything(), as.numeric)) %>%
  select(where(~ sd(., na.rm = TRUE) > 0)) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  filter(rowSums(., na.rm = TRUE) > 0)

nmds = metaMDS(df_composes_clean, distance = "bray", k = 2, trymax = 100)
scores_df = as.data.frame(scores(nmds, display = "sites"))
scores_df = bind_cols(df_meta, scores_df)

lagunes_2021_2025 = scores_df %>%
  filter(Annee %in% c(2021, 2025)) %>%
  group_by(ID_LAG) %>%
  summarise(annees_presente = list(unique(Annee))) %>%
  filter(all(c(2021, 2025) %in% unlist(annees_presente))) %>%
  pull(ID_LAG)

scores_df_filtre = scores_df %>%
  filter(ID_LAG %in% lagunes_2021_2025)

compo_scores = as.data.frame(scores(nmds, display = "species"))
compo_scores$Compo = rownames(compo_scores)
colnames(compo_scores)[1:2] = c("NMDS1", "NMDS2")

# Graph 1 : Ellipses par site
ggplot(scores_df, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(size = 1.5) +
  stat_ellipse(type = "t", level = 0.68, linewidth = 1) +
  geom_segment(data = compo_scores,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", inherit.aes = FALSE) +
  geom_text(data = compo_scores,
            aes(x = NMDS1, y = NMDS2, label = Compo),
            color = "black", vjust = -0.5, size = 3, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
  
  theme_minimal() +
  labs(
    title = "NMDS des compositions du sol par Site",
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

# Graph 2 : Ellipses par années
scores_df_filtre_ellipses = scores_df_filtre %>%
  group_by(Site) %>%
  filter(n() >= 3) %>%
  ungroup()

ggplot(scores_df_filtre, aes(x = NMDS1, y = NMDS2, color = Annee)) +
  geom_point(size = 2) +
  stat_ellipse(type = "t") +
  geom_segment(data = compo_scores,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", inherit.aes = FALSE) +
  geom_text(data = compo_scores,
            aes(x = NMDS1, y = NMDS2, label = Compo),
            color = "black", vjust = -0.5, inherit.aes = FALSE) +
  theme_minimal() +
  labs(title = "NMDS + Ellipses par Année",
       x = "NMDS1",
       y = "NMDS2") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

# Graph 3 : Ellipses par Site/Année + flèches de trajectoire
scores_df_filtre = scores_df_filtre %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_"))

# Supprimer les groupes avec < 3 points
scores_df_filtre_ellipse <- scores_df_filtre %>%
  group_by(Site_Annee) %>%
  filter(n() >= 3) %>%
  ungroup()

# Calcul des centroïdes
centroids <- scores_df_filtre_ellipse %>%
  group_by(Site, Annee, Site_Annee) %>%
  summarise(
    NMDS1 = mean(NMDS1, na.rm = TRUE),
    NMDS2 = mean(NMDS2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Site, Annee)

# Création des flèches de trajectoire
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
  labs(title = "Trajectoire temporelle des Sites dans l’espace NMDS")


# Graph 4 : Trajectoires par lagunes (coloration par année)
ggplot(scores_df_filtre, aes(x = NMDS1, y = NMDS2)) +
  geom_path(aes(group = ID_LAG),
            arrow = arrow(type = "closed", length = unit(0.15, "inches")),
            color = "grey50", linewidth = 0.8) +
  geom_point(aes(color = Annee), size = 3) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12))+ 
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8, linetype = "dashed")










