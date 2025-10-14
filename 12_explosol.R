#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Sol/Data/Processed")
Sol = read.csv("Sol_21_25.csv", header = TRUE, sep = ",", dec=",")

Sol= Sol %>%
  mutate(across(4:14, as.numeric))

Chimique = Sol [, -c(4, 8:14)]


Type_sol = Sol [, -c(5:7)]

####HISTOGRAMMES ----

#Global : Composition du sol 2021-2025  
Sol_sanslag = Type_sol [,-3]
Data = Sol_sanslag
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

#Global : Chimie du sol 2021-2025

Data1 = Chimique [, -3]
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


#Global : Evolution Azote dans le sol 2019/2025

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

#POURCENTAGES 
Sol_pourcentage = Sol[, -c(5,6,7)]

data_long_pourcentage = Sol_pourcentage %>%
  pivot_longer(cols = -c(Annee, Site, ID_LAG), names_to = "Compo_sol", values_to = "Pourcentage")

#Composition par site et par années
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
#Graphs par site (les deux années ensemble)
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

#Compo chimique par site et par années 
plot_variable_sol_par_site <- function(data, col_variable, fill_label, fill_color, y_label, titre_general) {

  df_var <- data %>%
    select(Annee, Site, ID_LAG, all_of(col_variable))
  data_long <- df_var %>%
    pivot_longer(cols = -c(Annee, Site, ID_LAG), 
                 names_to = fill_label, 
                 values_to = "Valeur")
  sites <- unique(data_long$Site)
  for (site in sites) {
    data_site <- data_long %>% filter(Site == site)
    
    p <- ggplot(data_site, aes(x = ID_LAG, y = Valeur, fill = .data[[fill_label]])) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(~ Annee) +
      labs(
        title = paste(titre_general, " - Site :", site),
        x = "LAGUNE",
        y = y_label
      ) +
      scale_fill_manual(values = setNames(fill_color, unique(data_site[[fill_label]]))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
  }
}

plot_variable_sol_par_site(
  data = Sol,
  col_variable = "C.N",  # nom exact de la colonne
  fill_label = "CarboneAzote",
  fill_color = "#a6761d",
  y_label = "C/N",
  titre_general = "Rapport C/N dans le sol"
)

plot_variable_sol_par_site(
  data = Sol,
  col_variable = "P2O5_TOT...",  # adapte au nom exact de ta colonne
  fill_label = "Engrais_phosphates",
  fill_color = "#1b9e77",
  y_label = "P2O5 (%)",
  titre_general = "Pourcentage de P2O5 dans le sol"
)

plot_variable_sol_par_site(
  data = Sol,
  col_variable = "AZOTE_TOT.mg.kg.",  # nom exact
  fill_label = "AzoteTotal",
  fill_color = "#7570b3",
  y_label = "Azote total (mg/kg)",
  titre_general = "Quantité d'azote dans le sol"
)



####NUAGE DE POINT = Détails site par site ----

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

####NUAGE DE POINTS = tous les sites regroupés ----

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
#PCA : Creation d'une fonction pour y appliquer les differents dataset 

faire_graphs_pca = function(data, nom_dataset, colonnes_compo, sites_a_exclure = NULL) {
  
  data = data %>%
    filter(!(Site %in% sites_a_exclure)) %>%
    mutate(
      Annee = as.factor(Annee),
      Site = as.factor(Site),
      ID_LAG = as.factor(ID_LAG)
    )
  df_composes = data %>%
    select(all_of(colonnes_compo)) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0)) %>%
    filter(rowSums(is.na(.)) < ncol(.))
  
  df_meta = data %>% select(Annee, Site, ID_LAG)
  
  # 3. PCA
  pca = prcomp(df_composes, center = TRUE, scale. = TRUE)
  scores_df = as.data.frame(pca$x[, 1:2])
  colnames(scores_df) = c("PCA1", "PCA2")
  scores_df = bind_cols(df_meta, scores_df)
  
  compo_scores = as.data.frame(pca$rotation[, 1:2])
  compo_scores$Compo = rownames(compo_scores)
  colnames(compo_scores)[1:2] = c("PCA1", "PCA2")
  
  # 4. Filtrage pour trajectoires 2021–2025
  lagunes_2021_2025 = scores_df %>%
    filter(Annee %in% c(2021, 2025)) %>%
    group_by(ID_LAG) %>%
    summarise(annees = list(unique(Annee)), .groups = "drop") %>%
    filter(all(c("2021", "2025") %in% unlist(annees))) %>%
    pull(ID_LAG)
  
  scores_df_filtre = scores_df %>% filter(ID_LAG %in% lagunes_2021_2025)
  scores_df_filtre = scores_df_filtre %>%
    mutate(Site_Annee = paste(Site, Annee, sep = "_"))
  
  # 5. Ellipses
  ellipse_data = scores_df_filtre %>%
    group_by(Site_Annee) %>%
    filter(n() >= 3) %>%
    ungroup()
  
  centroids = ellipse_data %>%
    group_by(Site, Annee, Site_Annee) %>%
    summarise(PCA1 = mean(PCA1), PCA2 = mean(PCA2), .groups = "drop") %>%
    arrange(Site, Annee)
  
  arrows_df = centroids %>%
    group_by(Site) %>%
    arrange(Annee) %>%
    mutate(
      PCA1_end = lead(PCA1),
      PCA2_end = lead(PCA2)
    ) %>%
    filter(!is.na(PCA1_end)) %>%
    ungroup()
  
  # --- Graphs ---
  
  ## Graph 1 : Ellipses par site
  g1 = ggplot(scores_df, aes(x = PCA1, y = PCA2, color = Site)) +
    geom_point(size = 1.5) +
    stat_ellipse(type = "t", level = 0.68, linewidth = 1) +
    geom_segment(data = compo_scores,
                 aes(x = 0, y = 0, xend = PCA1, yend = PCA2),
                 arrow = arrow(length = unit(0.3, "cm")),
                 color = "black", inherit.aes = FALSE) +
    geom_text(data = compo_scores,
              aes(x = PCA1, y = PCA2, label = Compo),
              color = "black", vjust = -0.5, size = 3, inherit.aes = FALSE) +
    theme_minimal() +
    labs(title = paste("PCA -", nom_dataset, "- Ellipses par Site"))
  
  ## Graph 2 : Ellipses par année
  g2 = ggplot(scores_df_filtre, aes(x = PCA1, y = PCA2, color = Annee)) +
    geom_point(size = 2) +
    stat_ellipse(type = "t") +
    geom_segment(data = compo_scores,
                 aes(x = 0, y = 0, xend = PCA1, yend = PCA2),
                 arrow = arrow(length = unit(0.2, "cm")),
                 color = "black", inherit.aes = FALSE) +
    geom_text(data = compo_scores,
              aes(x = PCA1, y = PCA2, label = Compo),
              color = "black", vjust = -0.5, inherit.aes = FALSE) +
    theme_minimal() +
    labs(title = paste("PCA -", nom_dataset, "- Ellipses par Année"))
  
  ## Graph 3 : Trajectoire par site
  g3 = ggplot(ellipse_data, aes(x = PCA1, y = PCA2, color = Site)) +
    geom_point(aes(shape = Annee), size = 2) +
    stat_ellipse(aes(group = Site_Annee), type = "t") +
    geom_segment(data = arrows_df,
                 aes(x = PCA1, y = PCA2, xend = PCA1_end, yend = PCA2_end),
                 arrow = arrow(type = "closed", length = unit(0.15, "inches")),
                 linewidth = 1) +
    theme_minimal() +
    labs(title = paste("PCA -", nom_dataset, "- Trajectoires temporelles par Site"))
  
  ## Graph 4 : Trajectoire par lagune (coloré par année)
  g4 = ggplot(scores_df_filtre, aes(x = PCA1, y = PCA2)) +
    geom_path(aes(group = ID_LAG),
              arrow = arrow(type = "closed", length = unit(0.15, "inches")),
              color = "grey50", linewidth = 0.8) +
    geom_point(aes(color = Annee), size = 3) +
    theme_minimal() +
    labs(title = paste("PCA -", nom_dataset, "- Trajectoire par Lagune (Années)"))
  
  return(list(graph1 = g1, graph2 = g2, graph3 = g3, graph4 = g4))
}

#Lancer sur mes jeux de données 

vars_chimique = c("P2O5_TOT...","AZOTE_TOT.mg.kg.","C.N")  
vars_sol = c("MO_TOT...","CAILLOUX...","ARGILE...","LIMONS_FINS...","LIMONS_GROSSIERS...","SABLES_FIN...","SABLES_GROSSIERS...")
sites_exclus = c("BAG_GRA", "HYERES", "VILLEROY")

graphs_chimique = faire_graphs_pca(
  data = Chimique,
  nom_dataset = "Chimique",
  colonnes_compo = vars_chimique,
  sites_a_exclure = sites_exclus
)

graphs_sol = faire_graphs_pca(
  data = Type_sol,
  nom_dataset = "Sol",
  colonnes_compo = vars_sol,
  sites_a_exclure = sites_exclus
)

# Chimique
graphs_chimique$graph1  # PCA par site
graphs_chimique$graph2  # PCA par année
graphs_chimique$graph3  # Trajectoire temporelle par site
graphs_chimique$graph4  # Trajectoire par lagune

# Sol
graphs_sol$graph1
graphs_sol$graph2
graphs_sol$graph3
graphs_sol$graph4



####PERMANOVA####

faire_permanova = function(data, colonnes_compo, facteurs, sites_a_exclure = NULL) {
  # Exclure sites si besoin
  data = data %>%
    filter(!(Site %in% sites_a_exclure)) %>%
    mutate(across(all_of(facteurs), as.factor))  # S'assurer que les facteurs sont bien en facteur
  
  # Sélection des variables de composition
  df_compo = data %>%
    select(all_of(colonnes_compo)) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0)) %>%
    filter(rowSums(is.na(.)) < ncol(.))
  
  # Métadonnées pour le design
  df_meta = data %>% 
    filter(rowSums(is.na(select(., all_of(colonnes_compo)))) < ncol(.)) %>%
    select(all_of(facteurs))
  
  # Calcul des distances (par exemple, euclidiennes ou Bray-Curtis selon le cas)
  distance_matrix = vegdist(df_compo, method = "euclidean")  # ou "bray"
  
  # PERMANOVA avec adonis2
  formule = as.formula(paste("distance_matrix ~", paste(facteurs, collapse = " + ")))
  
  permanova_result = adonis2(formule, data = df_meta, permutations = 999)
  
  return(permanova_result)
}


#Chimique 
resultats_permanova_chimique = faire_permanova(
  data = Chimique,
  colonnes_compo = vars_chimique,
  facteurs = c("Annee", "Site"),
  sites_a_exclure = sites_exclus
)

print(resultats_permanova_chimique)

#Structure du sol 
resultats_permanova_sol = faire_permanova(
  data = Sol,
  colonnes_compo = vars_sol,
  facteurs = c("Annee", "Site"),
  sites_a_exclure = sites_exclus
)

print(resultats_permanova_sol)
