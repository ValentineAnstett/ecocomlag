#Explo données Flore

#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")


###  Boxplots 2020 vs 2025 ----
couleurs_annee = c("2020" = "#F8766D",  # rouge clair
                    "2025" = "#00BFC4") 
#### Boxplot sur le total de chaque lagune ----
ggplot(Macro_Ptscontacts, aes(x = factor(Annee), y = TOT, fill = factor(Annee))) +
  geom_boxplot(alpha = 0.5, color = "black") +   # boxplot avec contour noir
  geom_line(aes(group = ID_LAG), color = "grey50", alpha = 0.7, show.legend = FALSE) +
  geom_point(color = "black", size = 2, show.legend = FALSE) +
  scale_fill_manual(values = couleurs_annee) +  # appliquer les couleurs
  labs(x = "Année", y =  "Valeurs TOT", fill = "Année") +
  theme_minimal() +
  theme(legend.position = "none")

#Boxplot sur la moyenne des indices par lagunes 
Macro_Ptscontacts_long = Macro_Ptscontacts %>%
  filter(Annee %in% c(2020, 2025)) %>%
  pivot_longer(
    cols = -c(Annee, Site, ID_LAG),
    names_to = "Espece",
    values_to = "indice"
  )

df_agg = Macro_Ptscontacts_long %>%
  group_by(Annee, ID_LAG) %>%
  summarise(moyenne_indice = mean(indice, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_agg, aes(x = factor(Annee), y = moyenne_indice, fill = factor(Annee))) +
  geom_boxplot(alpha = 0.5, color = "black") +   # boxplot avec contour noir
  geom_line(aes(group = ID_LAG), color = "grey50", alpha = 0.7, show.legend = FALSE) +
  geom_point(color = "black", size = 2, show.legend = FALSE) +
  scale_fill_manual(values = couleurs_annee) +  # appliquer les couleurs
  labs(x = "Année", y = "Moyenne indice par lagune", fill = "Année") +
  theme_minimal() +
  theme(legend.position = "none")

#### Nombre d'espèces par sites ----

Macro_Ptscontacts = Macro_Ptscontacts %>%
  rowwise() %>%
  mutate(n_sp = sum(c_across(-c(Annee, Site, ID_LAG)) > 0, na.rm = TRUE)) %>%
  ungroup()

df_n_sp = Macro_Ptscontacts %>%
  select(Annee,Site, ID_LAG, n_sp) %>%
  filter(Annee %in% c(2020, 2025)) %>%
  pivot_wider(
    names_from = Annee,
    values_from = n_sp,
    names_prefix = "n_sp_"
  ) %>%
  drop_na()

ggplot(df_n_sp, aes(x = n_sp_2025, y = n_sp_2020, color = Site)) +
  geom_jitter(size = 3,width = 0.2, height = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10), expand = TRUE) +
  labs(
    title = "Nombre d'espèces par site",
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

##AFC ####

#Transformation des datas avec Hellinger
data_afc = Macro_Ptscontacts[, -c(19, 22)]
data_afc_clean = data_afc %>% mutate(across(everything(), ~replace_na(.x, 0)))
meta = data_afc_clean %>% select(Annee, Site, ID_LAG)
data_num = data_afc_clean %>% select(-Annee, -Site, -ID_LAG)
data_hellinger = decostand(data_num, method = "hellinger")

#Faire l'ACP sur données transformées
pca_res = PCA(data_hellinger, graph = FALSE)

#Extraction des coordonnées meta
coord_ind = as.data.frame(pca_res$ind$coord[, 1:2])
colnames(coord_ind) = c("Dim.1", "Dim.2")
coord_ind = cbind(meta, coord_ind)

# Extraction coordonnées variables (espèces)
coord_var = as.data.frame(pca_res$var$coord[, 1:2])
colnames(coord_var) = c("Dim.1", "Dim.2")
coord_var$Espece = rownames(coord_var)

#### Graph avec polygone #### 

get_hull = function(df) df[chull(df$Dim.1, df$Dim.2), ]
hulls = coord_ind %>% group_by(Site) %>% group_modify(~get_hull(.x))

ggplot(coord_ind, aes(x = Dim.1, y = Dim.2, color = Site)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_polygon(data = hulls, aes(x = Dim.1, y = Dim.2, fill = Site), alpha = 0.15, color = NA, inherit.aes = FALSE) +
  geom_segment(data = coord_var, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
               arrow = arrow(length = unit(0.2, "cm")), color = "black", inherit.aes = FALSE) +
  geom_text_repel(data = coord_var, aes(x = Dim.1, y = Dim.2, label = Espece),
                  color = "black", size = 5, inherit.aes = FALSE, max.overlaps = 20) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  labs(title = "ACP sur données transformées Hellinger\navec lagunes colorées par site et flèches espèces",
       x = "Dimension 1",
       y = "Dimension 2") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_line(color = "black"),
    plot.title = element_text(size = 40, face = "bold"),       
    axis.title = element_text(size = 30),                     
    axis.text = element_text(size = 24),                      
    legend.title = element_text(size = 28),                    
    legend.text = element_text(size = 24),                     
    strip.text = element_text(size = 28), 
  )


#Graph polygone mais par LAGUNE 

group_circles = coord_ind %>%
  group_by(ID_LAG) %>%
  summarise(
    x0 = mean(Dim.1),
    y0 = mean(Dim.2),
    r = max(sqrt((Dim.1 - mean(Dim.1))^2 + (Dim.2 - mean(Dim.2))^2)) + 0.1
  )
ggplot(coord_ind, aes(x = Dim.1, y = Dim.2)) +
  geom_point(aes(color = ID_LAG), size = 3, alpha = 0.8) +
  
  geom_circle(data = group_circles, aes(x0 = x0, y0 = y0, r = r, fill = ID_LAG),
              alpha = 0.15, color = NA, inherit.aes = FALSE) +
  coord_fixed() +
  theme_minimal()

#Sortir les outliers et refaire le graph par site : PAL_VIL_06 et 07 

ggplot(coord_ind %>% filter(!ID_LAG %in% c("PAL_VIL_07", "PAL_VIL_06","ORB_MAI_04","ORB_MAI_05")), aes(x = Dim.1, y = Dim.2, color = Site)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_polygon(
    data = (coord_ind %>% filter(!ID_LAG %in% c("PAL_VIL_07", "PAL_VIL_06","ORB_MAI_04","ORB_MAI_05")) %>% group_by(Site) %>% group_modify(~get_hull(.x))),
    aes(x = Dim.1, y = Dim.2, fill = Site),
    alpha = 0.15, color = NA, inherit.aes = FALSE
  ) +
  geom_segment(
    data = coord_var, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
    arrow = arrow(length = unit(0.2, "cm")), color = "black", inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = coord_var, aes(x = Dim.1, y = Dim.2, label = Espece),
    color = "black", size = 6, inherit.aes = FALSE, max.overlaps = 20
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  labs(
    title = "ACP sur données transformées Hellinger\navec lagunes colorées par site et flèches espèces",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_line(color = "black"),
    plot.title = element_text(size = 40, face = "bold"),       
    axis.title = element_text(size = 30),                     
    axis.text = element_text(size = 24),                      
    legend.title = element_text(size = 28),                    
    legend.text = element_text(size = 24),                     
    strip.text = element_text(size = 28),                     
  )


##Graph avec polygone par année 

ggplot(coord_ind %>% filter(!ID_LAG %in% c("PAL_VIL_07", "PAL_VIL_06","ORB_MAI_04","ORB_MAI_05")), 
       aes(x = Dim.1, y = Dim.2, color = as.factor(Annee))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_polygon(
    data = (coord_ind %>% 
              filter(!ID_LAG %in% c("PAL_VIL_07", "PAL_VIL_06","ORB_MAI_04","ORB_MAI_05")) %>% 
              group_by(Annee) %>% group_modify(~get_hull(.x))),
    aes(x = Dim.1, y = Dim.2, fill = as.factor(Annee), group = Annee),
    alpha = 0.15, color = NA, inherit.aes = FALSE
  ) +
  geom_segment(
    data = coord_var, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
    arrow = arrow(length = unit(0.2, "cm")), color = "black", inherit.aes = FALSE
  ) +
  geom_text_repel(
    data = coord_var, aes(x = Dim.1, y = Dim.2, label = Espece),
    color = "black", size = 6, inherit.aes = FALSE, max.overlaps = 20
  ) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  labs(
    title = "ACP sur données transformées Hellinger\navec lagunes colorées par année et flèches espèces",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Année", fill = "Année"
  ) +
  scale_color_manual(values = couleurs_annee) +
  scale_fill_manual(values = couleurs_annee) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),
    axis.line = element_line(color = "black"),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title = element_text(size = 30),
    axis.text = element_text(size = 24),
    legend.title = element_text(size = 28),
    legend.text = element_text(size = 24),
    strip.text = element_text(size = 28),
  )














































#### EXPLO INTER-SITES ####
#Existe-il une difference entre les sites ? 

Cov_tot_moy_mat_inter <- Cov_tot_moy %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_")) %>%  # Créer Site_Annee
  select(Site_Annee, everything(), -c(Site, Annee, LAGUNE))  # Réorganiser les colonnes

df_aggregated_inter <- Cov_tot_moy_mat_inter %>%
  group_by(Site_Annee) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = "drop")

#ggplots ----
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

#Nuages de points ----

#### EXPLO INTRA-SITES #### 
#Existe t-il une différence entre les lagunes d'un même site ? 

data_long_intra = Cov_tot_moy %>%
  pivot_longer(cols = -c(Annee, Site, LAGUNE), names_to = "Espece", values_to = "Recouvrement")

data_long_intra$LAGUNE = gsub("^\\d{4}_", "", data_long_intra$LAGUNE)

#ggplots ----

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

#Nuage de points---- 
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
        coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 100), expand = TRUE) +
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

#ggplots ----
ggplot(data_long_stations, aes(x = LAGUNE, y = Recouvrement, fill = Espèce)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Recouvrement des espèces par Lagune",
    x = "Lagune",
    y = "Taux de recouvrement (%)",
    fill = "Espèce"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Nuage de points ---


#### NUAGE DE POINTS : GLOBAL #### 
#n sp / recouvrement total /  recouvrement espece par espece ---- 

Cov_tot_moy = Cov_tot_moy %>%
  mutate(across(4:21, as.numeric))

Cov_tot_moy <- Cov_tot_moy %>%
  mutate(LAGUNE = str_remove(LAGUNE, "^\\d{4}_"))

Cov_tot_moy <- Cov_tot_moy %>%
  rowwise() %>%
  mutate(n_sp = sum(c_across(-c(Annee, Site, LAGUNE)) > 0, na.rm = TRUE)) %>%
  ungroup()

especes <- colnames(Cov_tot_moy)[!(colnames(Cov_tot_moy) %in% c("Annee", "Site", "LAGUNE"))]

for (esp in especes) {
    df_wide <- Cov_tot_moy %>%
    select(Annee, Site, LAGUNE, value = all_of(esp)) %>%
    filter(Annee %in% c(2020, 2025)) %>%
    pivot_wider(
      names_from = Annee,
      values_from = value,
      names_prefix = paste0(esp, "_")
    ) %>%
    drop_na()  # enlève les lignes incomplètes
    col_2020 <- paste0(esp, "_2020")
  col_2025 <- paste0(esp, "_2025")
  
  if (all(c(col_2020, col_2025) %in% colnames(df_wide))) {
    
    p <- ggplot(df_wide, aes_string(x = col_2025, y = col_2020, color = "Site")) +
      geom_jitter(size = 3,width = 0.2, height = 0.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
      coord_fixed(ratio = 1, xlim = c(0, 100), ylim = c(0, 100), expand = TRUE) +
      labs(
        title = paste("Évolution du recouvrement pour :", esp),
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



#### ANALYSE TRAJECTOIRE ####


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
df_meta = Data_multi %>% select(Annee, Site, LAGUNE)

#nmds + triche : Ajouter une minuscule valeur pour les valeurs à zéro ---- 

colonnes_non_vides_triche = colSums(df_species, na.rm = TRUE) > 0
df_species_clean_triche = df_species[, colonnes_non_vides_triche]
lignes_vides_triche = rowSums(df_species_clean_triche) == 0
df_species_clean_triche[lignes_vides_triche, ] <- 1e-6
df_meta_clean_triche = df_meta 

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

species_scores = as.data.frame(scores(nmds_triche, display = "species"))
species_scores$Species <- rownames(species_scores)
colnames(species_scores)[1:2] <- c("NMDS1", "NMDS2")

ggplot(scores_df_triche, aes(x = NMDS1, y = NMDS2, color = Site)) +
  geom_point(size = 1) +
  stat_ellipse(type = "t") +
  geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", inherit.aes = FALSE) +
  geom_text_repel(data = species_scores, aes(x = NMDS1, y = NMDS2, label = Species),
            color = "black", vjust = -0.5, inherit.aes = FALSE, max.overlaps = 15) +
  
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5, linetype = "dotted") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5, linetype = "dotted") +
  
  theme_minimal() +
  labs(title = "NMDS par site / 2020 et 2025 confondus") +
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 0.8),
    panel.grid.major = element_line(color = "grey80", size = 0.5)
  )

#Graph2 : Ellipses par Annees 

centres = scores_df_triche_filtre %>%
  filter(Annee %in% c(2020, 2025)) %>%
  group_by(Annee) %>%
  summarise(
    centre_NMDS1 = mean(NMDS1),
    centre_NMDS2 = mean(NMDS2)
  )

fleche = data.frame(
  x = centres$centre_NMDS1[centres$Annee == 2020],
  y = centres$centre_NMDS2[centres$Annee == 2020],
  xend = centres$centre_NMDS1[centres$Annee == 2025],
  yend = centres$centre_NMDS2[centres$Annee == 2025]
)

ggplot(scores_df_triche_filtre, aes(x = NMDS1, y = NMDS2, color = Annee)) +
  geom_jitter(size = 2) +
  stat_ellipse(type = "norm") +
  geom_segment(data = species_scores, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black", inherit.aes = FALSE) +
  geom_text_repel(data = species_scores, aes(x = NMDS1, y = NMDS2, label = Species),
            color = "black", vjust = -0.5, inherit.aes = FALSE, max.overlaps = 15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
  geom_segment(data = fleche, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red", size = 1, inherit.aes = FALSE) +
  
  theme_minimal() +
  labs(
    title = "NMDS par années",
    x = "Axe NMDS 1",
    y = "Axe NMDS 2",
    color = "Année"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 0.8),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )


#Graph 3 : Ellipses par Site/Annees + fleches 

scores_df_triche_filtre = scores_df_triche_filtre %>%
  mutate(Site_Annee = paste(Site, Annee, sep = "_"))


scores_df_triche_filtre %>% #Identifier les groupes avec < 3 points
  group_by(Site_Annee) %>%
  summarise(nb_points = n()) %>%
  filter(nb_points < 3)

scores_df_triche_filtre_ellipse = scores_df_triche_filtre %>% #Garder uniquement les groupes avec au moins 3 points
  group_by(Site_Annee) %>%
  filter(n() >= 3) %>%
  ungroup()

groupes_sans_variance = scores_df_triche_filtre_ellipse %>% # Identifier les groupes sans variance (points identiques)
  group_by(Site_Annee) %>%
  summarise(
    var_NMDS1 = var(NMDS1),
    var_NMDS2 = var(NMDS2),
    .groups = "drop"
  ) %>%
  filter((is.na(var_NMDS1) | var_NMDS1 < 1e-6) & 
           (is.na(var_NMDS2) | var_NMDS2 < 1e-6))

cercles_centres = scores_df_triche_filtre_ellipse %>%  # Extraire les centres pour les groupes sans variance
  semi_join(groupes_sans_variance, by = "Site_Annee") %>%
  group_by(Site, Annee, Site_Annee) %>%
  summarise(NMDS1 = mean(NMDS1), NMDS2 = mean(NMDS2), .groups = "drop")

scores_df_triche_filtre_ellipse_var <- scores_df_triche_filtre_ellipse %>% # Garder uniquement les groupes AVEC variance pour les ellipses
  anti_join(groupes_sans_variance, by = "Site_Annee")

centroids_triche = scores_df_triche_filtre_ellipse %>% # Calculer les centroïdes pour les flèches
  group_by(Site, Annee, Site_Annee) %>%
  summarise(
    NMDS1 = mean(NMDS1, na.rm = TRUE),
    NMDS2 = mean(NMDS2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Site, Annee)

arrows_df_triche = centroids_triche %>%  # Calculer les flèches entre centroïdes
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
  stat_ellipse(data = scores_df_triche_filtre_ellipse_var,
               aes(group = Site_Annee),
               type = "norm") +  # Petits cercles autour des groupes sans variance
  geom_circle(data = cercles_centres,
              aes(x0 = NMDS1, y0 = NMDS2, r = 0.05, color = Site),
              inherit.aes = FALSE,
              linetype = "dashed",
              linewidth = 0.7) +
  geom_segment(data = arrows_df_triche,
               aes(x = NMDS1, y = NMDS2,
                   xend = NMDS1_end, yend = NMDS2_end, color = Site),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")),
               linewidth = 1) +
  theme_minimal() +
  labs(title = "Trajectoire temporelle des sites dans l’espace NMDS entre 2020 et 2025 (flèches entre ellipses)")

#Graph 4 : Trajectoires par lagunes 
scores_df_triche_piece_eau = scores_df_triche_filtre %>%
  mutate(LAGUNE = str_remove(LAGUNE, "^[0-9]{4}_"))

ggplot(scores_df_triche_piece_eau, aes(x = NMDS1, y = NMDS2)) +
  geom_jitter(size = 2)+
  geom_path(aes(group = LAGUNE), 
            arrow = arrow(type = "closed", length = unit(0.05, "inches")),
            color = "grey50", linewidth = 0.3) +
  geom_point(aes(color = Annee), size = 1) +
  theme_minimal() +
  labs(
    title = "Trajectoires temporelles (coloration par année)",
    x = "NMDS1",
    y = "NMDS2",
    color = "Année"
  )

