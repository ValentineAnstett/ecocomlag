#Explo données Flore

#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")
Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(ID_LAG %in% c("G_07", "G_06","D_04","D_05")))
Macro_Ptscontacts = Macro_Ptscontacts %>% dplyr::select(-c(algues))
Macro_Ptscontacts = Macro_Ptscontacts[, colSums(Macro_Ptscontacts != 0, na.rm = TRUE) > 0]

Macro_Ptscontacts_sanstot = Macro_Ptscontacts [, -13]


###  Boxplots 2020 vs 2025 ----
couleurs_Year = c("2020" = "#F8766D",  # rouge clair
                    "2025" = "#00BFC4") 
#### Boxplot sur le total de chaque lagune ----
ggplot(Macro_Ptscontacts, aes(x = factor(Year), y = TOT, fill = factor(Year))) +
  geom_boxplot(alpha = 0.5, color = "black") +   # boxplot avec contour noir
  geom_line(aes(group = ID_LAG), color = "grey50", alpha = 0.7, show.legend = FALSE) +
  geom_point(color = "black", size = 2, show.legend = FALSE) +
  scale_fill_manual(values = couleurs_Year) +  # appliquer les couleurs
  labs(x = "Year", y =  "Total", fill = "Year") +
  theme_minimal() +
  theme(legend.position = "none")

#Boxplot sur la moyenne des indices par lagunes 
Macro_Ptscontacts_long = Macro_Ptscontacts %>%
  filter(Year %in% c(2020, 2025)) %>%
  pivot_longer(
    cols = -c(Year, Site, ID_LAG),
    names_to = "Espece",
    values_to = "indice"
  )

df_agg = Macro_Ptscontacts_long %>%
  group_by(Year, ID_LAG) %>%
  summarise(moyenne_indice = mean(indice, na.rm = TRUE)) %>%
  ungroup()

ggplot(df_agg, aes(x = factor(Year), y = moyenne_indice, fill = factor(Year))) +
  geom_boxplot(alpha = 0.5, color = "black") +   # boxplot avec contour noir
  geom_line(aes(group = ID_LAG), color = "grey50", alpha = 0.7, show.legend = FALSE) +
  geom_point(color = "black", size = 2, show.legend = FALSE) +
  scale_fill_manual(values = couleurs_Year) +  # appliquer les couleurs
  labs(x = "Année", y = "Indice moyen de présence par lagune", fill = "Année") +
  theme_minimal() +
  theme(legend.position = "none")

#### Richesse spécifique par sites ----

Macro_Ptscontacts_sanstot = Macro_Ptscontacts_sanstot %>%
  rowwise() %>%
  mutate(n_sp = sum(c_across(-c(Year, Site, ID_LAG)) > 0, na.rm = TRUE)) %>%
  ungroup()

df_n_sp = Macro_Ptscontacts_sanstot %>%
  dplyr::select(Year,Site, ID_LAG, n_sp) %>%
  filter(Year %in% c(2020, 2025)) %>%
  pivot_wider(
    names_from = Year,
    values_from = n_sp,
    names_prefix = "n_sp_"
  ) %>%
  drop_na()

ggplot(df_n_sp, aes(x = n_sp_2025, y = n_sp_2020, color = Site)) +
  geom_jitter(size = 3,width = 0.2, height = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  coord_fixed(ratio = 1, xlim = c(0, 10), ylim = c(0, 10), expand = TRUE) +
  labs(
    title = "Richesse spécifique par site",
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

#Avec Barycentre et IC 

summary_df = df_n_sp %>%
  group_by(Site) %>%
  summarise(
    mean_2025 = mean(n_sp_2025),
    mean_2020 = mean(n_sp_2020),
    se_2025 = sd(n_sp_2025) / sqrt(n()),
    se_2020 = sd(n_sp_2020) / sqrt(n()),
    n = n()
  ) %>%
  mutate(
    # IC à 95% (approximation normale)
    ic_lower_2025 = mean_2025 - 1.96 * se_2025,
    ic_upper_2025 = mean_2025 + 1.96 * se_2025,
    ic_lower_2020 = mean_2020 - 1.96 * se_2020,
    ic_upper_2020 = mean_2020 + 1.96 * se_2020
  )

# Graphique avec barycentre et IC

ggplot(df_n_sp, aes(x = n_sp_2020, y = n_sp_2025, color = Site)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "red", linewidth = 1) +
  geom_errorbarh(data = summary_df,
                 aes(y = mean_2025, xmin = ic_lower_2020, xmax = ic_upper_2020, color = Site),
                 height = 0.3, linewidth = 1, inherit.aes = FALSE) +
  geom_errorbar(data = summary_df,
                aes(x = mean_2020, ymin = ic_lower_2025, ymax = ic_upper_2025, color = Site),
                width = 0.3, linewidth = 1, inherit.aes = FALSE) +
  geom_point(data = summary_df,
             aes(x = mean_2020, y = mean_2025, color = Site),
             shape = 21, fill = "white", size = 8, stroke = 2, inherit.aes = FALSE) +
  geom_text(data = summary_df,
            aes(x = mean_2020, y = mean_2025, label = Site),
            color = "black", fontface = "bold", size = 5, inherit.aes = FALSE) +
  scale_x_continuous(limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
  
  labs(
    x = "2020",
    y = "2025"
  ) +
  guides(color = "none") +
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey70", linewidth = 0.6),
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(size = 22, face = "bold"),
    axis.text = element_text(size = 18),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15)
  )

##ACP Simplifié ####

#Transformation des datas avec Hellinger
data_acp = Macro_Ptscontacts_sanstot[,-13]
data_acp_clean = data_acp %>% mutate(across(everything(), ~replace_na(.x, 0)))
meta = data_acp_clean %>% dplyr::select(Year, Site, ID_LAG)
data_num = data_acp_clean %>% dplyr::select(-Year, -Site, -ID_LAG)
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

# Pourcentages de variance expliquée
eig_vals = pca_res$eig
dim1_var = round(eig_vals[1, 2], 1)  # % de variance pour Dim 1
dim2_var = round(eig_vals[2, 2], 1)  # % de variance pour Dim 2

#### Graph avec polygone avec les lagunes outliers sorties : 
get_hull = function(df) df[chull(df$Dim.1, df$Dim.2), ]
hulls = coord_ind %>% group_by(Site) %>% group_modify(~get_hull(.x))

ggplot(coord_ind %>% filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05")), aes(x = Dim.1, y = Dim.2, color = Site)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_polygon(
    data = (coord_ind %>% filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05")) %>% group_by(Site) %>% group_modify(~get_hull(.x))),
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
    x = paste0("Dim 1 (", dim1_var, "%)"),
    y = paste0("Dim 2 (", dim2_var, "%)")
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

pca_res$var$contrib

##Graph avec polygone par année 

ggplot(coord_ind %>% filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05","K_03", "K_10", "B_06", "K_08", "B_09", "K_10", "B_01", "K_09", "B_10", "B_08")), 
       aes(x = Dim.1, y = Dim.2, color = as.factor(Year))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_polygon(
    data = (coord_ind %>% 
              filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05")) %>% 
              group_by(Year) %>% group_modify(~get_hull(.x))),
    aes(x = Dim.1, y = Dim.2, fill = as.factor(Year), group = Year),
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
    color = "Year", fill = "Year"
  ) +
  scale_color_manual(values = couleurs_Year) +
  scale_fill_manual(values = couleurs_Year) +
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


#FacetPlot 
# --- Données initiales : filtrage des années + passage en long format ---
Data_facet <- Macro_Ptscontacts %>%
  filter(Year %in% c(2020, 2025)) %>%
  pivot_longer(
    cols = -c(Year, Site, ID_LAG),
    names_to = "Espece",
    values_to = "Score"
  )

# --- Création des bins de score, à partir de 0.1 ---
Data_facet <- Data_facet %>%
  filter(!is.na(Score)) %>%
  mutate(Score_bin = cut(
    Score,
    breaks = seq(0.1, 1, by = 0.1),
    include.lowest = TRUE,
    right = FALSE
  )) %>%
  filter(!is.na(Score_bin))
# --- Séparation TOT vs espèces ---
df_especes_facet <- Data_facet %>% filter(Espece != "TOT")
df_tot_facet     <- Data_facet %>% filter(Espece == "TOT")

# --- Liste des espèces ---
liste_especes_facet <- unique(df_especes_facet$Espece)

# --- Réplication des scores TOT dans chaque facette d'espèce ---
df_tot_replique <- expand_grid(
  Espece = liste_especes_facet,
  Score_bin = levels(Data_facet$Score_bin)
) %>%
  left_join(
    df_tot_facet %>%
      group_by(Score_bin) %>%
      summarise(n = n(), .groups = "drop"),
    by = "Score_bin"
  ) %>%
  filter(n > 0)

# --- Comptage des occurrences pour les espèces ---
counts_especes_facet <- df_especes_facet %>%
  group_by(Espece, Score_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 0)

# --- Graphique final ---
ggplot() +
  # TOT en fond gris
  geom_col(data = df_tot_replique, aes(x = Score_bin, y = n), fill = "grey80") +
  
  # Espèces en vert
  geom_col(data = counts_especes_facet, aes(x = Score_bin, y = n), fill = "forestgreen") +
  scale_y_log10()+
  
  # Facettes par espèce
  facet_wrap(~ Espece, scales = "free_y") +
  labs(
    title = "Distribution des scores de recouvrement",
    x = "Score de recouvrement (binné)",
    y = "Nombre d'ID_LAG"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Identifier les outliers 
coord_ind$dist_origin <- sqrt(coord_ind$Dim.1^2 + coord_ind$Dim.2^2)
outliers_top10 <- coord_ind %>%
  arrange(desc(dist_origin)) %>%
  slice(1:10)
