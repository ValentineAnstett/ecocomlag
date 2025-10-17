#Explo données Flore

#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")
Macro_Ptscontacts_sanstot = Macro_Ptscontacts [, -19]


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
  labs(x = "Année", y = "Indice moyen de présence par lagune", fill = "Année") +
  theme_minimal() +
  theme(legend.position = "none")

#### Richesse spécifique par sites ----

Macro_Ptscontacts_sanstot = Macro_Ptscontacts_sanstot %>%
  rowwise() %>%
  mutate(n_sp = sum(c_across(-c(Annee, Site, ID_LAG)) > 0, na.rm = TRUE)) %>%
  ungroup()

df_n_sp = Macro_Ptscontacts_sanstot %>%
  dplyr::select(Annee,Site, ID_LAG, n_sp) %>%
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
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  
  geom_point(data = summary_df, aes(x = mean_2020, y = mean_2025, color = Site),
             size = 5, shape = 18, stroke = 1.5, inherit.aes = FALSE) +
  
  geom_text(data = summary_df,
            aes(x = mean_2020, y = mean_2025, label = Site, color = Site),
            inherit.aes = FALSE,
            fontface = "bold",
            size = 5,
            vjust = -0.8,  # au-dessus
            hjust = -0.3) +  # légèrement à droite
  
  geom_errorbarh(data = summary_df,
                 aes(y = mean_2025, xmin = ic_lower_2020, xmax = ic_upper_2020, color = Site),
                 height = 0.3, linewidth = 1, inherit.aes = FALSE) +
  
  geom_errorbar(data = summary_df,
                aes(x = mean_2020, ymin = ic_lower_2025, ymax = ic_upper_2025, color = Site),
                width = 0.3, linewidth = 1, inherit.aes = FALSE) +
  
  # Axes fixes 0 à 5 sans marge, ticks à chaque entier
  scale_x_continuous(limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 5), breaks = 0:5, expand = c(0, 0)) +
  
  labs(
    x = "2020",
    y = "2025"
  ) +
  
  guides(color = "none") +  # Supprime la légende
  
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey60", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey80", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black", linewidth = 0.8),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    plot.margin = margin(t = 10, r = 15, b = 10, l = 15)
  )


##AFC ####

#Transformation des datas avec Hellinger
data_afc = Macro_Ptscontacts[, -c(19, 22)]
data_afc_clean = data_afc %>% mutate(across(everything(), ~replace_na(.x, 0)))
meta = data_afc_clean %>% dplyr::select(Annee, Site, ID_LAG)
data_num = data_afc_clean %>% dplyr::select(-Annee, -Site, -ID_LAG)
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
    x = "Dim 1",
    y = "Dim 2"
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

ggplot(coord_ind %>% filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05")), 
       aes(x = Dim.1, y = Dim.2, color = as.factor(Annee))) +
  geom_point(size = 3, alpha = 0.8) +
  geom_polygon(
    data = (coord_ind %>% 
              filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05")) %>% 
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

