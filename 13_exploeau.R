###2020 - 2025 : date unique ----
#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro_germi = read.csv("Hydro_germi.csv", header = TRUE, sep = ",", dec=".")

##Nuage de points ----

#en eau ou non 
df_wide_eau = Hydro_germi %>%
  filter(annee %in% c(2020, 2025)) %>%
  select(site, code, annee, eau) %>%
  pivot_wider(
    names_from = annee,
    values_from = eau,
    names_prefix = "eau_"
  ) %>%
  drop_na(eau_2020, eau_2025)

ggplot(df_wide_eau, aes(x = eau_2025, y = eau_2020, color = site)) +
  geom_jitter(size = 3, width = 0.1, height = 0.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  coord_fixed(ratio = 1, xlim = c(0, 2), ylim = c(0, 2), expand = TRUE) +
  labs(
    title = "Présence d'eau : 2025 vs 2020",
    x = "2025",
    y = "2020",
    color = "Site"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey60", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey80", linewidth = 0.3),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_line(color = "black", linewidth = 0.8)
  )
#Parametres eau 
Compo = colnames(Hydro_germi)[!(colnames(Hydro_germi) %in% c("annee", "site", "code","eau"))]

for (comp in Compo) {
  df_wide = Hydro_germi %>%
    select(annee, site, code, value = all_of(comp)) %>%
    filter(annee %in% c(2020, 2025)) %>%
    pivot_wider(
      names_from = annee,
      values_from = value,
      names_prefix = paste0(comp, "_")
    ) 
  col_2020 = paste0(comp, "_2020")
  col_2025 = paste0(comp, "_2025")
  
  if (all(c(col_2020, col_2025) %in% colnames(df_wide))) {
    
    p = ggplot(df_wide, aes_string(x = col_2025, y = col_2020, color = "site")) +
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



##Trajectoire ----

Hydro_germi_acp = Hydro_germi [, -c(5,9)]

var_hydro = Hydro_germi_acp %>%
  select(eau,temperature, hauteur_eau, salinite)

lignes_valides = complete.cases(var_hydro) & !apply(var_hydro, 1, function(x) any(is.infinite(x)))
var_hydro_clean = var_hydro[lignes_valides, ]
Hydro_germi_clean = Hydro_germi_acp[lignes_valides, ]

acp = rda(var_hydro_clean, scale = TRUE)

scores_sites <- scores(acp, display = "sites") %>%
  as.data.frame() %>%
  bind_cols(Hydro_germi_clean %>% select(site, code, annee))

scores_vars <- scores(acp, display = "species") %>%
  as.data.frame() %>%
  rownames_to_column("variable")


#Fleches 
x_lim <- range(scores_sites$PC1)
y_lim <- range(scores_sites$PC2)

max_flèche_x <- max(abs(scores_vars$PC1))
max_flèche_y <- max(abs(scores_vars$PC2))

facteur_x <- (max(x_lim) - min(x_lim)) / (2 * max_flèche_x)
facteur_y <- (max(y_lim) - min(y_lim)) / (2 * max_flèche_y)
flèche_scale <- min(facteur_x, facteur_y) * 0.8

###Par sites 
ggplot(scores_sites, aes(x = PC1, y = PC2, color = site)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = site), type = "t", level = 0.68, linewidth = 1) +
  
  geom_segment(data = scores_vars,
               aes(x = 0, y = 0, xend = PC1 * flèche_scale, yend = PC2 * flèche_scale),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", inherit.aes = FALSE) +
  geom_text(data = scores_vars,
            aes(x = PC1 * flèche_scale, y = PC2 * flèche_scale, label = variable),
            color = "black", vjust = -0.5, size = 4, inherit.aes = FALSE) +
  
  coord_cartesian(xlim = x_lim, ylim = y_lim) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  theme_minimal() +
  labs(
    title = "ACP avec flèches variables (zoom points)",
    x = paste0("PC1 (", round(summary(acp)$cont$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(acp)$cont$importance[2,2] * 100, 1), "%)")
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )


#Par années 

ggplot(scores_sites, aes(x = PC1, y = PC2, color = site)) +
  geom_point(size = 3, aes(shape = factor(annee))) +
  stat_ellipse(aes(group = factor(annee), color = factor(annee)), 
               type = "t", level = 0.68, linewidth = 1) +
  
  geom_segment(data = scores_vars,
               aes(x = 0, y = 0, xend = PC1 * flèche_scale, yend = PC2 * flèche_scale),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", inherit.aes = FALSE) +
  
  geom_text(data = scores_vars,
            aes(x = PC1 * flèche_scale, y = PC2 * flèche_scale, label = variable),
            color = "black", vjust = -0.5, size = 4, inherit.aes = FALSE) +
  
  coord_cartesian(xlim = x_lim, ylim = y_lim) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  
  theme_minimal() +
  
  labs(
    title = "ACP des variables hydrologiques pour 2020 et 2025",
    x = paste0("PC1 (", round(summary(acp)$cont$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(acp)$cont$importance[2,2] * 100, 1), "%)"),
    shape = "Année",
    color = "Site"
  ) +
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

#Site + années 

# 1. Création de la variable Site_Annee
scores_sites <- scores_sites %>%
  mutate(Site_Annee = paste(site, annee, sep = "_"))

# 2. Filtrer les groupes avec au moins 3 points pour tracer ellipse
scores_sites_ellipse <- scores_sites %>%
  group_by(Site_Annee) %>%
  filter(n() >= 3) %>%
  ungroup()

# 3. Calcul des centroïdes par Site, Annee
centroids <- scores_sites_ellipse %>%
  group_by(site, annee, Site_Annee) %>%
  summarise(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(site, annee)

# 4. Création des flèches (trajectoires) reliant les centroïdes par site dans l'ordre des années
arrows_df <- centroids %>%
  group_by(site) %>%
  arrange(annee) %>%
  mutate(
    PC1_end = lead(PC1),
    PC2_end = lead(PC2)
  ) %>%
  filter(!is.na(PC1_end) & !is.na(PC2_end)) %>%
  ungroup()

# 5. Graphique avec points, ellipses par Site_Annee, trajectoires et flèches variables
ggplot(scores_sites_ellipse, aes(x = PC1, y = PC2, color = site)) +
  
  geom_point(aes(shape = factor(annee)), size = 3) +
  
  # Ellipses par groupe Site_Annee
  stat_ellipse(aes(group = Site_Annee), type = "t", level = 0.68, linewidth = 1) +
  
  # Trajectoires entre centroïdes par site
  geom_segment(data = arrows_df,
               aes(x = PC1, y = PC2, xend = PC1_end, yend = PC2_end, color = site),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")),
               linewidth = 1) +
  
  # Flèches des variables (flèche_scale calculé plus haut)
  geom_segment(data = scores_vars,
               aes(x = 0, y = 0, xend = PC1 * flèche_scale, yend = PC2 * flèche_scale),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black", inherit.aes = FALSE) +
  
  geom_text(data = scores_vars,
            aes(x = PC1 * flèche_scale, y = PC2 * flèche_scale, label = variable),
            color = "black", vjust = -0.5, size = 4, inherit.aes = FALSE) +
  
  # Axes limits selon scores_sites
  coord_cartesian(xlim = x_lim, ylim = y_lim) +
  
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "black") +
  
  theme_minimal() +
  
  labs(
    title = "Trajectoire temporelle des lagunes en ACP",
    x = paste0("PC1 (", round(summary(acp)$cont$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(acp)$cont$importance[2,2] * 100, 1), "%)"),
    shape = "Année",
    color = "Site"
  ) +
  
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  )

#Lagunes 

ggplot(scores_sites, aes(x = PC1, y = PC2)) +
  geom_path(aes(group = code),
            arrow = arrow(type = "closed", length = unit(0.15, "inches")),
            color = "grey50", linewidth = 0.8) +
  geom_point(aes(color = factor(annee)), size = 3) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black", linewidth = 1),
    panel.grid.major = element_line(color = "grey80", size = 0.5),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.8, linetype = "dashed") +
  labs(
    color = "Année",
    title = "Trajectoires des lagunes dans l’espace ACP",
    x = paste0("PC1 (", round(summary(acp)$cont$importance[2,1]*100,1), "%)"),
    y = paste0("PC2 (", round(summary(acp)$cont$importance[2,2]*100,1), "%)")
  )


####PERMANOVA####



