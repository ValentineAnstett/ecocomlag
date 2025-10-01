###2020 - 2025 : date unique ----
#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro_germi = read.csv("Hydro_germi.csv", header = TRUE, sep = ",", dec=".")

#####Nuage de points : mise en eau ----
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
####Nuages de points : détails de chaque paramètres -----
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



####Trajectoire ----

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

#1) Graphs par sites (années confondues)
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


#2) Graphs par années (sites confondus) 

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

#3) Graphs par Site et par années 


scores_sites = scores_sites %>%
  mutate(Site_Annee = paste(site, annee, sep = "_")) # Création de la variable Site_Annee


scores_sites_ellipse = scores_sites %>%
  group_by(Site_Annee) %>%
  filter(n() >= 3) %>%
  ungroup() # Filtrer les groupes avec au moins 3 points pour tracer ellipse


centroids <- scores_sites_ellipse %>%
  group_by(site, annee, Site_Annee) %>%
  summarise(
    PC1 = mean(PC1, na.rm = TRUE),
    PC2 = mean(PC2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(site, annee) # Calcul des centroïdes par Site, Annee

# Création des flèches (trajectoires) reliant les centroïdes par site dans l'ordre des années
arrows_df <- centroids %>%
  group_by(site) %>%
  arrange(annee) %>%
  mutate(
    PC1_end = lead(PC1),
    PC2_end = lead(PC2)
  ) %>%
  filter(!is.na(PC1_end) & !is.na(PC2_end)) %>%
  ungroup()


ggplot(scores_sites_ellipse, aes(x = PC1, y = PC2, color = site)) +
  geom_point(aes(shape = factor(annee)), size = 3) +
  stat_ellipse(aes(group = Site_Annee), type = "t", level = 0.68, linewidth = 1) +
  geom_segment(data = arrows_df,
               aes(x = PC1, y = PC2, xend = PC1_end, yend = PC2_end, color = site),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")),
               linewidth = 1) +
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

#4) Graphs avec le détails des lagunes 

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

###2019-2025 : suivis mensuels ----
# Import dataset
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro_mens <- read.csv("Hydro_mens.csv", header = TRUE, sep = ",", dec = ".")


vars_hydro = c("turbidite", "temperature", "hauteur_eau", "salinite", "conductivite")

Hydro_mens = Hydro_mens %>%
  mutate(across(all_of(vars_hydro), ~ str_replace_all(., ",", "."))) %>%
  mutate(across(all_of(vars_hydro), ~ as.numeric(as.character(.))))  # Remplacer les virgules par des points, puis convertir en numérique

# Gérer les NA : 
# si hauteur_eau = 0 et variable NA, remplacer NA par 0, sinon garder variable
Hydro_mens = Hydro_mens %>%
  mutate(across(all_of(vars_hydro), ~ ifelse(hauteur_eau == 0 & is.na(.), 0, .)))

# Création des variables date, mois, année calendaire et hydrologique
Hydro_mens = Hydro_mens %>%
  mutate(
    date = as.Date(paste0(date_releve, "-01")),
    mois = month(date),
    annee_cal = year(date),
    annee_hydro = if_else(mois >= 8, annee_cal + 1, annee_cal), # année hydrologique : mois août (8) devient l'année suivante
    mois_hydro = if_else(mois >= 8, mois - 7, mois + 5) # mois dans l'année hydrologique : août = 1, ..., juillet = 12
  )

# Supprimer les lignes où il y a NA dans une des variables hydro
Hydro_mens = Hydro_mens %>%
  filter(if_all(all_of(vars_hydro), ~ !is.na(.)))

### Visualisation individuelle par code et variable ----

visualize_variable = function(data, code_sel, var_sel) {
  df_plot <- data %>%
    filter(code == code_sel) %>%
    filter(!is.na(.data[[var_sel]])) %>%
    arrange(annee_hydro, mois_hydro)
  
  ggplot(df_plot, aes(x = mois_hydro, y = .data[[var_sel]], color = factor(annee_hydro), group = annee_hydro)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb[c(8:12, 1:7)]
    ) +
    labs(
      title = paste("Variation annuelle de", var_sel, "pour le site", code_sel),
      x = "Mois hydrologique (Août = 1)",
      y = var_sel,
      color = "Année hydrologique"
    ) +
    theme_minimal()
}
codes = unique(Hydro_mens$code)
variables = c("turbidite", "temperature", "hauteur_eau", "salinite", "conductivite")

for (code_i in codes) {
  for (var_i in variables) {
    p = visualize_variable(Hydro_mens, code_sel = code_i, var_sel = var_i)
    if (!is.null(p)) {
      print(p)  # Affiche dans la console (utile pour explorer visuellement)
    }
  }
}


#Rassembler par site 
# Transformer en format long
Hydro_long = Hydro_mens %>%
  pivot_longer(
    cols = all_of(vars_hydro),
    names_to = "variable",
    values_to = "value"
  )
# Calcul des médianes et quantiles par site, année hydrologique, mois, variable
summary_site = Hydro_long %>%
  group_by(site, annee_hydro, mois_hydro, variable) %>%
  summarise(
    median_val = median(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    .groups = "drop"
  )
#Fonction de plot 
plot_site_variable <- function(df_summary, site_sel, var_sel) {
  df_plot <- df_summary %>%
    filter(site == site_sel, variable == var_sel)
  
  if (nrow(df_plot) == 0) {
    return(NULL)
  }
  
  ggplot(df_plot, aes(x = mois_hydro)) +
    geom_point(aes(y = median_val, color = factor(annee_hydro)), size = 3) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = factor(annee_hydro)), alpha = 0.2) +
    scale_x_continuous(breaks = 1:12, labels = month.abb[c(8:12, 1:7)]) +
    labs(
      title = paste0("Variation de ", var_sel, " - Site: ", site_sel),
      x = "Mois hydrologique (Août = 1)",
      y = var_sel,
      color = "Année hydrologique",
      fill = "Année hydrologique"
    ) +
    theme_minimal()
}
#Afficher
sites = unique(summary_site$site)
variables = unique(summary_site$variable)

for (site_i in sites) {
  for (var_i in variables) {
    p <- plot_site_variable(summary_site, site_i, var_i)
    if (!is.null(p)) {
      print(p)
    }
  }
}
