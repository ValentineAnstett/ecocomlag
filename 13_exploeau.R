#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro_germi = read.csv("Hydro_germi.csv", header = TRUE, sep = ",", dec=".")



##Nuage de points 

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

