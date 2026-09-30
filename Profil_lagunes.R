# ============================================================
# Profil environnemental des lagunes — Boxplots par site
# Variables : water_level, surface, mise_en_eau, duree_assec
# ============================================================

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(scales)
library(RColorBrewer)
library(dplyr)

# ---- 1. Chargement et préparation des données ----

df_raw <- Data_envir_V3

vars_keep <- c("Year", "Site", "ID_LAG", "salinity","water_level", "Surface", "mise_en_eau", "duree_assec")

df <- df_raw %>%
  dplyr::select(all_of(vars_keep)) %>%
  filter(!is.na(Site)) %>%
  mutate(
    # Nombre de jours depuis le 1er mai précédant la mise en eau
    mise_en_eau_date = as.Date(mise_en_eau, format = "%Y-%m-%d"),
    ref_may1 = as.Date(ifelse(
      as.integer(format(mise_en_eau_date, "%m")) >= 5,
      paste0(format(mise_en_eau_date, "%Y"), "-05-01"),
      paste0(as.integer(format(mise_en_eau_date, "%Y")) - 1L, "-05-01")
    )),
    mise_en_eau = as.numeric(mise_en_eau_date - ref_may1)
  ) %>%
  dplyr::select(-mise_en_eau_date, -ref_may1)
# Chaque ligne = 1 lagune × 1 année → traité comme un réplicat
# ---- 2. Palette de couleurs par site (une couleur pastel par site) ----

sites <- sort(unique(df$Site))
n_sites <- length(sites)

# Palette pastel distincte (comme le graphique d'exemple)
palette_pastel <- c(
  "#A8D8B9", "#F9E4B7", "#F4B8C1", "#B8D4F0", "#D4C5F9",
  "#F4D4B8", "#C1E8C1", "#E8D4F4", "#F4E8C1", "#B8E8F4",
  "#F4C1C1"
)
if (n_sites > length(palette_pastel)) {
  palette_pastel <- colorRampPalette(palette_pastel)(n_sites)
}
names(palette_pastel) <- sites

# ---- 3. Fonction de détection des outliers (méthode IQR) ----

is_outlier <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
}

# ---- 4. Fonction générique pour créer un boxplot par variable ----

make_boxplot <- function(data, var, var_label, log_scale = FALSE) {
  
  df_var <- data %>%
    filter(!is.na(.data[[var]])) %>%
    group_by(Site) %>%
    mutate(outlier = is_outlier(.data[[var]])) %>%
    ungroup() %>%
    mutate(Year = factor(Year))
  
  df_outliers <- df_var %>% filter(outlier)
  
  p <- ggplot(df_var, aes(x = Site, y = .data[[var]], fill = Site)) +
    
    # Boîtes (toutes années confondues par site)
    geom_boxplot(
      outlier.shape = NA,
      alpha = 0.6,
      width = 0.6,
      color = "grey40",
      linewidth = 0.4
    ) +
    
    # Points par réplicat (année), forme différente par année
    geom_jitter(
      aes(shape = Year, color = Year),
      width = 0.15,
      size = 2,
      alpha = 0.85
    ) +
    
    # Labels des outliers en rouge : ID_LAG + année
    geom_text_repel(
      data = df_outliers,
      aes(label = paste0(ID_LAG, "\n(", Year, ")")),
      color = "red3",
      size = 2.5,
      fontface = "bold",
      max.overlaps = 25,
      box.padding = 0.35,
      segment.color = "red3",
      segment.size = 0.3,
      lineheight = 0.85
    ) +
    
    scale_fill_manual(values = palette_pastel) +
    scale_color_manual(
      values = c("2020" = "#2166ac", "2025" = "#d6604d"),
      name = "Année"
    ) +
    scale_shape_manual(
      values = c("2020" = 16, "2025" = 17),
      name = "Année"
    ) +
    
    labs(
      title = var_label,
      x = NULL,
      y = "Valeur"
    ) +
    
    theme_bw(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 12, hjust = 0.5,
                                        margin = margin(b = 6)),
      legend.position    = "right",
      legend.title       = element_text(size = 9, face = "bold"),
      legend.text        = element_text(size = 8),
      legend.key.size    = unit(0.5, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 10, face = "bold"),
      axis.text.y        = element_text(size = 9),
      plot.background    = element_rect(fill = "white", color = NA),
      panel.border       = element_rect(color = "grey70")
    )
  
  if (log_scale) {
    p <- p + scale_y_log10(labels = label_comma())
  }
  
  return(p)
}

# ---- 5. Création des 4 graphiques ----

p1 <- make_boxplot(df, "water_level",  "Niveau d'eau (water_level)")
p2 <- make_boxplot(df, "Surface",      "Surface (m²)",  log_scale = TRUE)
p3 <- make_boxplot(df, "mise_en_eau",  "Jour julien — Mise en eau")
p4 <- make_boxplot(df, "duree_assec",  "Durée assec (jours)")
p5 = make_boxplot(df, "salinity", "Salinité (g/L)")

print(p1)
print(p2)
print(p3)
print(p4)
print (p5)
# ---- 6. Assemblage avec patchwork ----

final_plot <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title    = "Profil environnemental — Outliers par site",
    subtitle = "Chaque point = une lagune (ID_LAG) · Outliers IQR identifiés en rouge",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15, hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40",
                                   margin = margin(b = 10))
    )
  )
print(final_plot)

# ---- 7. Export ----

ggsave(
  filename = "profil_lagunes_outliers.png",
  plot     = final_plot,
  width    = 14,
  height   = 10,
  dpi      = 300,
  bg       = "white"
)

message("✔ Graphique exporté : profil_lagunes_outliers.png")

# ---- 8. [OPTIONNEL] Graphiques séparés par variable ----
# Décommenter pour exporter chaque variable individuellement :

# plots_list <- list(
#   water_level = p1,
#   Surface     = p2,
#   mise_en_eau = p3,
#   duree_assec = p4
# )
# 
# walk2(plots_list, names(plots_list), ~ {
#   ggsave(paste0("boxplot_", .y, ".png"), plot = .x,
#          width = 8, height = 5, dpi = 300, bg = "white")
# })
