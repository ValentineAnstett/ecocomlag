#####I.a - Plant community structure -----
#Cluster----
# Packages

library(vegan)
library(ggplot2)
library(ggdendro)
library(dplyr)
library(tidyr)

#Données

getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro = read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=",")

Macro[is.na(Macro)] = 0 #Remplacer les 3 NA par 0 
Macro[, 4:21] = lapply(Macro[, 4:21], function(x) as.numeric(as.character(x))) #Passer les colonnes en num 
Macro= Macro %>%
  filter(!(ID_LAG %in% c("G_07", "G_06","D_04","D_05")))
Macro = Macro %>% dplyr::select(-c(algues,TOT))
Macro = Macro[, colSums(Macro != 0, na.rm = TRUE) > 0]


comm <- Macro[, 4:12] #Sortir les colonnes avec les chr
comm <- comm[rowSums(comm) > 0, ] #Supprimer les lignes vides 
comm <- comm[, colSums(comm) > 0]


# Distance de Bray-Curtis
dist_bray = vegdist(comm, method = "bray")

#Cophenetic : choix de la méthode de clustering 

#methods <- c("complete", "average", "ward.D2")

#coph <- sapply(methods, function(m){
  #hc <- hclust(dist_bray, method = m)
  #cor(dist_bray, cophenetic(hc))
#})

#best_method <- methods[which.max(coph)]
#hc <- hclust(dist_bray, method = best_method)


#Forcer Ward 
hc <- hclust(dist_bray, method = "ward.D2")
best_method <- "ward.D2"
cat("Méthode retenue :", best_method, "\n")
cat("Corrélation cophenétique :", max(coph), "\n")

#Permanova
# On coupe l'arbre en k groupes
k <- 3
groups <- cutree(hc, k = k)

adonis_res <- adonis2(comm ~ as.factor(groups),
                      method = "bray",
                      permutations = 999)

print(adonis_res)



# Dendrogramme ----


dend <- dendro_data(hc)

leaf_pos <- dend$labels
leaf_pos$site <- leaf_pos$label
leaf_pos <- leaf_pos[order(leaf_pos$x), ]

leaf_pos$x_plot <- leaf_pos$x
leaf_pos$y_plot <- 0
leaf_pos$group <- as.factor(groups[leaf_pos$site])



# Ordre initial des espèces (optionnel, uniquement pour cohérence des données)

dist_sp <- vegdist(t(comm), method = "bray")
hc_sp <- hclust(dist_sp, method = "average")

sp_order <- hc_sp$labels[hc_sp$order]



# Ordre FIXE des espèces (manuel)

sp_order_opt <- c(
  "Riella.helicophylla",
  "Tolypella.salina",
  "Chara.canescens",
  "Tolypella.hispanica",
  "Ruppia.maritima",
  "Lamprothamnium.papulosum",
  "Althenia.filiformis",
  "Ruppia.cirrhosa",
  "Riella.notarisii"
)

sp_pos <- data.frame(
  species = sp_order_opt,
  x_plot = seq(min(leaf_pos$x), max(leaf_pos$x), length.out = length(sp_order_opt)),
  y_plot = -2
)



# Réseau bipartite initial

links <- as.data.frame(comm) %>%
  mutate(site = rownames(comm)) %>%
  pivot_longer(-site, names_to = "species", values_to = "abundance") %>%
  filter(abundance > 0)


# Mise à jour des liens après optimisation

links <- as.data.frame(comm) %>%
  mutate(site = rownames(comm)) %>%
  pivot_longer(-site, names_to = "species", values_to = "abundance") %>%
  filter(abundance > 0) %>%
  
  left_join(leaf_pos, by = "site") %>%
  rename(x_site = x_plot, y_site = y_plot) %>%
  
  left_join(sp_pos, by = "species") %>%
  rename(x_sp = x_plot, y_sp = y_plot)



# PLOT FINAL

p <- ggplot() +
  
  # dendrogramme
  geom_segment(data = dend$segments,
               aes(x = x, y = y, xend = xend, yend = yend),
               linewidth = 1) +
  
  # liens bipartites
  geom_segment(data = links,
               aes(x = x_site, y = y_site,
                   xend = x_sp, yend = y_sp,
                   linewidth = abundance),
               alpha = 0.5, color = "grey30") +
  
  # sites
  geom_point(data = leaf_pos,
             aes(x = x_plot, y = y_plot, fill = group),
             size = 3, shape = 21, color = "black") +
  
  # espèces
  geom_point(data = sp_pos,
             aes(x = x_plot, y = y_plot, color = species),
             size = 5) +
  
  geom_text(data = sp_pos,
            aes(x = x_plot, y = y_plot - 0.3, label = species),
            size = 4) +
  
  scale_linewidth(range = c(0.2, 2.5)) +
  
  scale_fill_brewer(palette = "Set1", name = "Groupes") +
  scale_color_brewer(palette = "Dark2", name = "Espèces") +
  
  labs(
    title = "Clustering des relevés + réseau bipartite",
    subtitle = paste("Méthode :", best_method,
                     "| PERMANOVA p =",
                     signif(adonis_res$`Pr(>F)`[1], 3)),
    y = "Distance Bray-Curtis"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    panel.grid = element_blank()
  )

print(p)



#ACP Simplifié ####

#Transformation des datas avec Hellinger
data_acp = Macro
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

ggplot(coord_ind %>% filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05","K_03", "K_10", "B_06", "K_08", "B_09", "K_10", "B_01", "K_09", "B_10", "B_08")), aes(x = Dim.1, y = Dim.2, color = Site)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_polygon(
    data = (coord_ind %>% filter(!ID_LAG %in% c("G_07", "G_06","D_04","D_05","K_03", "K_10", "B_06", "K_08", "B_09", "K_10", "B_01", "K_09", "B_10", "B_08")) %>% group_by(Site) %>% group_modify(~get_hull(.x))),
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


#Identifier les outliers 
coord_ind$dist_origin <- sqrt(coord_ind$Dim.1^2 + coord_ind$Dim.2^2)
outliers_top10 <- coord_ind %>%
  arrange(desc(dist_origin)) %>%
  slice(1:10)

####I.b - Species richness ----
#Richesse spé par annee ----
Macro_sp = Macro %>%
  rowwise() %>%
  mutate(n_sp = sum(c_across(-c(Year, Site, ID_LAG)) > 0, na.rm = TRUE)) %>%
  ungroup()

df_n_sp = Macro_sp %>%
  dplyr::select(Year,Site, ID_LAG, n_sp) %>%
  filter(Year %in% c(2020, 2025)) %>%
  pivot_wider(
    names_from = Year,
    values_from = n_sp,
    names_prefix = "n_sp_"
  ) %>%
  drop_na()


# Graphique avec barycentre et IC

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

p = ggplot(df_n_sp, aes(x = n_sp_2020, y = n_sp_2025, color = Site)) +
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

ggsave("Ib_speciesrichness.svg", plot = p, device = "svg")



#I.c - Ecological drivers ----

#Import dataset Envirr 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data")
Data_envir= read.csv("Data_envir_V3.csv", header = TRUE, sep = ",", dec=".")


Data_envir$mise_en_eau = as.Date(Data_envir$mise_en_eau)
Data_envir$mise_en_eau_num = as.numeric(format(Data_envir$mise_en_eau, "%j"))

Data_envir = Data_envir %>%
  filter(!(ID_LAG %in% c("G_07", "G_06","D_04","D_05"))) #Outliers

#Selection des variables 
Data_envir = Data_envir %>%
  dplyr::select(Site, ID_LAG, Year, organic_matter, ilr_fines_vs_sand, ilr_clay_vs_silt, water_level, Surface, dist_trait_cote_m, salinity, mise_en_eau_num, duree_assec)



#RDA ----
#Preparer les données 
df_merged = merge(Macro, Data_envir, by = c("Year", "Site", "ID_LAG"))

Y = df_merged [, 4:12]  #Especes == variable a exploquer 
X = df_merged [, 13:21] #Var envirr == variables explicatives 

# Nettoyage
complete_rows = complete.cases(Y, X)
Y_clean = Y[complete_rows, ]
X_clean = X[complete_rows, ]

# --- Transformation Hellinger (espèces) ---
library(vegan)
Y_hell = decostand(Y_clean, method = "hellinger")

# --- Standardisation variables environnementales ---
X_scaled = scale(X_clean)

# --- RDA ---
rda_result = rda(Y_hell ~ ., data = as.data.frame(X_scaled))

summary(rda_result)

#Tester la significativite de la RDA ? 
anova(rda_result)                      # Test global
anova(rda_result, by = "axis")        # Test par axe
anova(rda_result, by = "term") 

# Plot RDA
# Obtenir les scores
site_scores = scores(rda_result, display = "sites", scaling = 2)
species_scores = scores(rda_result, display = "species", scaling = 2)
env_scores = scores(rda_result, display = "bp", scaling = 2)

# Mettre en frame avec les axes
df_sites = as.data.frame(site_scores)
df_species = as.data.frame(species_scores)
df_env = as.data.frame(env_scores)
df_species$Species = rownames(df_species)
df_env$Var = rownames(df_env)

# --- Extraire % variance expliquée ---
eig_vals <- summary(rda_result)$concont$importance["Proportion Explained", 1:2] * 100
expl_var1 <- round(eig_vals[1], 1)
expl_var2 <- round(eig_vals[2], 1)

# --- Graphique RDA ---
graph1 = ggplot() +
  # Sites
  geom_point(data = df_sites, aes(x = RDA1, y = RDA2),
             colour = "grey50", size = 4) +
  
  # Flèches espèces
  geom_segment(data = df_species, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.3, "cm")),
               colour = "darkgreen", linewidth = 0.8) +
  geom_text_repel(
    data = df_species, aes(x = RDA1, y = RDA2, label = Species),
    colour = "darkgreen",
    size = 7,
    box.padding = 0.7,
    point.padding = 0.7,
    segment.size = 0.6,
    segment.color = "darkgreen",
    min.segment.length = 0,
    max.overlaps = Inf,
    force = 2
  ) +
  
  # Flèches environnement
  geom_segment(data = df_env, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.3, "cm")),
               colour = "darkblue", linewidth = 0.8) +
  geom_text_repel(
    data = df_env, aes(x = RDA1, y = RDA2, label = Var),
    colour = "darkblue",
    size = 7,
    box.padding = 0.7,
    point.padding = 0.7,
    segment.size = 0.6,
    segment.color = "darkblue",
    min.segment.length = 0,
    max.overlaps = Inf,
    force = 2
  ) +
  
  # Axes
  geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.6) +
  
  # Labels axes avec % de variance
  xlab(paste0("RDA1 (", expl_var1, "%)")) +
  ylab(paste0("RDA2 (", expl_var2, "%)")) +
  
  # Thème publication
  theme_minimal(base_size = 18) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.title = element_text(size = 25),
    axis.text = element_text(size = 25),
    plot.title = element_text(size = 25, face = "bold")
  )

print(graph1)

ggsave("Images/test.svg", plot=graph1, width=10, height=8)

#### Influence des variables environnementales ----
species_scores = scores(rda_result, display = "species", scaling = 2)
env_scores = scores(rda_result, display = "bp", scaling = 2)

cosine_similarity = function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

cor_matrix = matrix(NA, nrow = nrow(species_scores), ncol = nrow(env_scores),
                    dimnames = list(rownames(species_scores), rownames(env_scores)))

for (i in 1:nrow(species_scores)) {
  for (j in 1:nrow(env_scores)) {
    cor_matrix[i, j] <- cosine_similarity(species_scores[i, 1:2], env_scores[j, 1:2])
  }
}
round(cor_matrix, 2)

apply(cor_matrix, 1, function(x) {
  var_max <- names(which.max(abs(x)))
  value <- x[var_max]
  c(Var = var_max, Correlation = round(value, 2))
})


cor_matrix_clean = cor_matrix[!apply(cor_matrix, 1, function(x) any(is.na(x))), ]
pheatmap(
  cor_matrix_clean,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  display_numbers = TRUE,
  fontsize = 14,        
  fontsize_number = 12, 
  number_color = "black"
)

