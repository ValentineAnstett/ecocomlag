#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")

Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(ID_LAG %in% c("PAL_VIL_07", "PAL_VIL_06","ORB_MAI_04","ORB_MAI_05"))) #Outliers

couleurs_annee = c("2020" = "#F8766D",  # rouge clair
                   "2025" = "#00BFC4") 
Macro_Ptscontacts_sanstot = Macro_Ptscontacts [, -19]

### Permanova sur 2020-2025 ----

especes = Macro_Ptscontacts_sanstot[, 4:ncol(Macro_Ptscontacts_sanstot)]
especes_clean = especes
especes_clean[is.na(especes_clean)] = 0
especes_clean = especes_clean[apply(especes_clean, 1, function(x) sum(x) > 0), ]
rows_to_keep = rownames(especes_clean)
data_clean = Macro_Ptscontacts_sanstot[rownames(Macro_Ptscontacts_sanstot) %in% rows_to_keep, ]


dist_mat = vegdist(especes_clean, method = "bray")

permanova_macro_global_result = adonis2(dist_mat ~ Annee, data = data_clean)
print(permanova_macro_global_result)

permanova_macro_details_result = adonis2(dist_mat ~ Annee + Site + ID_LAG, data = data_clean, by = "terms", permutations = 999)
print(permanova_macro_details_result)

permanova_macro_site_result = adonis2(dist_mat ~ Site, data = data_clean, by = "terms", permutations = 999)
print(permanova_macro_site_result)

permanova_macro_lag_result = adonis2(dist_mat ~ ID_LAG, data = data_clean, by = "terms", permutations = 999)
print(permanova_macro_lag_result)

#Contribution des facteurs 
result <- permanova_macro_details_result
R2 <- result$R2
names(R2) <- rownames(result)
barplot(R2, main = "Contribution des facteurs (R²)", las=2)

#Verifier la dispersion 

disp_annee <- betadisper(dist_mat, data_clean$Annee)
anova(disp_annee)
plot(disp_annee) 

### Wilcoxon test ---- 
#test sur le total 
df_wide = reshape(Macro_Ptscontacts[, c("ID_LAG", "Annee", "TOT")],
                   idvar = "ID_LAG",
                   timevar = "Annee",
                   direction = "wide")

df_wide = na.omit(df_wide)

wilcox.test(df_wide$TOT.2020, df_wide$TOT.2025, paired = TRUE)

wilcox_tot = wilcox.test(df_wide$TOT.2020, df_wide$TOT.2025, paired = TRUE)

#Graph etopile
sig_stars = function(p) {
  if (is.na(p)) {
    return("")
  } else if (p < 0.001) {
    return("***")
  } else if (p < 0.01) {
    return("**")
  } else if (p < 0.05) {
    return("*")
  } else {
    return("")
  }
}

stars = sig_stars(wilcox_tot$p.value)

#Graph

couleurs_annee_transparentes = adjustcolor(couleurs_annee, alpha.f = 0.5)
pval_tot = signif(wilcox_tot$p.value, 3)
pval_text_tot = paste0("p = ", signif(wilcox_tot$p.value, 3), " ", stars)


y_max_tot = max(c(df_wide$TOT.2020, df_wide$TOT.2025), na.rm = TRUE)
y_lim_tot = c(0, y_max_tot * 1.2)
par(mar = c(5, 6, 4, 2) + 0.1)  
boxplot(df_wide$TOT.2020, df_wide$TOT.2025,
        names = c("2020", "2025"),
        main = "Comparaison de TOT entre 2020 et 2025",
        ylab = "Abondance TOT",
        col = couleurs_annee_transparentes,
        cex.main = 1.8,
        cex.lab = 1.5,
        cex.axis = 1.3,
        border = "black",
        ylim = y_lim_tot)
text(x = 1.5, y = y_max_tot * 1.1, labels = pval_text_tot, cex = 1.5)


#test espèce par espèce 

annees_a_comparer = c(2020, 2025)
df_sub = subset(Macro_Ptscontacts_sanstot, Annee %in% annees_a_comparer)
especes_cols = names(df_sub)[4:ncol(df_sub)]

resultats = data.frame(Espece = especes_cols, p_value = NA)

# Boucle sur chaque espèce
for (i in seq_along(especes_cols)) {
  espece = especes_cols[i]
  form = as.formula(paste(espece, "~ Annee"))
  test = tryCatch({
    wilcox.test(form, data = df_sub)
  }, error = function(e) {
    return(NULL)
  })
  if (!is.null(test)) {
    resultats$p_value[i] = test$p.value
  }
}

resultats = resultats[order(resultats$p_value), ]

significatives = subset(resultats, p_value < 0.05)

print(significatives)

#Representation graphique 
especes_signif = c("Ruppia.maritima", "Althenia.filiformis", 
                    "Riella.helicophylla", "Tolypella.salina")

df_plot = df_sub[, c("Annee", especes_signif)]

df_long = pivot_longer(df_plot,
                        cols = all_of(especes_signif),
                        names_to = "Espece",
                        values_to = "Abondance")
#Ajout p-values 
y_pos = df_long %>%
  group_by(Espece) %>%
  summarise(y = max(Abondance, na.rm = TRUE) * 1.05)
annot = left_join(significatives, y_pos, by = "Espece")

# 3. Ajout des étoiles de significativité et création du label complet
annot = annot %>%
  mutate(
    star = sapply(p_value, sig_stars),
    p_label = paste0("p = ", signif(p_value, 3), " ", star)
  )

# 4. Plot avec annotation
ggplot(df_long, aes(x = Espece, y = Abondance, fill = as.factor(Annee))) +
  geom_boxplot(position = position_dodge(0.8), alpha = 0.5) +
  geom_text(data = annot,
            aes(x = Espece, y = y, label = p_label),
            inherit.aes = FALSE,
            size = 6,
            vjust = 0) +
  labs(title = "Comparaison des abondances par espèce (2020 vs 2025)",
       x = "Espèce", y = "Abondance", fill = "Année") +
  theme_minimal(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
  ) +
  scale_fill_manual(values = c("2020" = "#F8766D", "2025" = "#00BFC4"))


###TBI----

Macro_T1 = Macro_Ptscontacts_sanstot %>% filter(Annee == 2020) %>% arrange(ID_LAG)
Macro_T2 = Macro_Ptscontacts_sanstot %>% filter(Annee == 2025) %>% arrange(ID_LAG)
# Verifeir que LAG communes 
ID_communs = intersect(Macro_T1$ID_LAG, Macro_T2$ID_LAG)

Macro_T1 = Macro_T1 %>% filter(ID_LAG %in% ID_communs) %>% arrange(ID_LAG)
Macro_T2 = Macro_T2 %>% filter(ID_LAG %in% ID_communs) %>% arrange(ID_LAG)

#Création des matrices d'especes 
especes_T1 = Macro_T1 %>% dplyr::select(-Annee, -Site, -ID_LAG)
especes_T2 = Macro_T2 %>% dplyr::select(-Annee, -Site, -ID_LAG)
rownames(especes_T1) = Macro_T1$ID_LAG
rownames(especes_T2) = Macro_T2$ID_LAG

#Nettoyage 
non_vides = which(rowSums(especes_T1) != 0 & rowSums(especes_T2) != 0)
especes_T1 = especes_T1[non_vides, ]
especes_T2 = especes_T2[non_vides, ]
ID_LAG_vecteur = Macro_T1$ID_LAG[non_vides]


#### Calcul du TBI ----

result = TBI(especes_T1, especes_T2, method = "%difference", nperm = 999, test.t.perm = FALSE)

# Résumé dans un tableau

tbi_result = data.frame(
  ID_LAG = ID_LAG_vecteur,
  TBI = result$TBI,
  p_value = result$p.TBI,
  pertes = result$BCD.mat[, "B/(2A+B+C)"],
  gains = result$BCD.mat[, "C/(2A+B+C)"],
  change = result$BCD.mat[, "D=(B+C)/(2A+B+C)"]
)


#### Graphs TBI ----
tbi_result = tbi_result %>%
  left_join(Macro_T1 %>% dplyr::select(ID_LAG, Site), by = "ID_LAG")
tbi_result_sorted = tbi_result %>%
  arrange(desc(change))

#1
plot(tbi_result)
text(tbi_result$BCD.mat[, "B/(2A+B+C)"], tbi_result$BCD.mat[, "C/(2A+B+C)"],
     labels = ID_LAG_vecteur, pos = 3, cex = 0.8)
#2
plot(result$BCD.mat[, "B/(2A+B+C)"],  # pertes
     result$BCD.mat[, "C/(2A+B+C)"],  # gains
     xlab = "Losses (B / (2A+B+C))",
     ylab = "Gains (C / (2A+B+C))",
     main = "Changes in dissimilarity (TBI)",
     pch = 19, col = "grey",
     xlim = c(0,1),
     ylim = c(0,1)
     )

text(result$BCD.mat[, "B/(2A+B+C)"], result$BCD.mat[, "C/(2A+B+C)"],
     labels = rownames(result$BCD.mat), pos = 3, cex = 0.7)

abline(a = 0, b = 1, col = "lightgreen", lty = 1, lwd = 2)

#Courbe qui suit la distrib 
#loess_fit = loess(result$BCD.mat[, "C/(2A+B+C)"] ~ result$BCD.mat[, "B/(2A+B+C)"])
#x_vals = seq(min(result$BCD.mat[, "B/(2A+B+C)"]), max(result$BCD.mat[, "B/(2A+B+C)"]), length.out = 100)
#y_preds = predict(loess_fit, newdata = x_vals)
#lines(x_vals, y_preds, col = "darkblue", lwd = 2)

#Ligne de déviance 
delta = mean(result$BCD.mat[, "C/(2A+B+C)"] - result$BCD.mat[, "B/(2A+B+C)"], na.rm = TRUE)
abline(a = delta, b = 1, col = "red", lty = 3, lwd = 2)

#4
df_long = tbi_result %>%
  dplyr::select(ID_LAG, pertes, gains) %>%
  pivot_longer(cols = c(pertes, gains), names_to = "type", values_to = "valeur")

ggplot(df_long, aes(x = ID_LAG, y = valeur, fill = type)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("pertes" = "#F8766D", "gains" = "lightgreen")) +
  labs(title = "Pertes et gains d'espèces par site (2020-2025)",
       x = "ID_LAG",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7 : Scatter plot Pertes/Gains avec polygones 
hulls = tbi_result %>%
  group_by(Site) %>%
  slice(chull(pertes, gains)) %>%
  ungroup()

ggplot(tbi_result, aes(x = pertes, y = gains, color = Site)) +
  geom_point(size = 3) +
  geom_polygon(data = hulls, aes(fill = Site), alpha = 0.2, color = NA) +
  labs(title = "Pertes vs Gains par site avec polygones convexes",
       x = "Pertes",
       y = "Gains") +
  theme_minimal()

# Avec lignes 
ggplot(tbi_result, aes(x = pertes, y = gains, color = Site)) +
  geom_point(size = 3) +
  geom_polygon(data = hulls, aes(fill = Site), alpha = 0.2, color = NA) +
  geom_abline(slope = 1, intercept = 0, color = "lightgreen", linetype = "solid", size = 1) +
  geom_abline(slope = 1, intercept = delta, color = "red", linetype = "dotted", size = 1) +
  labs(
    title = "Changes in dissimilarity (TBI) with convex hulls by site",
    x = "Losses (B / (2A + B + C))",
    y = "Gains (C / (2A + B + C))",
    color = "Sites",      # titre légende color
    fill = "Sites"        # titre légende fill
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal() +
  theme(
    legend.position = "right",         # déplace la légende à droite
    legend.direction = "vertical",     # disposition verticale des items
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.key.width = unit(1, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 15))  # espace au-dessus du titre x
  ) +
  guides(
    color = guide_legend(ncol = 1),   # une colonne, pour vertical
    fill = guide_legend(ncol = 1)
  )

#8 : Contour de densité 
ggplot(tbi_result, aes(x = pertes, y = gains, label = ID_LAG)) +
  geom_density_2d(color = "red") + 
  geom_point(size = 3, color = "grey") +
  geom_text_repel(vjust = -0.5, size = 3) +
  labs(
    title = "Pertes vs Gains d'espèces par site avec contours de densité",
    x = "Pertes (B/(2A+B+C))",
    y = "Gains (C/(2A+B+C))"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +  # ⬅️ cadre qui colle aux axes
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )

#9 : Heatmap de densité

pb = ggplot_build(
  ggplot(tbi_result, aes(pertes, gains)) + geom_density_2d_filled()
)

niveaux = length(levels(pb$data[[1]]$level))  # 13
palette_vert_rouge <- colorRampPalette(c("lightgreen", "orange", "#F8766D"))(niveaux)

ggplot(tbi_result, aes(x = pertes, y = gains)) +
  geom_density_2d_filled(alpha = 0.8, bins = niveaux) +  # heatmap en dessous
  scale_fill_manual(values = palette_vert_rouge) +
  geom_point(size = 3, color = "grey") +              # points au-dessus
  geom_text_repel(aes(label = ID_LAG), vjust = -0.5, size = 3) + # texte au-dessus
  labs(title = "Pertes vs Gains avec heatmap de densité",
       x = "Pertes (B/(2A+B+C))",
       y = "Gains (C/(2A+B+C))") +
  theme_minimal()

#avec contours et les lignes 
ggplot(tbi_result, aes(x = pertes, y = gains)) +
  geom_density_2d_filled(alpha = 0.8, bins = niveaux) +
  scale_fill_manual(values = palette_vert_rouge, name = "Density") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid", size = 1) +
  geom_abline(slope = 1, intercept = delta, color = "red", linetype = "dotted", size = 2) +
  geom_point(size = 3, color = "grey") +
  geom_point(data = tbi_result %>% filter(pertes == 0 | gains == 0),
             aes(x = pertes, y = gains),
             color = "gray", size = 3, inherit.aes = FALSE) +
  geom_text_repel(aes(label = ID_LAG), vjust = -0.5, size = 3) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  
  coord_cartesian() +
  labs(
    title = "Changes in dissimilarity (TBI) with density heatmap",
    x = "Losses (B / (2A + B + C))",
    y = "Gains (C / (2A + B + C))"
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.key.width = unit(1, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 15))
  ) +
  guides(
    fill = guide_legend(ncol = 1)
  )



####Sites les plus changés ----

tbi_result = tbi_result %>%
  left_join(Macro_T1 %>% dplyr::select(ID_LAG, Site), by = "ID_LAG")
tbi_result_sorted = tbi_result %>%
  arrange(desc(change))

head(tbi_result_sorted, 5)
tail(tbi_result_sorted, 5)

#Graph 
#1 : Par lagunes 
ggplot(tbi_result_sorted, aes(x = reorder(ID_LAG, change), y = change, fill = Site)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Changement total (TBI) par lagune",
    x = "ID_LAG",
    y = "TBI - Dissimilarité totale (B+C)/(2A+B+C)"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

#2 : Par sites 

tbi_result = tbi_result %>%
  mutate(Site = factor(Site, levels = names(sort(tapply(change, Site, median), decreasing = TRUE))))

ggplot(tbi_result, aes(x = Site, y = change)) +
  geom_boxplot(fill = "#A6D8A8", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.6, color = "darkblue") +
  geom_hline(yintercept = 0, color = "grey80", size = 0.5) +
  labs(
    title = "Variation of Change (TBI) by Site",
    x = "Site",
    y = "TBI - Total Dissimilarity"
  ) +
  theme_minimal(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

