#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")

Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(ID_LAG %in% c("PAL_VIL_07", "PAL_VIL_06","ORB_MAI_04","ORB_MAI_05")))

couleurs_annee = c("2020" = "#F8766D",  # rouge clair
                   "2025" = "#00BFC4") 

### Permanova sur 2020-2025 ----

especes = Macro_Ptscontacts[, 4:ncol(Macro_Ptscontacts)]
especes_clean = especes
especes_clean[is.na(especes_clean)] = 0
especes_clean = especes_clean[apply(especes_clean, 1, function(x) sum(x) > 0), ]
rows_to_keep = rownames(especes_clean)
data_clean = Macro_Ptscontacts[rownames(Macro_Ptscontacts) %in% rows_to_keep, ]


dist_mat = vegdist(especes_clean, method = "bray")

permanova_macro_global_result = adonis2(dist_mat ~ Annee, data = data_clean)
print(permanova_macro_global_result)

permanova_macro_details_result = adonis2(dist_mat ~ Annee + Site + ID_LAG, data = data_clean, by = "terms", permutations = 999)
print(permanova_macro_details_result)


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
pval_tot = signif(wilcox_tot$p.value, 3)
pval_text_tot = paste0("p = ", signif(wilcox_tot$p.value, 3), " ", stars)


y_max_tot = max(c(df_wide$TOT.2020, df_wide$TOT.2025), na.rm = TRUE)
y_lim_tot = c(0, y_max_tot * 1.2)
par(mar = c(5, 6, 4, 2) + 0.1)  
boxplot(df_wide$TOT.2020, df_wide$TOT.2025,
        names = c("2020", "2025"),
        main = "Comparaison de TOT entre 2020 et 2025",
        ylab = "Abondance TOT",
        col = couleurs_annee,
        cex.main = 1.8,
        cex.lab = 1.5,
        cex.axis = 1.3,
        border = "black",
        ylim = y_lim_tot)
text(x = 1.5, y = y_max_tot * 1.1, labels = pval_text_tot, cex = 1.5)


#test espèce par espèce 

annees_a_comparer = c(2020, 2025)
df_sub = subset(Macro_Ptscontacts, Annee %in% annees_a_comparer)
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
  geom_boxplot(position = position_dodge(0.8)) +
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

#Clustering 


#TBI ? 