#Import dataset Flore
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")

#Import dataset Envirr 
#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data")
Data_envir= read.csv("Data_envir.csv", header = TRUE, sep = ",", dec=".")


### RDA ----
#Sortir la colonne sable pour eviter une correlation entre les variables envir 
Data_envir = Data_envir [, -11]

#Preparer les données 
df_merged = merge(Macro_Ptscontacts, Data_envir, by = c("Annee", "Site", "ID_LAG"))

Y = df_merged [, 4:21]  #Especes == variable a exploquer 
X = df_merged [, 22:28] #Var envirr == variables explicatives 

#Nettoyer les donnes 
complete_rows = complete.cases(Y, X)
Y_clean = Y[complete_rows, ]
X_clean = X[complete_rows, ]
cat("Nombre de lignes après nettoyage :", nrow(Y_clean), "\n")

#RDA
rda_result = rda(Y_clean ~ ., data = X_clean)

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

# Graphe ggplot
ggplot() +
  geom_point(data = df_sites, aes(x = RDA1, y = RDA2), colour = "grey50") +
  geom_segment(data = df_species, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")), colour = "darkgreen") +
  geom_text_repel(data = df_species, aes(x = RDA1, y = RDA2, label = Species),
                  colour = "darkgreen", size = 5, max.overlaps = 30) +  
  geom_segment(data = df_env, aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               arrow = arrow(length = unit(0.2, "cm")), colour = "darkblue") +
  geom_text_repel(data = df_env, aes(x = RDA1, y = RDA2, label = Var),
                  colour = "darkblue", size = 5, max.overlaps = 30) + 
  xlab("RDA1") +
  ylab("RDA2") +
  ggtitle("Biplot RDA : Espèces (vert) / Variables environnementales (bleu)") +
  theme_minimal(base_size = 16)


#Influence des variables environnementales 
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
pheatmap(cor_matrix_clean,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = TRUE,
         main = "Corrélation espèces vs variables environnementales")
