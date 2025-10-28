#Import dataset Flore
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")

Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(ID_LAG %in% c("G_07", "G_06","D_04","D_05"))) #Outliers
Macro_Ptscontacts = Macro_Ptscontacts[, colSums(Macro_Ptscontacts != 0, na.rm = TRUE) > 0]


Macro_Ptscontacts_sanstot = Macro_Ptscontacts [, -15]


#Import dataset Envirr 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data")
Data_envir= read.csv("Data_envir.csv", header = TRUE, sep = ",", dec=".")

Data_envir = Data_envir %>%
  filter(!(ID_LAG %in% c("G_07", "G_06","D_04","D_05"))) #Outliers

#Selection des variables 
Data_envir = Data_envir %>%
  dplyr::select(-CAILLOUX, -eau, -turbidite, -conductivite, -SABLES, -AZOTE_TOT)


### RDA ----

#Preparer les données 
df_merged = merge(Macro_Ptscontacts_sanstot, Data_envir, by = c("Annee", "Site", "ID_LAG"))

Y = df_merged [, 4:14]  #Especes == variable a exploquer 
X = df_merged [, 15:22] #Var envirr == variables explicatives 

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

#### Graph ggplot ----

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
  geom_hline(yintercept = 0, color = "black") +           
  geom_vline(xintercept = 0, color = "black") +           
  xlab("RDA1") +
  ylab("RDA2") +
  ggtitle("Biplot RDA ") +
  theme_minimal(base_size = 16) +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 1))

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
pheatmap(cor_matrix_clean,
         cluster_rows = TRUE,
         cluster_cols = TRUE,
         display_numbers = TRUE,
         main = "Corrélation espèces vs variables environnementales")


###LM sur TBI global ---- 

#Refaire TBI ici rapidement 
Macro_T1 = Macro_Ptscontacts %>% filter(Annee == 2020) %>% arrange(ID_LAG)
Macro_T2 = Macro_Ptscontacts %>% filter(Annee == 2025) %>% arrange(ID_LAG)
ID_communs = intersect(Macro_T1$ID_LAG, Macro_T2$ID_LAG)
Macro_T1 = Macro_T1 %>% filter(ID_LAG %in% ID_communs) %>% arrange(ID_LAG)
Macro_T2 = Macro_T2 %>% filter(ID_LAG %in% ID_communs) %>% arrange(ID_LAG)
especes_T1 = Macro_T1 %>% dplyr::select(-Annee, -Site, -ID_LAG)
especes_T2 = Macro_T2 %>% dplyr::select(-Annee, -Site, -ID_LAG)
rownames(especes_T1) = Macro_T1$ID_LAG
rownames(especes_T2) = Macro_T2$ID_LAG
non_vides = which(rowSums(especes_T1) != 0 & rowSums(especes_T2) != 0)
especes_T1 = especes_T1[non_vides, ]
especes_T2 = especes_T2[non_vides, ]
ID_LAG_vecteur = Macro_T1$ID_LAG[non_vides]
#### Calcul du TBI ----
result = TBI(
  especes_T1,
  especes_T2,
  method = "%difference",
  nperm = 999,
  test.t.perm = FALSE
)
tbi_result = data.frame(
  ID_LAG = ID_LAG_vecteur,
  TBI = result$TBI,
  p_value = result$p.TBI,
  pertes = result$BCD.mat[, "B/(2A+B+C)"],
  gains = result$BCD.mat[, "C/(2A+B+C)"],
  change = result$BCD.mat[, "D=(B+C)/(2A+B+C)"]
)

#Preparer les donnees envir
envir_2020 = Data_envir %>% filter(Annee == 2020) %>% arrange(ID_LAG)
envir_2025 = Data_envir %>% filter(Annee == 2025) %>% arrange(ID_LAG)
envir_2020 = Data_envir %>%
  filter(Annee == 2020) %>%
  drop_na() %>%           # Supprime lignes avec NA
  arrange(ID_LAG)

envir_2025 = Data_envir %>%
  filter(Annee == 2025) %>%
  drop_na() %>%           # Supprime lignes avec NA
  arrange(ID_LAG)

ID_envir_communs = intersect(envir_2020$ID_LAG, envir_2025$ID_LAG)
envir_2020 = envir_2020 %>% filter(ID_LAG %in% ID_envir_communs) %>% arrange(ID_LAG)
envir_2025 = envir_2025 %>% filter(ID_LAG %in% ID_envir_communs) %>% arrange(ID_LAG)
stopifnot(identical(envir_2020$ID_LAG, envir_2025$ID_LAG))
env_vars_2020 = envir_2020 %>% dplyr::select(-Annee, -Site, -ID_LAG)
env_vars_2025 = envir_2025 %>% dplyr::select(-Annee, -Site, -ID_LAG)

# Calcul du delta (2025 - 2020)
delta_envir = env_vars_2025 - env_vars_2020
delta_envir$ID_LAG = envir_2020$ID_LAG

df_model = tbi_result %>%
  inner_join(delta_envir, by = "ID_LAG")

#### Calcul du LM  ----
modele_TBI = lm(change ~ ., data = df_model %>% dplyr::select(-ID_LAG, -TBI, -p_value, -pertes, -gains))
summary(modele_TBI)

par(mfrow = c(2, 2))  # 4 graphiques en 1
plot(df_model)

# Simplification du modèle
modele_simplifie = stepAIC(modele_TBI, direction = "both", trace = FALSE)
summary(modele_simplifie)
par(mfrow = c(2, 2))  # 4 graphiques en 1
plot(modele_simplifie)


#### Graphs ----

#1 : uns par uns 
variables = c("P2O5_TOT", "LIMONS", "temperature", "hauteur_eau", "salinite")

for (var in variables) {
  p = ggplot(modele_TBI, aes_string(x = var, y = "change")) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = paste("Relation entre", var, "et change"),
         x = var, y = "Change (TBI)") +
    theme_minimal()
  
  print(p)
}

#2 : les 4 significatifs ensembles 

vars_subset = c("LIMONS", "salinite", "temperature")
plots = lapply(vars_subset, function(var) {
  ggplot(modele_TBI, aes_string(x = var, y = "change")) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = paste("Relation entre", var, "et change"),
         x = var, y = "Change (TBI)") +
    theme_minimal()
})
# Afficher les 4 graphiques en 2x2
(plots[[1]] | plots[[2]]) / (plots[[3]])


### LM sur TBI pertes ----

modele_pertes = lm(pertes ~ ., data = df_model %>% dplyr::select(-ID_LAG, -TBI, -p_value, -gains, -change))
summary(modele_pertes)

# Simplification du modèle
modele_simplifie_pertes = stepAIC(modele_pertes, direction = "both", trace = FALSE)
summary(modele_simplifie_pertes)
par(mfrow = c(2, 2))  # 4 graphiques en 1
plot(modele_simplifie_pertes)

####Graph ----
vars_subset_pertes = c("LIMONS", "P2O5_TOT")
plots_pertes = lapply(vars_subset_pertes, function(var) {
  ggplot(df_model, aes_string(x = var, y = "pertes")) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = paste("Relation entre", var, "et pertes"),
         x = var, y = "Pertes (TBI)") +
    theme_minimal()
})
# Afficher les 4 graphiques en 2x2
(plots_pertes[[1]] / plots_pertes[[2]])



## GLM ----