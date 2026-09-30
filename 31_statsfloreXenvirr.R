#Import dataset Flore
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")

Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(ID_LAG %in% c("G_07", "G_06","D_04","D_05"))) #Outliers
Macro_Ptscontacts = Macro_Ptscontacts[, colSums(Macro_Ptscontacts != 0, na.rm = TRUE) > 0]


Macro_Ptscontacts_sanstot = Macro_Ptscontacts [, -14]


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

#################################################
###################### RDA ######################
#################################################
#Preparer les données 
df_merged = merge(Macro_Ptscontacts_sanstot, Data_envir, by = c("Year", "Site", "ID_LAG"))

Y = df_merged [, 4:13]  #Especes == variable a exploquer 
X = df_merged [, 14:22] #Var envirr == variables explicatives 

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



#################################################
################## LM sur TBI ###################
#################################################

#Refaire TBI ici rapidement 
Macro_T1 = Macro_Ptscontacts %>% filter(Year == 2020) %>% arrange(ID_LAG)
Macro_T2 = Macro_Ptscontacts %>% filter(Year == 2025) %>% arrange(ID_LAG)
ID_communs = intersect(Macro_T1$ID_LAG, Macro_T2$ID_LAG)
Macro_T1 = Macro_T1 %>% filter(ID_LAG %in% ID_communs) %>% arrange(ID_LAG)
Macro_T2 = Macro_T2 %>% filter(ID_LAG %in% ID_communs) %>% arrange(ID_LAG)
especes_T1 = Macro_T1 %>% dplyr::select(-Year, -Site, -ID_LAG)
especes_T2 = Macro_T2 %>% dplyr::select(-Year, -Site, -ID_LAG)
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
envir_2020 = Data_envir %>% filter(Year == 2020) %>% arrange(ID_LAG)
envir_2025 = Data_envir %>% filter(Year == 2025) %>% arrange(ID_LAG)
envir_2020 = Data_envir %>%
  filter(Year == 2020) %>%
  drop_na() %>%           # Supprime lignes avec NA
  arrange(ID_LAG)

envir_2025 = Data_envir %>%
  filter(Year == 2025) %>%
  drop_na() %>%           # Supprime lignes avec NA
  arrange(ID_LAG)

# Identifier les colonnes communes (hors Year, Site, ID_LAG)
cols_commun = intersect(
  colnames(envir_2020 %>% dplyr::select(-Year, -Site, -ID_LAG)),
  colnames(envir_2025 %>% dplyr::select(-Year, -Site, -ID_LAG))
)

# Sélectionner uniquement ces colonnes dans le même ordre
env_vars_2020 = envir_2020 %>% dplyr::select(all_of(cols_commun))
env_vars_2025 = envir_2025 %>% dplyr::select(all_of(cols_commun))


ID_envir_communs = intersect(envir_2020$ID_LAG, envir_2025$ID_LAG)
envir_2020 = envir_2020 %>% filter(ID_LAG %in% ID_envir_communs) %>% arrange(ID_LAG)
envir_2025 = envir_2025 %>% filter(ID_LAG %in% ID_envir_communs) %>% arrange(ID_LAG)
stopifnot(identical(envir_2020$ID_LAG, envir_2025$ID_LAG))
env_vars_2020 = envir_2020 %>% dplyr::select(-Year, -Site, -ID_LAG)
env_vars_2025 = envir_2025 %>% dplyr::select(-Year, -Site, -ID_LAG)

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


#### Graphs ----

#1 : uns par uns 
variables = c("duree_assec", "water_level")

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

vars_subset = c("water_level", "duree_assec")
plots = lapply(vars_subset, function(var) {
  ggplot(modele_TBI, aes_string(x = var, y = "change")) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    labs(title = paste("Relation entre", var, "et change"),
         x = var, y = "Change (TBI)") +
    theme_minimal()
})
# Afficher les 4 graphiques en 2x2
(plots[[1]] / plots[[2]])


### LM sur TBI pertes ----

modele_pertes = lm(pertes ~ ., data = df_model %>% dplyr::select(-ID_LAG, -TBI, -p_value, -gains, -change))
summary(modele_pertes)

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


corrplot(cor(df_model[, c("organic_matter", "P2O5_TOT", "C.N", "ilr_fines_vs_sand", 
                          "ilr_clay_vs_silt", "temperature", "water_level", "salinity")], 
             use = "pairwise.complete.obs"), method = "color")
library(car)
vif(modele_TBI)




#################################################
############ GLM à effets mixtes ################
#################################################
library(lmerTest)
library(sjPlot) 
library(glmmTMB)

species = c("Althenia.filiformis", "Lamprothamnium.papulosum", "Ruppia.maritima")
#Transformation SMitthson & Verkuilen (2006) : garde les proportion mais mets toutes les valeurs strictemlent entre 0 et 1 
n = nrow(df_merged)
for (sp in species) {
  df_merged[[paste0(sp, "_beta")]] <-
    (df_merged[[sp]] * (n - 1) + 0.5) / n
}
#Paramètres envir
env_vars = colnames(df_merged)[14:22]

#Site en effet aleatoire et Year en effet fixe (car que 2 niveaux)
models = list()

for (sp in species) {
  
  response = paste0(sp, "_beta")
  
  formula_beta = as.formula(
    paste(response, "~",
          paste(env_vars, collapse = " + "),
          "+ (1|Site) + Year")
  )

# Ajustement du modèle
  models[[sp]] = glmmTMB(
    formula_beta,
    data = df_merged,
    family = beta_family()
  )
}
# Résumé des résultats
lapply(models, summary)

#Graph des effets envir 
library(sjPlot)

for (sp in species) {
  print(
    plot_model(
      models[[sp]],
      type = "est",
      show.values = TRUE,
      value.offset = 0.3,
      title = paste("Effets environnementaux sur le recouvrement de", sp)
    )
  )
}


### /!\ Pas d'effet site pour Ruppia maritima == refaire le glm sans effet aleatoir pour le Site 
formula_ruppia <- as.formula(
  paste("Ruppia.maritima_beta ~",
        paste(env_vars, collapse = " + "),
        "+ Year")
)

model_ruppia <- glmmTMB(
  formula_ruppia,
  data = df_merged,
  family = beta_family()
)

summary(model_ruppia)



#Graph plot 

ggplot(model_ruppia, aes(x = NAP, y = Richness, color = Beach)) +
  geom_point() +
  geom_line(aes(y = fitted(glmm_res)))



plot_model(
  model_ruppia,
  type = "est",
  show.values = TRUE,
  value.offset = 0.3,
  title = "Effets des variables environnementales sur le recouvrement de Ruppia maritima"
)


#Graph courbes 
library(ggeffects)

get_all_preds <- function(model, env_vars, species_name) {
  
  preds <- lapply(env_vars, function(v) {
    
    p <- ggpredict(
      model,
      terms = v,
      bias_correction = TRUE
    )
    
    p$variable <- v
    p$species  <- species_name
    p
  })
  
  bind_rows(preds)
}

pred_all <- bind_rows(
  get_all_preds(models[["Althenia.filiformis"]],
                env_vars,
                "Althenia filiformis"),
  
  get_all_preds(models[["Lamprothamnium.papulosum"]],
                env_vars,
                "Lamprothamnium papulosum"),
  
  get_all_preds(model_ruppia,
                env_vars,
                "Ruppia maritima")
)

species_list <- unique(pred_all$species)

for (sp in species_list) {
  
  p <- ggplot(
    filter(pred_all, species == sp),
    aes(x = x, y = predicted)
  ) +
    geom_line(size = 1.1, color = "#5B2A86") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                alpha = 0.25, fill = "#5B2A86") +
    facet_wrap(~ variable, scales = "free_x") +
    labs(
      title = sp,
      x = NULL,
      y = "Relative suitability"
    ) +
    theme_minimal()
  
  print(p)
}






#GLMM binomial = beaucoup de zéros 

library(glmmTMB)

species <- c("Althenia.filiformis",
             "Lamprothamnium.papulosum",
             "Ruppia.maritima")

env_vars <- colnames(df_merged)[13:21]

# standardisation (très important)
df_merged[env_vars] <- scale(df_merged[env_vars])

models <- list()

for (sp in species) {
  
  response <- df_merged[[sp]]
  
  # transformation présence / absence
  df_merged[[paste0(sp, "_pa")]] <- ifelse(response > 0, 1, 0)
  
  formula_pa <- as.formula(
    paste0(paste0(sp, "_pa"),
           " ~ ",
           paste(env_vars, collapse = " + "),
           " + Year + (1|Site)")
  )
  
  models[[sp]] <- glmmTMB(
    formula_pa,
    data = df_merged,
    family = binomial()
  )
}

# résultats
lapply(models, summary)

#Plot 
ggplot(models, aes(x = NAP, y = Richness, color = Site)) +
  geom_point() +
  geom_line(aes(y = fitted(glmm_res)))
