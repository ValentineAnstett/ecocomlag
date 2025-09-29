#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Sol/Data/Processed")
Sol = read.csv("Sol_21_25.csv", header = TRUE, sep = ",", dec=",")

Sol= Sol %>%
  mutate(across(4:14, as.numeric))

Chimique = Sol [, -c(4, 8:14)]


Type_sol = Sol [, -c(5:7)]



#### PERMANOVA -----
vars_chimique = c("P2O5_TOT...","AZOTE_TOT.mg.kg.","C.N")  
vars_sol = c("MO_TOT...","CAILLOUX...","ARGILE...","LIMONS_FINS...","LIMONS_GROSSIERS...","SABLES_FIN...","SABLES_GROSSIERS...")
sites_exclus = c("BAG_GRA", "HYERES", "VILLEROY")


faire_permanova = function(data, colonnes_compo, facteurs, sites_a_exclure = NULL) {
  # Exclure sites si besoin
  data = data %>%
    filter(!(Site %in% sites_a_exclure)) %>%
    mutate(across(all_of(facteurs), as.factor))  # S'assurer que les facteurs sont bien en facteur
  
  # Sélection des variables de composition
  df_compo = data %>%
    select(all_of(colonnes_compo)) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0)) %>%
    filter(rowSums(is.na(.)) < ncol(.))
  
  # Métadonnées pour le design
  df_meta = data %>% 
    filter(rowSums(is.na(select(., all_of(colonnes_compo)))) < ncol(.)) %>%
    select(all_of(facteurs))
  
  # Calcul des distances (par exemple, euclidiennes ou Bray-Curtis selon le cas)
  distance_matrix = vegdist(df_compo, method = "euclidean")  # ou "bray"
  
  # PERMANOVA avec adonis2
  formule = as.formula(paste("distance_matrix ~", paste(facteurs, collapse = " + ")))
  
  permanova_result = adonis2(formule, data = df_meta, permutations = 999)
  
  return(permanova_result)
}


#Chimique 
resultats_permanova_chimique = faire_permanova(
  data = Chimique,
  colonnes_compo = vars_chimique,
  facteurs = c("Annee", "Site"),
  sites_a_exclure = sites_exclus
)

print(resultats_permanova_chimique)

#Structure du sol 
resultats_permanova_sol = faire_permanova(
  data = Sol,
  colonnes_compo = vars_sol,
  facteurs = c("Annee", "Site"),
  sites_a_exclure = sites_exclus
)

print(resultats_permanova_sol)
