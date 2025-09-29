###2020 - 2025 : date unique 
#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro_germi = read.csv("Hydro_germi.csv", header = TRUE, sep = ",", dec=".")

####PERMANOVA ---- 

Hydro_germi = Hydro_germi [, -c(5,9)]

vars_hydro = Hydro_germi %>%
  select(eau,temperature, hauteur_eau, salinite)

lignes_valides = complete.cases(vars_hydro) & !apply(vars_hydro, 1, function(x) any(is.infinite(x)))
vars_hydro_clean = vars_hydro[lignes_valides, ]
Hydro_germi_clean = Hydro_germi[lignes_valides, ]

vars_hydro = c("eau", "temperature", "hauteur_eau", "salinite")

sites_exclus = NULL #vérifier si besoin d'exclure

faire_permanova = function(data, colonnes_compo, facteurs, sites_a_exclure = NULL) {
  data = data %>%
    filter(!(Site %in% sites_a_exclure)) %>%
    mutate(across(all_of(facteurs), as.factor))  # S'assurer que les facteurs sont bien en facteur
  df_compo = data %>%
    select(all_of(colonnes_compo)) %>%
    mutate(across(everything(), as.numeric)) %>%
    select(where(~ sd(., na.rm = TRUE) > 0)) %>%
    filter(rowSums(is.na(.)) < ncol(.))
  df_meta = data %>% 
    filter(rowSums(is.na(select(., all_of(colonnes_compo)))) < ncol(.)) %>%
    select(all_of(facteurs))
  distance_matrix = vegdist(df_compo, method = "euclidean")  # ou "bray"
  
  formule = as.formula(paste("distance_matrix ~", paste(facteurs, collapse = " + ")))
  
  permanova_result = adonis2(formule, data = df_meta, permutations = 999)
  
  return(permanova_result)
}

# Effet uniquement du site
faire_permanova(
  data = Hydro_germi_clean %>% rename(Site = site, Annee = annee),
  colonnes_compo = vars_hydro,
  facteurs = c("Site")
)

# Effet uniquement de l'année
faire_permanova(
  data = Hydro_germi_clean %>% rename(Site = site, Annee = annee),
  colonnes_compo = vars_hydro,
  facteurs = c("Annee")
)

