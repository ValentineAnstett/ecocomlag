#################################################################
##################### DATAFRAME Hydro avril ####################
################################################################
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro")
Hydro = read.csv("Hydro_Finale_19_23_25.csv", header = TRUE, sep = ",", dec=",")

Hydro = Hydro %>%
  mutate(date_releve = as.Date(date_releve, format = "%Y-%m-%d"))

##Faire data frame avec valeur unique pour chaque année

#Valeur du mois de germination 

Hydro_mois = Hydro %>%
  mutate(
    Year = year(date_releve),
    mois = month(date_releve)
  ) %>%
  filter(
    Year %in% c(2019,2020, 2025),
    mois %in% c(4, 5, 6)  # avril, mai, juin
  )
# Nombre de sites uniques par année et mois
site_counts = Hydro_mois %>%
  group_by(Year, mois) %>%
  summarise(nb_sites = n_distinct(site), .groups = "drop")
print(site_counts)

# Trouver le mois qui a le plus de sites pour chaque année
mois_max_sites = site_counts %>%
  group_by(Year) %>%
  filter(nb_sites == max(nb_sites)) %>%
  ungroup()

print("Mois optimal par année :")
print(mois_max_sites)

#Creer le dataframe pour germi


Hydro_germi <- Hydro %>%
  mutate(
    Year = year(date_releve),
    mois = month(date_releve)
  ) %>%
  filter(
    # Garder avril 2020 et avril 2025 pour tous les sites
    (Year == 2020 & mois == 4) |
      (Year == 2025 & mois == 4 & !site %in% c("Orpellieres", "La Grande Motte")) |
      # Mars 2025 uniquement pour ces sites
      (Year == 2025 & mois == 3 & site %in% c("Orpellieres", "La Grande Motte","Capelude","Chaumadou","La Palme"))
  )

Hydro_germi = Hydro_germi[,-c(3,4,5,6,13,15)]
Hydro_germi = Hydro_germi [,c(1,2,9,3:8)]
Hydro_germi = Hydro_germi %>%
  mutate(eau = if_else(tolower(eau) == "oui", 1, 0))

Hydro_germi= Hydro_germi %>%
  mutate(across(6:9, ~as.numeric(gsub(",", ".", .x))))
Hydro_germi = Hydro_germi %>%
  mutate(salinite = round(salinite, 1),
         conductivite = round(conductivite, 1)
         )

sites_a_exclure =  c("Hyeres", "Villeroy", "Salses-Leucate", "Canet", "Pissevaches")
Hydro_germi = Hydro_germi %>%
  filter(!site %in% sites_a_exclure)
lag_a_exclure =  c("PCA_CHA_02", "PCA_CHA_04", "ORB_ORP_11")
Hydro_germi = Hydro_germi %>%
  filter(!code %in% lag_a_exclure)
#Renommer PCA_CHA 
Hydro_germi = Hydro_germi %>%
  mutate(code = dplyr::recode(code,
                         "PCA_CHA_01" = "PCA_CAP_06",
                         "PCA_CHA_03" = "PCA_CAP_08",
                         "PCA_CHA_05" = "PCA_CAP_10"))

#Renommer les colonnes pour coller avec les autres frame 
Hydro_germi = Hydro_germi %>%
  mutate(Site = sub("_[^_]+$", "", code))
Hydro_germi = Hydro_germi %>%
  rename(
    Year = Year,
    ID_LAG = code,
    water_level = hauteur_eau,
    salinity = salinite,
    conductivity = conductivite
  )

Hydro_germi = Hydro_germi[,-1]

Hydro_germi = Hydro_germi[,c(2,9,1,3:8)]


#Mettre des lettres pour les sites 
Hydro_germi = Hydro_germi %>%
  mutate(Site = dplyr::recode(Site,
                       "LAP_SAL" = "A",
                       "SIG_GSA" = "B",
                       "ORB_ORP" = "C",
                       "ORB_MAI" = "D",
                       "BAG_PET" = "E",
                       "PAL_FRO" = "F",
                       "PAL_VIL" = "G",
                       "EOR_MOT" = "H",
                       "PCA_CAP" = "I",
                       "GCA_RNC" = "J",
                       "FOS_REL" = "K"))

#Adapter dans la colonne ID_LAG les nouveaux codes sites 
Hydro_germi = Hydro_germi %>%
  mutate(ID_LAG = paste0(Site, "_", sub(".*_", "", ID_LAG)))



# Importer dans processed 
write.csv(Hydro_germi, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_hydro/Hydro_germi.csv", row.names = FALSE)

#################################################################
######################### DATAFRAME Sol ########################
################################################################
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Sol/Data/Processed")
Sol = read.csv("Sol_21_25.csv", header = TRUE, sep = ",", dec=",")

#Renommer PCA_CHA 
Sol = Sol %>%
  mutate(ID_LAG = dplyr::recode(ID_LAG,
                         "PCA_CHA_01" = "PCA_CAP_06",
                         "PCA_CHA_03" = "PCA_CAP_08",
                         "PCA_CHA_05" = "PCA_CAP_10"))
Sol = Sol %>%
  filter(!(ID_LAG %in% c("PCA_CHA_02", "PCA_CHA_04","ORB_ORP_11")))

#Ajouter les SIte 
Sol = Sol %>%
  mutate(Site = sub("_[^_]+$", "", ID_LAG))

#Trier les sites
sites_a_exclure = c("BPA_PIS", "BAG_GRA", "CAN_NAZ", "HYE_VIE", "THA_SET")
Sol = Sol %>%
  filter(!(Site %in% sites_a_exclure))

# Regrouper Limons et sables 
Sol$LIMONS = Sol$LIMONS_FINS + Sol$LIMONS_GROSSIERS 

Sol$SABLES = Sol$SABLES_FIN + Sol$SABLES_GROSSIERS

#Supprimer les colonnes inutiles 
Sol = Sol[, -c(10:13)]

#Mettre 2020 à la place de 2021 pour fit les autres dataframe 
Sol$Annee[Sol$Annee == 2021] = 2020

#Mettre des lettres pour les sites 
Sol = Sol %>%
  mutate(Site = dplyr::recode(Site,
                       "LAP_SAL" = "A",
                       "SIG_GSA" = "B",
                       "ORB_ORP" = "C",
                       "ORB_MAI" = "D",
                       "BAG_PET" = "E",
                       "PAL_FRO" = "F",
                       "PAL_VIL" = "G",
                       "EOR_MOT" = "H",
                       "PCA_CAP" = "I",
                       "GCA_RNC" = "J",
                       "FOS_REL" = "K"))

#Adapter dans la colonne ID_LAG les nouveaux codes sites 
Sol = Sol %>%
  mutate(ID_LAG = paste0(Site, "_", sub(".*_", "", ID_LAG)))

Sol = Sol %>%
  rename(
    Year = Annee, 
    organic_matter = MO_TOT,
    nitrogen = AZOTE_TOT,
    clay = ARGILE,
    silt = LIMONS,
    sand = SABLES,
    P2O5 = P2O5_TOT
  )

#######Appliquer log pour texture 

# Vérifier et corriger la somme si nécessaire
Sol = Sol %>%
  mutate(across(c(clay, sand, silt), ~ .x / (clay + sand + silt))) %>%
  mutate(across(c(clay, sand, silt), ~ replace(.x, .x == 0, 1e-6)))

# Calcul ILR (3 composantes → 2 coordonnées)
ilr_mat = as.data.frame(ilr(acomp(Sol[, c("clay", "silt", "sand")])))
colnames(ilr_mat) = c("ilr_fines_vs_sand", "ilr_clay_vs_silt")

# Ajouter au dataframe original
Sol = bind_cols(Sol, ilr_mat)

####### Applique standardisation pour chimie 

## --- 2. Transformations adaptées ---
Sol_transfo <- Sol %>%
  mutate(
    #C_log      = log10(C + 1),            # Carbone (g/kg)
    N_log      = log10(nitrogen + 1),        # Azote (mg/kg)
    P2O5_log   = log10(P2O5 + 1),   # P₂O₅ (%)
    MO_sqrt    = sqrt(organic_matter), # Matière organique (%)
    CN_log     = ifelse(C.N > 0, log10(C.N), NA)  # Ratio C/N (optionnel)
  )


# Importer dans processed 
write.csv(Sol_transfo, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_sol/Sol.csv", row.names = FALSE)


#################################################################
########## DATAFRAME Variables Environnementales ################
################################################################
setdiff(Sol$ID_LAG, Hydro_germi$ID_LAG)
setdiff(Hydro_germi$ID_LAG, Sol$ID_LAG)

#Merge les tableaux

data_envir = merge(Sol, Hydro_germi, 
                   by = c("Year", "Site", "ID_LAG"), 
                   all = FALSE)
#verif 
any(duplicated(data_envir[, c("Year", "Site", "ID_LAG")]))

# Importer dans processed 
write.csv(data_envir, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Data_envir.csv", row.names = FALSE)


#################################################################
########### DATAFRAME Indices de présence macrophytes ##########
################################################################


###  Data 2020 ----
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Macrophytes_/Data/Raw")
Macro2020 = read_xls("MACRO_occur_03.21.xls")

Macro2020_clean = Macro2020 [,-c(2,3,4,5,34,35)]
Macro2020_clean = Macro2020_clean %>%
  dplyr::select(-ends_with("_REL_COV"))
colnames(Macro2020_clean) = sub("_ABS_COV$", "", colnames(Macro2020_clean))
Macro2020_clean = Macro2020_clean %>%
  mutate(Year = 2020)
Macro2020_clean = Macro2020_clean[, c(16, 1:15)]
Macro2020_clean = Macro2020_clean %>%
  mutate(across(-c(1, 2), ~{
    x_num <- suppressWarnings(as.numeric(trimws(.)))
    replace_na(x_num, 0)
  }))

df_long_2020 = Macro2020_clean %>%
  pivot_longer(-c(Year,LAGUNE), names_to = "Espece", values_to = "Recouvrement")

df_result_2020 = df_long_2020 %>%
  group_by(Year,LAGUNE, Espece) %>%
  summarise(
    Frequence = sum(Recouvrement > 0),
    Recouvrement_Total = sum(Recouvrement),
    .groups = "drop"
  )
print(df_result_2020)

Macro_Ptscontacts_2020 = df_result_2020 %>%
  mutate(indice_sp = Frequence / 20)

#Formater comme 2025 

Macro_Ptscontacts_2020$LAGUNE = gsub("_(\\d)$", "_0\\1", Macro_Ptscontacts_2020$LAGUNE)  # Ajouter un zéro si un seul chiffre après le dernier "_"
Macro_Ptscontacts_2020$LAGUNE = gsub("_(\\d{2})$", "_\\1", Macro_Ptscontacts_2020$LAGUNE) # Assurez-vous qu'il n'y ait pas de modification si deux chiffres
Macro_Ptscontacts_2020$LAGUNE = gsub("FOS_CAB", "FOS_REL", Macro_Ptscontacts_2020$LAGUNE) # Remplacer FOS_CAB par FOS_REL dans la colonne LAGUNE
Macro_Ptscontacts_2020$LAGUNE = gsub("BAG_GRA", "BAG_PET", Macro_Ptscontacts_2020$LAGUNE)

###  Data 2023




###  Data 2025 ----
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Macrophytes_/Data/Raw")
Macro2025 = read_xlsx("macrophytes_2025.xlsx")

Macro2025_clean = Macro2025 [,-c(3,4)]
colnames(Macro2025_clean) = sub("_ABS_COV$", "", colnames(Macro2025_clean))

df_long_2025 = Macro2025_clean %>%
  pivot_longer(-c(Year,LAGUNE), names_to = "Espece", values_to = "Recouvrement")

df_result_2025 = df_long_2025 %>%
  group_by(Year,LAGUNE, Espece) %>%
  summarise(
    Frequence = sum(Recouvrement > 0),
    Recouvrement_Total = sum(Recouvrement),
    .groups = "drop"
  )
print(df_result_2025)

Macro_Ptscontacts_2025 = df_result_2025 %>%
  mutate(indice_sp = Frequence / 20)


###  Merge les 3 frames

#Remettre dans le bon sens 
Macro_Ptscontacts_2025 = Macro_Ptscontacts_2025 %>%
  dplyr::select(Year, LAGUNE, Espece, indice_sp) %>%  
  pivot_wider(
    names_from = Espece,
    values_from = indice_sp
  )
Macro_Ptscontacts_2020 = Macro_Ptscontacts_2020 %>%
  dplyr::select(Year, LAGUNE, Espece, indice_sp) %>%  
  pivot_wider(
    names_from = Espece,
    values_from = indice_sp
  )

#Lisser les colonnes espèces 
all_cols = union(names(Macro_Ptscontacts_2020), names(Macro_Ptscontacts_2025))

add_missing_cols = function(df, all_cols) {
  missing_cols = setdiff(all_cols, names(df))
  if (length(missing_cols) > 0) {
    df[missing_cols] <- 0
  }
  df = df %>% dplyr::select(all_of(all_cols))
  return(df)
}

Macro_Ptscontacts_2020 = add_missing_cols(Macro_Ptscontacts_2020, all_cols)
Macro_Ptscontacts_2025 = add_missing_cols(Macro_Ptscontacts_2025, all_cols)

Macro_Ptscontacts_2020 = Macro_Ptscontacts_2020 %>%
  dplyr::select(1, 2, sort(names(.)[-c(1, 2)]))
Macro_Ptscontacts_2025 = Macro_Ptscontacts_2025 %>%
  dplyr::select(1, 2, sort(names(.)[-c(1, 2)]))

#Megre les 2 frames 
Macro_Ptscontacts = bind_rows(Macro_Ptscontacts_2020, Macro_Ptscontacts_2025)

#Renommer PCA_CHA 
Macro_Ptscontacts = Macro_Ptscontacts %>%
  mutate(LAGUNE = recode(LAGUNE,
                       "PCA_CHA_01" = "PCA_CAP_06",
                       "PCA_CHA_03" = "PCA_CAP_08",
                       "PCA_CHA_05" = "PCA_CAP_10"))

Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(LAGUNE %in% c("PCA_CHA_02", "PCA_CHA_04")))

#Ajouter les SIte 
Macro_Ptscontacts = Macro_Ptscontacts %>%
  mutate(Site = sub("_[^_]+$", "", LAGUNE))
Macro_Ptscontacts = Macro_Ptscontacts[, c(1, 21, 2:20)]

#Trier les sites

sites_a_exclure = c("BPA_PIS", "BAG_GRA", "CAN_NAZ", "HYE_VIE", "THA_SET")
Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(Site %in% sites_a_exclure))

#Renommer les colonnes pour coller avec les autres frame 
Macro_Ptscontacts = Macro_Ptscontacts %>%
  rename(
    Year = Year,
    ID_LAG = LAGUNE
  )

#Mettre des lettres pour les sites 
Macro_Ptscontacts = Macro_Ptscontacts %>%
  mutate(Site = recode(Site,
                       "LAP_SAL" = "A",
                       "SIG_GSA" = "B",
                       "ORB_ORP" = "C",
                       "ORB_MAI" = "D",
                       "BAG_PET" = "E",
                       "PAL_FRO" = "F",
                       "PAL_VIL" = "G",
                       "EOR_MOT" = "H",
                       "PCA_CAP" = "I",
                       "GCA_RNC" = "J",
                       "FOS_REL" = "K"))
#Adapter dans la colonne ID_LAG les nouveaux codes sites 
Macro_Ptscontacts = Macro_Ptscontacts %>%
  mutate(ID_LAG = paste0(Site, "_", sub(".*_", "", ID_LAG)))

#Enregistrer
write.csv(Macro_Ptscontacts, file = "/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro/Macro_Ptscontacts.csv", row.names = FALSE)





####Corrélation des variables ----
# --- Données
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/")
data_envir = read.csv("Data_envir.csv", header = TRUE, sep = ",", dec = ",")

data_envir = data_envir %>%
  mutate(across(-c(Site, ID_LAG), ~ as.numeric(as.character(.))))

vars_env = data_envir %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::select(-any_of("Year")) %>%
  dplyr::select(where(~ sd(., na.rm = TRUE) > 0))

# --- Calcul du VIF
vif_result = vifstep(vars_env, th = 10)
vars_env_vif = exclude(vars_env, vif_result)
vif_table = vif_result@results
vif_values = vif_table$VIF
names(vif_values) = vif_table$Variables

# --- Matrice de corrélation
cor_matrix = cor(vars_env_vif, use = "pairwise.complete.obs")

# --- Ordre cohérent entre VIF et variables du corrplot
vif_values = vif_values[colnames(cor_matrix)]

#Corrplot
par(mar = c(1, 1, 6, 1))  # marge inférieure augmentée pour tout placer au-dessus
corrplot(cor_matrix, method = "color", type = "lower",
         addCoef.col = "black", tl.col = "black",
         tl.cex = 0.8, number.cex = 0.7,
         col = colorRampPalette(c("#4575B4", "white", "#D73027"))(200),
         diag = FALSE)

#Ajout VIF
n <- ncol(cor_matrix)
x_positions <- seq(1, n)
par(xpd = TRUE)
y_base = n + 0.2 
text(x = mean(x_positions), y = y_base + 0.9, 
     labels = "VIF (Variance Inflation Factor)", cex = 1, font = 2)
vif_min = floor(min(vif_values))
vif_max = ceiling(max(vif_values))
vif_breaks = pretty(c(vif_min, vif_max), n = 5)

segments(x0 = 1, x1 = n, y0 = y_base + 0.65, col = "gray40", lwd = 1)
for (i in seq_along(vif_breaks)) {
  xpos <- 1 + (i - 1) * (n - 1) / (length(vif_breaks) - 1)
  segments(x0 = xpos, y0 = y_base + 0.65, y1 = y_base + 0.68, col = "gray40", lwd = 1)
  text(xpos, y_base + 0.78, labels = vif_breaks[i], cex = 0.7)
}
text(x = x_positions, y = rep(y_base + 0.4, n),
     labels = round(vif_values, 2),
     cex = 0.8, font = 2)


#autres graphs 
# Format long pour ggplot
df_env_long = data_envir %>%
  pivot_longer(cols = -c(Site, ID_LAG), names_to = "variable", values_to = "valeur")

# Histogrammes globaux
ggplot(df_env_long, aes(x = valeur)) +
  geom_histogram(bins = 30, fill = "#69b3a2", color = "white") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution (globale) des variables environnementales")

# Boxplots par site
ggplot(df_env_long, aes(x = Site, y = valeur, fill = Site)) +
  geom_boxplot(outlier.color = "red", alpha = 0.7) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution des variables environnementales par site") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = "none")





