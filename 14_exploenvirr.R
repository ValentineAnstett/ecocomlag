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
Data_envir= read.csv("Data_envir.csv", header = TRUE, sep = ",", dec=".")

Data_envir = Data_envir %>%
  filter(!(ID_LAG %in% c("G_07", "G_06","D_04","D_05"))) #Outliers


df <- Macro_Ptscontacts_sanstot %>%
  inner_join(Data_envir, by = c("Year", "Site", "ID_LAG"))

df_species <- df %>%
  dplyr::select(
    salinity, organic_matter, C.N, duree_moyenne_jours, ilr_fines_vs_sand, P2O5_TOT, nitrogen, clay, silt, turbidite, water_level, date_mise_en_eau_premier, date_assec_premier,
    Althenia.filiformis,
    Ruppia.maritima,
    Lamprothamnium.papulosum
  )

df_species$date_mise_en_eau_premier <- substr(df_species$date_mise_en_eau_premier, 1, 2)
df_species$date_assec_premier <- substr(df_species$date_assec_premier, 1, 2)

library(tidyr)

df_long <- df_species %>%
  pivot_longer(
    cols = c(Althenia.filiformis,
             Ruppia.maritima,
             Lamprothamnium.papulosum),
    names_to = "Species",
    values_to = "Abundance"
  )

library(ggplot2)

ggplot(df_long, aes(x = salinity, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction de la salinité")


ggplot(df_long, aes(x = water_level, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction de la matière organique")

df_long <- df_long %>%
  rename(CN = C.N)
ggplot(df_long, aes(x = CN, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction du ratio C/N")

ggplot(df_long, aes(x = duree_moyenne_jours, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction de la durée moyenne de la mise en eau ")
#mise en eau 
df_long <- df_long[complete.cases(df_long[, c("Abundance", "date_mise_en_eau_premier")]), ]
df_long$date_mise_en_eau_premier <- as.numeric(as.character(df_long$date_mise_en_eau_premier))
ggplot(df_long, aes(x = date_mise_en_eau_premier, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction de la date de mise en eau ")

#assec 
df_long <- df_long[complete.cases(df_long[, c("Abundance", "date_assec_premier")]), ]
df_long$date_assec_premier <- as.numeric(as.character(df_long$date_assec_premier))

ggplot(df_long, aes(x = date_assec_premier, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction de la date d'assec ")

df_summary <- df_long %>%
  group_by(Species, date_assec_premier) %>%
  summarise(Abundance = mean(Abundance, na.rm = TRUE))

ggplot(df_summary, aes(x = date_assec_premier, y = Abundance, color = Species)) +
  geom_line(linewidth = 1) +
  geom_point() +
  theme_minimal()

ggplot(df_long, aes(x = silt, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction du ")

ggplot(df_long, aes(x = P2O5_TOT, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction du P2O5 total")

ggplot(df_long, aes(x = nitrogen, y = Abundance, color = Species)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Abondance des espèces en fonction de l'azote totale")



