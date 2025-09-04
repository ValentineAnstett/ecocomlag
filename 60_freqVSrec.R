#Import dataset 
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Macrophytes_/Data/Raw")
Macro2025 = read_xlsx("macrophytes_2025.xlsx")

#Tester si la fréquence est corrélée au recouvrement

Macro2025_clean = Macro2025 [,-c(1,3,4,22)]
colnames(Macro2025_clean) = sub("_ABS_COV$", "", colnames(Macro2025_clean))

Macro2025_clean = Macro2025_clean %>%
  filter(rowSums(select(., -LAGUNE)) > 0)

library(tidyverse)
####Reorganiser mon data frame ####
df_long = Macro2025_clean %>%
  pivot_longer(-LAGUNE, names_to = "Espece", values_to = "Recouvrement")

df_result = df_long %>%
  group_by(LAGUNE, Espece) %>%
  summarise(
    Frequence = sum(Recouvrement > 0),
    Recouvrement_Total = sum(Recouvrement),
    .groups = "drop"
  )
df_result_clean = df_result %>%
  filter(Frequence > 0 | Recouvrement_Total > 0)

print(df_result_clean)

####Relation Fréquence et Recouvrement ####


r2_df <- df_result_clean %>%
  group_by(Espece) %>%
  summarise(
    model = list(lm(Frequence ~ Recouvrement_Total)),
    r_squared = summary(model[[1]])$r.squared
  ) %>%
  mutate(
    Espece_r2 = paste0(Espece, " (R²=", round(r_squared, 2), ")")
  ) %>%
  select(Espece, Espece_r2)

# Joindre cette info au df original
df_r2 <- df_result_clean %>%
  left_join(r2_df, by = "Espece")

# Convertir en facteur pour que ggplot utilise le nouveau nom
df_r2$Espece_r2 <- factor(df_r2$Espece_r2, levels = unique(df_r2$Espece_r2))

ggplot(df_r2, aes(x = Recouvrement_Total, y = Frequence, color = Espece_r2)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Espece_r2), linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Relation entre recouvrement et fréquence par espèce",
    x = "Recouvrement (%)",
    y = "Fréquence (1 à 20) ",
    color = "Espèce (R²)"
  )

