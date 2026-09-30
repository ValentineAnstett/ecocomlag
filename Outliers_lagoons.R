# Packages
library(vegan)
library(dplyr)
library(ggplot2)

# Lecture des données
df <- read.csv("Macro_Ptscontacts.csv", check.names = FALSE)

# Variables espèces (on enlève les métadonnées)
species <- df %>%
  dplyr::select(-Year, -Site, -ID_LAG)

# Option : retirer TOT s'il s'agit simplement d'un total
species <- species %>% select(-TOT)
#remplacer NA par 0 

species[is.na(species)] <- 0

# Somme des abondances par ligne
row.sum <- rowSums(species, na.rm = TRUE)

# Combien de lignes sont vides ?
sum(row.sum == 0)

# Quels ID_LAG sont concernés ?
df$ID_LAG[row.sum == 0]
keep <- row.sum > 0

species2 <- species[keep, ]
df2 <- df[keep, ]

# Distance de Bray-Curtis
dist.bc <- vegdist(species2, method = "bray")

# NMDS
nmds <- metaMDS(
  species2,
  distance = "bray",
  k = 2,
  trymax = 100,
  autotransform = FALSE
)

# Coordonnées
scores <- as.data.frame(scores(nmds, display = "sites"))
scores$ID_LAG <- df2$ID_LAG
head(scores)

# Distance au centroïde
centre <- colMeans(scores[, c("NMDS1", "NMDS2")])

scores$distance <- sqrt(
  (scores$NMDS1 - centre[1])^2 +
    (scores$NMDS2 - centre[2])^2
)

seuil <- quantile(scores$distance, 0.95)

outliers <- scores[scores$distance > seuil, ]

outliers

print(outliers[, c("ID_LAG", "distance")])

# Graphique

ggplot(scores, aes(NMDS1, NMDS2)) +
  geom_point(colour = "grey50") +
  geom_point(data = outliers, colour = "red", size = 3) +
  geom_text(data = outliers,
            aes(label = ID_LAG),
            vjust = -0.5,
            colour = "red") +
  theme_bw()



#outliers lagoons 

# ============================================================
# Exploration des cortèges végétaux par lagune
# Objectif : repérer les lagunes au cortège atypique (espèces
# présentes seulement dans quelques lagunes) AVANT les analyses stat.
# Pas de test statistique ici, juste de l'exploration descriptive.
# ============================================================

library(tidyverse)   # manipulation de données
library(vegan)        # indices de dissimilarité écologique (Bray-Curtis)

# ---- 0. Import ----
df <- Macro_Ptscontacts

# colonnes d'espèces (on exclut Year, Site, ID_LAG, algues, TOT)
sp_cols <- setdiff(names(df), c("Year", "Site", "ID_LAG", "algues", "TOT"))

# ============================================================
# 1. Présence/absence de chaque espèce PAR LAGUNE
#    (une espèce est "présente" dans une lagune si au moins un
#    point de mesure de cette lagune a un recouvrement > 0)
# ============================================================
pres_pts <- as.data.frame(df[sp_cols] > 0)
pres_pts$Site <- df$Site

occ_lagune <- pres_pts %>%
  group_by(Site) %>%
  summarise(across(all_of(sp_cols), ~ as.integer(any(.x)))) %>%
  column_to_rownames("Site")

# Fréquence de chaque espèce = nombre de lagunes où elle est présente
freq_sp <- sort(colSums(occ_lagune))
cat("=== Fréquence des espèces (nb de lagunes où elles apparaissent) ===\n")
print(freq_sp)

# ============================================================
# 2. Espèces "rares" = restreintes à peu de lagunes
#    -> ce sont elles qui donnent un cortège "à part" à une lagune
# ============================================================
seuil <- 2   # à ajuster : "présente dans <= seuil lagunes" = espèce restreinte
sp_rares <- names(freq_sp[freq_sp > 0 & freq_sp <= seuil])

cat("\n=== Espèces restreintes à", seuil, "lagune(s) ou moins ===\n")
for (sp in sp_rares) {
  lagunes_sp <- rownames(occ_lagune)[occ_lagune[[sp]] == 1]
  cat(sprintf(" - %s : uniquement dans %s\n", sp, paste(lagunes_sp, collapse = ", ")))
}

# ============================================================
# 3. Score de "singularité" par lagune
#    = combien d'espèces rares chaque lagune héberge
# ============================================================
singularite <- occ_lagune[, sp_rares, drop = FALSE] %>%
  rowSums() %>%
  sort(decreasing = TRUE)

cat("\n=== Score de singularité par lagune (nb d'espèces rares portées) ===\n")
print(singularite)

#Par site 
site <- "D"

dat <- df %>% filter(Site == site)

species <- dat %>%
  select(-(Year:ID_LAG))

species[is.na(species)] <- 0

for(i in 1:nrow(species)){
  
  lagune <- species[i, ]
  autres <- species[-i, ]
  
  # Espèces présentes dans la lagune mais absentes partout ailleurs
  uniques <- names(lagune)[
    lagune > 0 &
      colSums(autres > 0) == 0
  ]
  
  cat("\n------------------\n")
  cat(dat$ID_LAG[i], "\n")
  print(uniques)
}

cols <- grep("Zannichell|Potamo|Ranunc", names(df), value = TRUE)

df %>%
  filter(if_any(all_of(cols), ~ . > 0)) %>%
  select(Site, ID_LAG, Year, all_of(cols))
