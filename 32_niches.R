# =============================================================================
# ANALYSE DES NICHES ÉCOLOGIQUES
# Données : df_merged_final.csv
# Auteur   : script généré pour analyse communautés + niches
# =============================================================================

# ── 0. Packages ──────────────────────────────────────────────────────────────
packages <- c("vegan", "ggplot2", "tidyr", "dplyr", "ecospat",
              "corrplot", "RColorBrewer", "ggrepel", "patchwork",
              "reshape2", "mgcv", "gratia")
library(vegan)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ecospat)
library(corrplot)
library(RColorBrewer)
library(ggrepel)
library(patchwork)
library(reshape2)
library(mgcv)
library(gratia)

# ── 1. Import & préparation des données ──────────────────────────────────────
df <- read.csv("df_merged_final.csv", stringsAsFactors = FALSE)
df = df_merged_final
# Colonnes espèces (présence/absence ou abondance)
sp_cols <- c("algues", "Althenia.filiformis", "Chara.canescens",
             "Lamprothamnium.papulosum", "Riella.helicophylla",
             "Riella.notarisii", "Ruppia.cirrhosa", "Ruppia.maritima",
             "Tolypella.hispanica", "Tolypella.salina")

# Variables environnementales
env_cols <- c("salinity", "water_level", "organic_matter",
              "ilr_fines_vs_sand", "ilr_clay_vs_silt",
              "Surface", "dist_trait_cote_m",
              "mise_en_eau_num", "duree_assec")

# Retirer les espèces avec 0 présence (Tolypella.hispanica) et rares (<3 occ)
sp_occ <- colSums(df[, sp_cols] > 0, na.rm = TRUE)
print("Occurrences par espèce :")
print(sp_occ)

sp_keep <- names(sp_occ[sp_occ > 3])   # seuil min 3 occurrences
cat("\nEspèces conservées :", paste(sp_keep, collapse = ", "), "\n")

# Matrices travail
Y  <- as.data.frame(df[, sp_keep])          # matrice espèces
# ── CORRECTIF : NA → 0 dans la matrice espèces ──────────────────────────────
# (Ruppia.maritima et Tolypella.salina ont quelques NA)
Y[is.na(Y)] <- 0

X  <- as.data.frame(df[, env_cols])
X  <- X %>% mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Noms courts pour les graphiques
sp_labels <- gsub("\\.", " ", sp_keep)
env_labels <- c("Salinité", "Niveau eau", "Mat. org.", "Fines/Sable",
                "Argile/Limon", "Surface", "Dist. côte",
                "Mise en eau", "Durée assec")


# =============================================================================
# PARTIE 1 : STRUCTURE DES COMMUNAUTÉS
# =============================================================================

cat("\n\n=== PARTIE 1 : STRUCTURE DES COMMUNAUTÉS ===\n")

# ── 1.1 Corrélations entre variables environnementales ───────────────────────
#pdf("figures_communautes.pdf", width = 14, height = 12)

par(mar = c(2, 2, 3, 2))
cor_env <- cor(X, use = "complete.obs", method = "spearman")
colnames(cor_env) <- rownames(cor_env) <- env_labels
corrplot(cor_env, method = "ellipse", type = "upper",
         tl.cex = 0.9, addCoef.col = "black", number.cex = 0.7,
         title = "Corrélations de Spearman entre variables environnementales",
         mar = c(0, 0, 2, 0))

# ── 1.2 Heatmap présence/absence × sites ─────────────────────────────────────
Y_pa    <- decostand(Y, method = "pa")

empty_rows <- rowSums(Y_pa) == 0
Y_pa_nz <- Y_pa[!empty_rows, ]

row_ord <- hclust(vegdist(Y_pa_nz, method = "jaccard"))
col_ord <- hclust(dist(t(Y_pa_nz)))

# Construire les labels AVANT le melt
site_labels <- paste(df$Site[!empty_rows],
                     df$ID_LAG[!empty_rows],
                     df$Year[!empty_rows], sep = "_")
rownames(Y_pa_nz) <- site_labels

# Melt avec rownames comme id
Y_long <- reshape2::melt(as.matrix(Y_pa_nz),
                         varnames = c("site_id", "espece"),
                         value.name = "presence")

# Appliquer l'ordre du clustering
Y_long$site_id <- factor(Y_long$site_id,
                         levels = site_labels[row_ord$order])
Y_long$espece  <- factor(Y_long$espece,
                         levels = colnames(Y_pa_nz)[col_ord$order])

p_heat <- ggplot(Y_long, aes(x = espece, y = site_id, fill = factor(presence))) +
  geom_tile(colour = "grey80", linewidth = 0.2) +
  scale_fill_manual(values = c("0" = "#f0f0f0", "1" = "#2c7bb6"),
                    labels = c("Absent", "Présent")) +
  scale_x_discrete(labels = gsub("\\.", " ", levels(Y_long$espece))) +
  labs(title = "Heatmap présence/absence (sites × espèces)",
       x = NULL, y = "Site × Année", fill = NULL) +
  theme_minimal(base_size = 9) +
  theme(axis.text.y  = element_text(size = 5),
        axis.text.x  = element_text(angle = 35, hjust = 1, size = 8),
        legend.position = "top")

print(p_heat)

# ── 1.3 NMDS (Bray-Curtis) ──────────────────────────────────────────────────
# -----------------------------------------------------------------------------
# Nettoyage de la matrice avant nMDS
# -----------------------------------------------------------------------------

# 1. Supprimer les lignes entièrement nulles (aucune pousse = pas d'info compositionnelle)
row_sums <- rowSums(Y_pa, na.rm = TRUE)
cat("Lignes vides supprimées :", sum(row_sums == 0), "sur", nrow(Y_pa), "\n")

Y_nmds  <- Y_pa[row_sums > 0, ]
df_nmds <- df[row_sums > 0, ]   # conserver les métadonnées alignées

# 2. Vérifier qu'il ne reste plus de NA dans la matrice
Y_nmds <- as.matrix(Y_nmds)
Y_nmds[is.na(Y_nmds)] <- 0

# 3. Recalculer la distance et vérifier l'absence de NaN
dist_check <- vegdist(Y_nmds, method = "bray")
cat("NaN dans la distance :", sum(is.nan(as.vector(dist_check))), "\n")


# Utiliser le même masque que pour Y_nmds
X_nmds <- X[row_sums > 0, ]



#NMDS
set.seed(42)
nmds <- metaMDS(Y_nmds, distance = "bray", k = 2, trymax = 200,
                autotransform = FALSE, trace = FALSE)
cat("NMDS stress :", round(nmds$stress, 3), "\n")

# Fit environnemental
ef <- envfit(nmds, X_nmds, permutations = 999, na.rm = TRUE)
env_scores <- as.data.frame(scores(ef, "vectors"))
env_scores$variable <- env_labels
env_scores$r2 <- ef$vectors$r
env_scores$pval <- ef$vectors$pvals

# Data NMDS points
nmds_pts <- as.data.frame(scores(nmds, "sites"))
nmds_pts$Site  <- df_nmds$Site
nmds_pts$Year  <- as.factor(df_nmds$Year)
nmds_pts$rich  <- rowSums(Y_nmds)

# Scores espèces
sp_scores <- as.data.frame(scores(nmds, "species"))
sp_scores$espece <- gsub("\\.", " ", rownames(sp_scores))

p_nmds <- ggplot(nmds_pts, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = Site, shape = Year, size = rich), alpha = 0.7) +
  geom_text(data = sp_scores,
            aes(x = NMDS1, y = NMDS2, label = espece),
            colour = "#d73027", size = 3, fontface = "italic") +
  geom_segment(data = subset(env_scores, pval < 0.05),
               aes(x = 0, y = 0, xend = NMDS1 * 0.5, yend = NMDS2 * 0.5),
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "black", linewidth = 0.7) +
  geom_text_repel(data = subset(env_scores, pval < 0.05),
                  aes(x = NMDS1 * 0.55, y = NMDS2 * 0.55, label = variable),
                  size = 3.5, colour = "black") +
  scale_size_continuous(name = "Richesse", range = c(1, 6)) +
  annotate("text", x = Inf, y = Inf,
           label = paste("Stress =", round(nmds$stress, 3)),
           hjust = 1.1, vjust = 1.5, size = 4) +
  labs(title = "NMDS (Bray) — Structure des communautés",
       subtitle = "Vecteurs env. significatifs (p < 0.05) en noir",
       colour = "Site", shape = "Année") +
  theme_bw(base_size = 11) +
  coord_equal()
print(p_nmds)

# ── 1.4 RDA ou CCA ──────────────────────────────────────────────────────────
# CCA si espèces avec gradient long, RDA si court
# Test préliminaire : DCA pour longueur de gradient
dca <- decorana(Y_nmds)
grad_length <- dca$evals[1]
cat("Longueur du gradient DCA1 :", round(grad_length, 2), "\n")
if (grad_length > 3) {
  cat("→ Gradient long : utilisation de la CCA\n")
  ord_constrained <- cca(Y_pa ~ ., data = X)
  ord_type <- "CCA"
} else {
  cat("→ Gradient court : utilisation de la RDA\n")
  ord_constrained <- rda(Y_pa ~ ., data = X)
  ord_type <- "RDA"
}

# Test global de la CCA/RDA
set.seed(42)
perm_test <- anova(ord_constrained, permutations = 999)
cat("\nTest permutationnel global :\n")
print(perm_test)

# Test par axe
perm_axis <- anova(ord_constrained, by = "axis", permutations = 999)
cat("\nTest par axe :\n")
print(perm_axis)

# Test par variable
perm_var <- anova(ord_constrained, by = "terms", permutations = 999)
cat("\nTest par variable :\n")
print(perm_var)

# Graphique CCA/RDA
sc_sp   <- as.data.frame(scores(ord_constrained, display = "species",  scaling = 2))
sc_site <- as.data.frame(scores(ord_constrained, display = "sites",    scaling = 2))
sc_env  <- as.data.frame(scores(ord_constrained, display = "bp",       scaling = 2))
sc_sp$espece <- gsub("\\.", " ", rownames(sc_sp))
sc_env$variable <- env_labels
sc_site$Site <- df$Site
sc_site$Year <- as.factor(df$Year)
ax_names <- paste0(ord_type, c("1", "2"))
colnames(sc_sp)[1:2]   <- ax_names
colnames(sc_site)[1:2] <- ax_names
colnames(sc_env)[1:2]  <- ax_names

scale_arrow <- 1.2
p_cca <- ggplot(sc_site, aes_string(x = ax_names[1], y = ax_names[2])) +
  geom_point(aes(colour = Site, shape = Year), alpha = 0.6, size = 2) +
  geom_text(data = sc_sp,
            aes_string(x = ax_names[1], y = ax_names[2], label = "espece"),
            colour = "#d73027", size = 3.2, fontface = "italic") +
  geom_segment(data = sc_env,
               aes_string(x = 0, y = 0,
                          xend = paste0(ax_names[1], " * scale_arrow"),
                          yend = paste0(ax_names[2], " * scale_arrow")),
               arrow = arrow(length = unit(0.25, "cm")),
               colour = "black", linewidth = 0.6) +
  geom_text_repel(data = sc_env,
                  aes_string(x = paste0(ax_names[1], " * scale_arrow * 1.1"),
                             y = paste0(ax_names[2], " * scale_arrow * 1.1"),
                             label = "variable"),
                  size = 3.2) +
  labs(title = paste(ord_type, "contrainte — espèces × environnement"),
       subtitle = paste("Gradient DCA1 =", round(grad_length, 2)),
       colour = "Site", shape = "Année") +
  theme_bw(base_size = 11) +
  coord_equal()
print(p_cca)

dev.off()
cat("→ Figures communautés sauvegardées dans figures_communautes.pdf\n")


# =============================================================================
# PARTIE 2 : NICHES ÉCOLOGIQUES PAR ESPÈCE
# =============================================================================

cat("\n\n=== PARTIE 2 : NICHES ÉCOLOGIQUES ===\n")

#pdf("figures_niches.pdf", width = 14, height = 10)

# ── 2.1 Optima & tolérances par Weighted Averaging (WA) ─────────────────────
wa_results <- data.frame()

for (sp in sp_keep) {
  occ_idx <- which(df[[sp]] > 0)
  if (length(occ_idx) < 3) next
  for (ev in seq_along(env_cols)) {
    ev_name  <- env_cols[ev]
    ev_vals  <- X[[ev_name]]
    weights  <- df[[sp]]     # utilise abondance/PA comme poids
    # Optimum = moyenne pondérée
    opt  <- weighted.mean(ev_vals, w = weights, na.rm = TRUE)
    # Tolérance = écart-type pondéré (mesure largeur de niche)
    tol  <- sqrt(weighted.mean((ev_vals - opt)^2, w = weights, na.rm = TRUE))
    wa_results <- rbind(wa_results,
                        data.frame(espece = gsub("\\.", " ", sp),
                                   variable = env_labels[ev],
                                   optimum = opt,
                                   tolerance = tol))
  }
}

# Optima standardisés pour comparaison entre espèces
X_scaled <- as.data.frame(scale(X))
wa_std <- data.frame()
for (sp in sp_keep) {
  occ_idx <- which(df[[sp]] > 0)
  if (length(occ_idx) < 3) next
  for (ev in seq_along(env_cols)) {
    ev_name <- env_cols[ev]
    ev_vals <- X_scaled[[ev_name]]
    weights <- df[[sp]]
    opt <- weighted.mean(ev_vals, w = weights, na.rm = TRUE)
    tol <- sqrt(weighted.mean((ev_vals - opt)^2, w = weights, na.rm = TRUE))
    wa_std <- rbind(wa_std,
                    data.frame(espece = gsub("\\.", " ", sp),
                               variable = env_labels[ev],
                               opt_std = opt,
                               tol_std = tol))
  }
}

# Graphique optima ± tolérance (variables standardisées)
p_wa <- ggplot(wa_std, aes(x = variable, y = opt_std, colour = espece)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbar(aes(ymin = opt_std - tol_std, ymax = opt_std + tol_std),
                position = position_dodge(width = 0.6), width = 0.3, linewidth = 0.7) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  scale_colour_brewer(palette = "Dark2") +
  labs(title = "Niches écologiques - Optima ± Tolérance (Weighted Averaging)",
       subtitle = "Variables standardisées (z-scores) - erreurs = 1 tolérance",
       x = NULL, y = "Valeur standardisée", colour = "Espèce") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 9))
print(p_wa)

# ── 2.2 Réponses individuelles (GAM binomial) par variable clé ───────────────
# Sélection des variables clés (forte corrélation avec communautés)
key_vars <- c("salinity", "water_level", "duree_assec",
              "organic_matter", "mise_en_eau_num")
key_labs <- c("Salinité", "Niveau eau", "Durée assec",
              "Mat. org.", "Mise en eau")

for (kv in seq_along(key_vars)) {
  ev_name <- key_vars[kv]
  ev_lab  <- key_labs[kv]
  plots_list <- list()
  
  for (sp in sp_keep) {
    pa  <- as.integer(df[[sp]] > 0)
    ev  <- X[[ev_name]]
    ok  <- complete.cases(pa, ev)
    if (sum(pa[ok]) < 3) next
    
    dat_gam <- data.frame(pa = pa[ok], ev = ev[ok])
    # GAM avec spline
    tryCatch({
      m <- gam(pa ~ s(ev, k = 4), data = dat_gam, family = binomial,
               method = "REML")
      x_pred <- seq(min(dat_gam$ev), max(dat_gam$ev), length.out = 200)
      pred   <- predict(m, newdata = data.frame(ev = x_pred),
                        type = "response", se.fit = TRUE)
      pred_df <- data.frame(
        x    = x_pred,
        fit  = pred$fit,
        lo   = pred$fit - 1.96 * pred$se.fit,
        hi   = pred$fit + 1.96 * pred$se.fit
      )
      pred_df$lo <- pmax(pred_df$lo, 0)
      pred_df$hi <- pmin(pred_df$hi, 1)
      
      sp_lab <- gsub("\\.", " ", sp)
      p <- ggplot(pred_df, aes(x = x, y = fit)) +
        geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#74c2e1", alpha = 0.4) +
        geom_line(colour = "#1d6fa4", linewidth = 1) +
        geom_rug(data = dat_gam[dat_gam$pa == 1, ],
                 aes(x = ev, y = NULL), colour = "#d73027",
                 sides = "t", alpha = 0.7) +
        geom_rug(data = dat_gam[dat_gam$pa == 0, ],
                 aes(x = ev, y = NULL), colour = "grey50",
                 sides = "b", alpha = 0.4) +
        labs(title = sp_lab, x = ev_lab, y = "P(présence)") +
        ylim(0, 1) +
        theme_bw(base_size = 10)
      plots_list[[sp_lab]] <- p
    }, error = function(e) {
      cat("  GAM impossible pour", sp, "~", ev_name, ":", conditionMessage(e), "\n")
    })
  }
  
  if (length(plots_list) > 0) {
    combined <- wrap_plots(plots_list, ncol = 3) +
      plot_annotation(
        title = paste("Réponse GAM à :", ev_lab),
        subtitle = "IC 95% — rug rouge = présences, gris = absences",
        theme = theme(plot.title = element_text(size = 14, face = "bold"))
      )
    print(combined)
  }
}
#Test 
key_vars <- c("duree_assec", "water_level", "salinity")
key_labs <- c("Durée assec", "Niveau eau", "Salinité")

sp_subset <- c("Althenia.filiformis", "Lamprothamnium.papulosum", "Ruppia.maritima")

all_plots <- list()

for (kv in seq_along(key_vars)) {
  ev_name <- key_vars[kv]
  ev_lab  <- key_labs[kv]
  
  for (sp in sp_subset) {
    pa  <- as.integer(df[[sp]] > 0)
    ev  <- X[[ev_name]]
    ok  <- complete.cases(pa, ev)
    if (sum(pa[ok]) < 3) next
    
    dat_gam <- data.frame(pa = pa[ok], ev = ev[ok])
    tryCatch({
      m <- gam(pa ~ s(ev, k = 4), data = dat_gam, family = binomial,
               method = "REML")
      x_pred <- seq(min(dat_gam$ev), max(dat_gam$ev), length.out = 200)
      pred   <- predict(m, newdata = data.frame(ev = x_pred),
                        type = "response", se.fit = TRUE)
      pred_df <- data.frame(
        x   = x_pred,
        fit = pred$fit,
        lo  = pmax(pred$fit - 1.96 * pred$se.fit, 0),
        hi  = pmin(pred$fit + 1.96 * pred$se.fit, 1)
      )
      
      sp_lab <- gsub("\\.", " ", sp)
      p <- ggplot(pred_df, aes(x = x, y = fit)) +
        geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#74c2e1", alpha = 0.4) +
        geom_line(colour = "#1d6fa4", linewidth = 1) +
        geom_rug(data = dat_gam[dat_gam$pa == 1, ],
                 aes(x = ev, y = NULL), colour = "#d73027",
                 sides = "t", alpha = 0.7) +
        geom_rug(data = dat_gam[dat_gam$pa == 0, ],
                 aes(x = ev, y = NULL), colour = "grey50",
                 sides = "b", alpha = 0.4) +
        labs(title = sp_lab, x = ev_lab, y = "P(présence)") +
        ylim(0, 1) +
        theme_bw(base_size = 10)
      
      all_plots[[paste(ev_name, sp, sep = "_")]] <- p
    }, error = function(e) {
      cat("  GAM impossible pour", sp, "~", ev_name, ":", conditionMessage(e), "\n")
    })
  }
}

# Planche 3×3 : lignes = variables, colonnes = espèces
combined <- wrap_plots(all_plots, ncol = 3, nrow = 3) +
  plot_annotation(
    title    = "Réponses GAM — espèces × variables environnementales",
    subtitle = "IC 95% — rug rouge = présences, gris = absences",
    theme    = theme(plot.title = element_text(size = 14, face = "bold"))
  )

print(combined)


# ── 2.3 Ellipses de niche dans l'espace NMDS ─────────────────────────────────
nmds_pts2 <- as.data.frame(scores(nmds, "sites"))
colnames(nmds_pts2) <- c("NMDS1", "NMDS2")
nmds_pts2$Site <- df$Site
nmds_pts2$Year <- as.factor(df$Year)

# Colorier le fond par richesse
nmds_pts2$richesse <- rowSums(Y_pa)

plots_ellipses <- list()
for (sp in sp_keep) {
  pa_sp <- as.integer(df[[sp]] > 0)
  if (sum(pa_sp) < 3) next
  nmds_pts2$presence <- factor(pa_sp, levels = c(0, 1),
                               labels = c("Absent", "Présent"))
  sp_lab <- gsub("\\.", " ", sp)
  
  p_ell <- ggplot(nmds_pts2, aes(x = NMDS1, y = NMDS2)) +
    geom_point(aes(colour = presence, size = presence), alpha = 0.65) +
    stat_ellipse(data = subset(nmds_pts2, presence == "Présent"),
                 aes(x = NMDS1, y = NMDS2),
                 type = "t", level = 0.95,
                 colour = "#d73027", linewidth = 1) +
    scale_colour_manual(values = c("Absent" = "grey70", "Présent" = "#d73027")) +
    scale_size_manual(values = c("Absent" = 1, "Présent" = 3)) +
    labs(title = sp_lab, colour = NULL, size = NULL,
         x = "NMDS1", y = "NMDS2") +
    theme_bw(base_size = 9) +
    theme(legend.position = "bottom") +
    coord_equal()
  plots_ellipses[[sp_lab]] <- p_ell
}

combined_ell <- wrap_plots(plots_ellipses, ncol = 3) +
  plot_annotation(
    title = "Niches écologiques dans l'espace NMDS (Jaccard)",
    subtitle = "Ellipse à 95% autour des présences (rouge)",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )
print(combined_ell)

# ── 2.4 Heatmap synthétique des optima standardisés ──────────────────────────
wa_mat <- wa_std %>%
  select(espece, variable, opt_std) %>%
  pivot_wider(names_from = variable, values_from = opt_std) %>%
  tibble::column_to_rownames("espece")

wa_long2 <- reshape2::melt(as.matrix(wa_mat),
                           varnames = c("espece", "variable"),
                           value.name = "opt_std")

p_hm_niche <- ggplot(wa_long2, aes(x = variable, y = espece, fill = opt_std)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(opt_std, 1)), size = 3.5) +
  scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027",
                       midpoint = 0, name = "Optimum\n(z-score)") +
  labs(title = "Heatmap synthétique des optima de niche",
       subtitle = "Weighted Averaging sur variables standardisées",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        axis.text.y = element_text(face = "italic"))
print(p_hm_niche)

# ── 2.5 Tableau récapitulatif (optima en unités réelles) ─────────────────────
wa_wide <- wa_results %>%
  select(espece, variable, optimum) %>%
  pivot_wider(names_from = variable, values_from = optimum)
cat("\nOptima de niche (unités réelles) :\n")
print(wa_wide)
write.csv(wa_wide, "optima_niche.csv", row.names = FALSE)

wa_tol_wide <- wa_results %>%
  select(espece, variable, tolerance) %>%
  pivot_wider(names_from = variable, values_from = tolerance)
cat("\nTolérances de niche (largeur) :\n")
print(wa_tol_wide)
write.csv(wa_tol_wide, "tolerances_niche.csv", row.names = FALSE)

dev.off()
cat("\n→ Figures niches sauvegardées dans figures_niches.pdf\n")
cat("→ Tableaux : optima_niche.csv, tolerances_niche.csv\n")

cat("\n✓ Analyse terminée.\n")
