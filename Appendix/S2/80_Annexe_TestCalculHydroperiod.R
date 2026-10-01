# =============================================================================
# Comparaison de deux méthodes de calcul des hydropériodes de lagunes
# Tests : Friedman | ICC | Bland-Altman
# Auteur  : Anstett Valentine (+Claude)
# Date    : 11/05/2025
# =============================================================================

# --- 1. Packages nécessaires -------------------------------------------------
# install.packages(c("irr", "ggplot2", "dplyr", "patchwork"))
library(irr)
library(ggplot2)
library(dplyr)
library(patchwork)

# =============================================================================
# 2. IMPORT DES DONNÉES
# =============================================================================
data_raw <- hydroperiode_2025_testannexe

cat("=== Données brutes ===\n")
print(data_raw)

# =============================================================================
# 3. CONVERSION EN JOURS DEPUIS LE T0 (1er mai)
#
#   - drying_PC et drying_copernicus  : déjà en jours (entiers) → rien à faire
#   - flooding_PC et flooding_copernicus : format AAAA-MM-JJ → à convertir
#
# Règle T0 : 1er mai de la même année si date >= 1er mai,
#            sinon 1er mai de l'année précédente.
# =============================================================================
date_to_days <- function(date_chr) {
  d  <- as.Date(date_chr)
  yr <- as.integer(format(d, "%Y"))
  t0 <- as.Date(paste0(yr, "-05-01"))
  t0 <- as.Date(ifelse(d < t0,
                       paste0(yr - 1L, "-05-01"),
                       paste0(yr,      "-05-01")))
  as.integer(d - t0)
}

data_days <- data_raw %>%
  mutate(
    flooding_PC_j         = date_to_days(flooding_PC),
    flooding_copernicus_j = date_to_days(flooding_copernicus),
    drying_PC_j           = drying_PC,          # déjà en jours
    drying_copernicus_j   = drying_copernicus    # déjà en jours
  ) %>%
  mutate(
    duration_PC         = flooding_PC_j         - drying_PC_j,
    duration_copernicus = flooding_copernicus_j - drying_copernicus_j
  )

cat("\n=== Données converties (jours depuis le 1er mai) ===\n")
print(data_days %>% dplyr::select(ID_LAG,
                           drying_PC_j, drying_copernicus_j,
                           flooding_PC_j, flooding_copernicus_j,
                           duration_PC, duration_copernicus))

# =============================================================================
# 4. TEST DE FRIEDMAN
# =============================================================================
friedman_run <- function(m1, m2, label) {
  mat <- cbind(m1, m2)
  ft  <- friedman.test(mat)
  cat(sprintf("\n--- Friedman : %s ---\n", label))
  cat(sprintf("  Chi2(%.0f) = %.4f  |  p = %.4f  %s\n",
              ft$parameter, ft$statistic, ft$p.value,
              ifelse(ft$p.value > 0.05,
                     "--> pas de difference significative (p > 0.05)",
                     "--> difference significative (p <= 0.05)")))
  invisible(ft)
}

cat("\n========== TEST DE FRIEDMAN ==========\n")
ft_dry <- friedman_run(data_days$drying_PC_j,   data_days$drying_copernicus_j,   "Mise en assec")
ft_fld <- friedman_run(data_days$flooding_PC_j, data_days$flooding_copernicus_j, "Mise en eau")
ft_dur <- friedman_run(data_days$duration_PC,   data_days$duration_copernicus,   "Duree d assec")

# =============================================================================
# 5. ICC — Two-way random, accord absolu, mesure unique
#    Seuils (Koo & Mae, 2016) : <0.50 Faible | 0.50-0.75 Modere |
#                                0.75-0.90 Bon | >=0.90 Excellent
# =============================================================================
icc_run <- function(m1, m2, label) {
  mat <- data.frame(PC = m1, Copernicus = m2)
  ic  <- icc(mat, model = "twoway", type = "agreement", unit = "single")
  interp <- dplyr::case_when(
    ic$value < 0.50 ~ "Faible",
    ic$value < 0.75 ~ "Modere",
    ic$value < 0.90 ~ "Bon",
    TRUE             ~ "Excellent"
  )
  cat(sprintf("\n--- ICC : %s ---\n", label))
  cat(sprintf("  ICC = %.4f  [IC 95%% : %.4f - %.4f]  p = %.4f  --> %s\n",
              ic$value, ic$lbound, ic$ubound, ic$p.value, interp))
  invisible(ic)
}

cat("\n========== ICC ==========\n")
icc_dry <- icc_run(data_days$drying_PC_j,   data_days$drying_copernicus_j,   "Mise en assec")
icc_fld <- icc_run(data_days$flooding_PC_j, data_days$flooding_copernicus_j, "Mise en eau")
icc_dur <- icc_run(data_days$duration_PC,   data_days$duration_copernicus,   "Duree d assec")

# =============================================================================
# 6. GRAPHIQUES DE BLAND-ALTMAN
# =============================================================================
ba_data <- function(m1, m2, label) {
  moy  <- (m1 + m2) / 2
  diff <- m1 - m2
  bias <- mean(diff, na.rm = TRUE)
  sd_d <- sd(diff,  na.rm = TRUE)
  data.frame(
    ID     = data_days$ID_LAG,
    mean   = moy,
    diff   = diff,
    label  = label,
    bias   = bias,
    loa_up = bias + 1.96 * sd_d,
    loa_lo = bias - 1.96 * sd_d
  )
}

ba_dry <- ba_data(data_days$drying_PC_j,   data_days$drying_copernicus_j,   "Mise en assec")
ba_fld <- ba_data(data_days$flooding_PC_j, data_days$flooding_copernicus_j, "Mise en eau")
ba_dur <- ba_data(data_days$duration_PC,   data_days$duration_copernicus,   "Duree d'assec")

plot_ba <- function(df) {
  ggplot(df, aes(x = mean, y = diff)) +
    geom_hline(yintercept = 0,         colour = "grey70", linewidth = 0.5, linetype = "dotted") +
    geom_hline(aes(yintercept = bias),   colour = "#E84855", linewidth = 1) +
    geom_hline(aes(yintercept = loa_up), colour = "#F4A261", linewidth = 0.8, linetype = "dashed") +
    geom_hline(aes(yintercept = loa_lo), colour = "#F4A261", linewidth = 0.8, linetype = "dashed") +
    geom_point(size = 3, colour = "#2E86AB", alpha = 0.85) +
    geom_text(aes(label = ID), size = 2.5, vjust = -0.7, colour = "grey30") +
    annotate("text", x = Inf, y = df$bias[1],
             label = paste0("Biais = ", round(df$bias[1], 1), " j"),
             hjust = 1.05, vjust = -0.5, colour = "#E84855", size = 3.2) +
    annotate("text", x = Inf, y = df$loa_up[1],
             label = paste0("+1,96 SD = ", round(df$loa_up[1], 1), " j"),
             hjust = 1.05, vjust = -0.5, colour = "#F4A261", size = 3.2) +
    annotate("text", x = Inf, y = df$loa_lo[1],
             label = paste0("-1,96 SD = ", round(df$loa_lo[1], 1), " j"),
             hjust = 1.05, vjust = 1.4, colour = "#F4A261", size = 3.2) +
    labs(
      title    = df$label[1],
      subtitle = "PC - Copernicus  (jours depuis le 1er mai)",
      x        = "Moyenne des deux methodes (jours)",
      y        = "Difference  PC - Copernicus (jours)"
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(colour = "grey40"),
      plot.margin   = margin(10, 50, 10, 10)
    )
}

p1 <- plot_ba(ba_dry)
p2 <- plot_ba(ba_fld)
p3 <- plot_ba(ba_dur)

fig <- (p1 | p2) / p3 +
  plot_annotation(
    title   = "Comparaison methodes PC vs Copernicus — Bland-Altman",
    caption = "Rouge : biais moyen  |  Orange tirete : limites d'accord a 95 % (±1,96 SD)",
    theme   = theme(
      plot.title   = element_text(size = 14, face = "bold"),
      plot.caption = element_text(colour = "grey40")
    )
  )

print(fig)
