# Main Results

This folder contains the Quarto document(s) reproducing the main analyses and the figures presented in the paper.

## Analyses

The document covers:
 
1. **Plant community structure and dynamics**
   - Hierarchical clustering of sampling plots (all years combined, Bray-Curtis dissimilarity), with a confusion matrix summarising how lagoons change cluster membership between 2020 and 2025, and indicator species analysis (IndVal) per cluster.
   - PERMANOVA testing Site and Year effects on community composition, using permutation schemes restricted to account for the repeated (paired) sampling of each lagoon across years.
   - Species richness comparison between 2020 and 2025 (paired Wilcoxon test).
   - Partial RDA of community composition against environmental variables, conditioned on Site and with permutations restricted within lagoon to account for temporal pseudo-replication.
2. **Spatial hotspots of community change (Temporal Beta-diversity Index, TBI)**
   - TBI decomposition (losses, gains, total dissimilarity) between 2020 and 2025 per lagoon.
   - Distribution checks (normality, environmental Δ variables) before model fitting.
   - Linear models of TBI components against Δ (2025 − 2020) environmental variables, restricted to lagoons with vegetation in both years, with no post-hoc variable selection.
3. **Realised niche modelling for three key species** (*Althenia filiformis*, *Ruppia maritima*, *Lamprothamnium papulosum*)
   - Cover expressed as an ordinal variable (grid of ~0.05) and modelled with ordinal GAMMs (`mgcv::gam`, `family = ocat()`), including a random intercept for `Site` to account for the sampling design.
   - Automatic variable selection via double-penalty shrinkage (`select = TRUE`): non-informative environmental terms are shrunk toward zero within a single model fit, rather than through stepwise backward elimination (which proved numerically unstable given the limited number of non-zero observations per species).
   - Partial effect plots (expected cover along each retained environmental gradient), restricted to variables not shrunk out of each species' model.
   - A complementary univariate (single-predictor) screening of species–environment relationships is also included as an exploratory cross-check; unlike the GAMM, it does not control for correlations among predictors and should be interpreted as marginal associations only.
  
   
## Figures
 
| Figure in the paper | Produced in | Output file |
|---------------------|-------------|-------------|
| Cluster dendrogram + bipartite species network | `main_results.qmd`, section "Composition (Clustering + confusion matrix + IndVal)" | `figures/figure_cluster_color.svg` |
| Species richness, 2020 vs 2025 | `main_results.qmd`, section "Species richness" | `figures/figure_richesse.svg` |
| RDA biplot (partial, Condition = Site) | `main_results.qmd`, section "Ecological drivers of composition (constrained RDA)" | `figures/figure_rda_colorblind.svg` |
| TBI distribution by site / gains vs losses (composite panel) | `main_results.qmd`, section "Spatial hotspots of community change (TBI)" | `figures/figure_tbi_panel.svg` |
| Realised niche, partial effects (GAMM, all variables) | `main_results.qmd`, section "Realised niche modelling" | `figures/GAMM_colorblind.svg` |
| Realised niche, one row per species (GAMM, shrinkage-retained variables only) | `main_results.qmd`, section "Realised niche modelling" | `figures/niche_gamm_par_espece.svg` |
 
> Figures are currently written to the working directory when the document is rendered; update the `ggsave()` calls (or move the files) to `figures/` to match the table above.

## How to reproduce
 
1. Make sure the formatted data are available and referenced via `here("df_merged_final.csv")` (adjust the path in the `setup` chunk if the file lives elsewhere, e.g. `../data/formatted/`).
2. Install the required R packages:
   `vegan`, `ggplot2`, `ggdendro`, `dplyr`, `tidyr`, `readr`, `adespatial`, `indicspecies`, `FactoMineR`, `ggrepel`, `mgcv`, `patchwork`, `pheatmap`, `here`, `permute`, `corrplot`






## Notes on methodological choices
 
- A single random seed (`set.seed(123)`) is set once at the top of the document and governs clustering, permutation tests, and model fitting throughout.
- Lagoons without any vegetation in a given year are excluded from the clustering itself (Bray-Curtis distance is undefined for two empty samples) but are retained and tracked separately in the confusion matrix and TBI distribution figure, rather than silently dropped.
- The TBI linear models are restricted to lagoons with vegetation in **both** survey years, since dissimilarity is undefined (or reflects total colonisation/loss rather than a compositional shift) otherwise.
- The niche models were initially attempted as multivariate GLMMs (binomial, quadratic terms, backward selection by AIC/BIC), but this approach was numerically unstable given the sample size per species (convergence failures, extreme coefficients). The ordinal GAMM with shrinkage selection reproduces the same variable-selection logic within a single, more stable model fit, and better reflects the flexible (non-parabolic) shape a realised niche can take.
