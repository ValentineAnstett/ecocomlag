#Import dataset
getwd()
setwd("/home/anstett/Documents/LTM-Flora/Analyses_stats/Analyse_Globale/Data/Processed_Macro")
Macro_Ptscontacts= read.csv("Macro_Ptscontacts.csv", header = TRUE, sep = ",", dec=".")

Macro_Ptscontacts = Macro_Ptscontacts %>%
  filter(!(LAGUNE %in% c("PAL_VIL_07", "PAL_VIL_06","ORB_MAI_04","ORB_MAI_05")))

#Permanova 

especes = Macro_Ptscontacts[, 4:ncol(Macro_Ptscontacts)]
especes_clean = especes
especes_clean[is.na(especes_clean)] = 0
especes_clean = especes_clean[apply(especes_clean, 1, function(x) sum(x) > 0), ]
rows_to_keep = rownames(especes_clean)
data_clean = Macro_Ptscontacts[rownames(Macro_Ptscontacts) %in% rows_to_keep, ]


dist_mat = vegdist(especes_clean, method = "bray")

permanova_macro_global_result = adonis2(dist_mat ~ annee + Site + LAGUNE, data = data_clean)
print(permanova_macro_global_result)

permanova_macro_details_result = adonis2(dist_mat ~ annee + Site + LAGUNE, data = data_clean, by = "terms", permutations = 999)
print(permanova_macro_details_result)


#Verifier la dispersion 

disp <- betadisper(dist_mat, data_clean$Site)
anova(disp)    # p > 0.05 → OK : ici non OK == signal dû a différence dans  dispersion (variabilité interne) entre sites, pas uniquement à une différence réelle de composition.
plot(disp) 



disp_lagune <- betadisper(dist_mat, data_clean$LAGUNE)
anova(disp_lagune)

disp_annee <- betadisper(dist_mat, data_clean$annee)
anova(disp_annee)




#Clustering 


#TBI ? 